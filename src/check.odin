package zeus

import "core:fmt"
import "core:strings"
import "core:os"
import "core:c/libc"

SymbolKind :: enum {
    PROCEDURE,
    VARIABLE,
    TYPE,
}

PrimitiveType :: enum {
    UNKNOWN, // Not a real type. Just used for declarations that have not been inferred yet.
    VOID,
    NUMBER, // Can coerce to any number type.
    BOOL,
    STRING,
    CHAR,
    VARIADIC,

    F32,
    F64,
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
}

FunctionType :: struct {
    ast: ^AstProcedure,
}

PointerType :: struct {
    inner: ^Type,
}

ArrayType :: struct {
    inner: ^Type,
    is_dynamic: bool,
}

StructType :: struct {
    ast: ^AstStruct,
}

UnparsedType :: struct {
    ast: Ast,
}

Type :: union {
    UnparsedType,
    PrimitiveType,
    PointerType,
    ArrayType,
    StructType,
    FunctionType,
}

Symbol :: struct {
    ast: ^AstDeclaration,

    // what number it was defined. So the first variable of a scope would be 0 and the second would be 1.
    // Used to check ordering of variables used.
    n: int
}

SymbolTable :: struct {
    symbols: map[string] Symbol,
    parent: int,
    n: int,
}

symbol_table_insert :: proc(ctx: ^ParseCtx, ast: ^AstDeclaration) {
    table := get_current_scope(ctx)
    if ast.name in table.symbols {
        compiler_error(ctx.file, get_span(ast)^, "Trying to redefine symbol of name '%v'", ast.name)
    } else {
        table.symbols[ast.name] = Symbol{ast, table.n}
        table.n += 1
    }
}

symbol_table_lookup_from :: proc(from: int, ctx: ^ParseCtx, name: string) -> (^Symbol, int) {
    current := ctx.current_scope
    for current != -1 {
        if name in ctx.scopes[current].symbols {
            return &ctx.scopes[current].symbols[name], current
        }
        current = ctx.scopes[current].parent
    }
    return nil, -1 // Not found
}

symbol_table_lookup :: proc(ctx: ^ParseCtx, name: string) -> (^Symbol, int) {
    return symbol_table_lookup_from(ctx.current_scope, ctx, name)
}

free_type :: proc(t: ^Type) {
    #partial switch _ in t {
    case PointerType: 
        free_type(t.(PointerType).inner)
        free(t)
    }
}

get_type_of :: proc(ctx: ^ParseCtx, ast: Ast) -> Type {
    switch _ in ast {
        case ^AstBinOp:
            it := ast.(^AstBinOp);
            lhs_t := get_type_of(ctx, it.lhs)
            if is_compare_op(it.op) {
                return PrimitiveType.BOOL
            }
            // rhs_t := get_type_of(ctx, it.rhs)
            // if lhs_t != rhs_t {
            //     // @ErrorMessages "trying to do sus things with binary operators"
            // }
            return lhs_t

        case ^AstAssignment: return PrimitiveType.VOID

        case ^AstUnaryOp:
            it := ast.(^AstUnaryOp)
            if it.op == .ADDRESS {
                inner := new(Type)
                inner^ = get_type_of(ctx, it.inner)
                return PointerType{inner}
            } else if it.op == .MUL {
                t := get_type_of(ctx, it.inner)
                return t.(PointerType).inner^
            } else {
                return get_type_of(ctx, it.inner)
            }

        case ^AstIndex:
            it := ast.(^AstIndex)

            array_type := get_type_of(ctx, it.array)
            #partial switch _ in array_type {
            case ArrayType:
                return array_type.(ArrayType).inner^
            }
            compiler_error(ctx.file, get_span(it)^, "Trying to index on non-array type. Indexing on pointers is not supported yet. Sorry!")
            return PrimitiveType.UNKNOWN
            
        case ^AstVariable: 
            it := ast.(^AstVariable)
            decl, _ := symbol_table_lookup(ctx, it.value)
            if decl == nil { return PrimitiveType.UNKNOWN }
            if decl.ast.type == PrimitiveType.UNKNOWN {
                decl.ast.type = get_type_of(ctx, decl.ast.value)
            }
            return decl.ast.type;

        case ^AstNumberLit: 
            return PrimitiveType.NUMBER

        case ^AstStringLit: 
            return PrimitiveType.STRING

        case ^AstArrayLit: 
            return ast.(^AstArrayLit).type

        case ^AstDeclaration: 
            return PrimitiveType.VOID

        case ^AstProcedureCall:
            it := ast.(^AstProcedureCall);
            decl, _ := symbol_table_lookup(ctx, it.procedure)
            #partial switch _ in decl.ast.value {
            case ^AstProcedure: return decl.ast.value.(^AstProcedure).return_type
            }
            // @ErrorMessages give some error here. "trying to call non-function symbol"
            return PrimitiveType.UNKNOWN
            
        case ^AstScope: return PrimitiveType.VOID
        case ^AstProcedure: 
            it := ast.(^AstProcedure);
            return FunctionType{it}

        case ^AstStruct: 
            it := ast.(^AstStruct);
            return StructType{it}

        case ^AstAccess: 
            it := ast.(^AstAccess);
            outer := get_type_of(ctx, it.node)

            structure: StructType
            ok := false
            #partial switch _ in outer {
            case StructType: 
                structure = outer.(StructType)
                ok = true
            case PointerType: 
                structure = outer.(PointerType).inner.(StructType)
                ok = true
            case ArrayType:
                if it.value == "count" {
                    return PrimitiveType.S64;
                }
                compiler_error(ctx.file, it.span, "Arrays do not have property '%v'.", it.value)
            }
            if !ok {
                compiler_error(ctx.file, it.span, "Can not access on non struct type.")
                return PrimitiveType.UNKNOWN;
            }
            for field in structure.ast.fields {
                if field.name == it.value {
                    return field.type
                }
            }
            compiler_error(ctx.file, it.span, "Could not find field '%v' in variable.", it.value)

        case ^AstIf: return PrimitiveType.VOID
        case ^AstWhile: return PrimitiveType.VOID
        case ^AstReturn: return PrimitiveType.VOID
        case ^AstEmitCode: return PrimitiveType.VOID
    }
    return PrimitiveType.VOID
}

should_default_to_float :: proc(ast: Ast) -> bool {
    #partial switch _ in ast {
    case ^AstNumberLit:
        return strings.contains_rune(ast.(^AstNumberLit).value, '.')
    case ^AstBinOp:
        it := ast.(^AstBinOp)
        return should_default_to_float(it.lhs) || should_default_to_float(it.rhs)
    }
    // @Unfinished does this seem right?
    return false
}

is_number_like :: proc(t: Type) -> bool {
    #partial switch _ in t {
    case PrimitiveType:
        #partial switch t.(PrimitiveType) {
        case .NUMBER: return true
		case .F32: return true
		case .F64: return true
		case .U8: return true
		case .U16: return true
		case .U32: return true
		case .U64: return true
		case .S8: return true
		case .S16: return true
		case .S32: return true
		case .S64: return true
        }
    }
    return false
}

compare_types :: proc(a: Type, b: Type) -> bool {
    if a == PrimitiveType.NUMBER {
        return is_number_like(b)
    } else if b == PrimitiveType.NUMBER {
        return is_number_like(a)
    }
    #partial switch _ in a {
    case PointerType: 
        #partial switch _ in b {
        case PointerType: 
            return compare_types(a.(PointerType).inner^, b.(PointerType).inner^)
        }
        return false
    case ArrayType:
        #partial switch _ in b {
        case ArrayType: 
            a_arr := a.(ArrayType)
            b_arr := b.(ArrayType)
            return a_arr.is_dynamic == b_arr.is_dynamic && compare_types(a_arr.inner^, b_arr.inner^)
        }
        return false
    }
    return a == b
}

is_integer :: proc(t: ^Type) -> bool {
    ok := false
    #partial switch _ in t {
    case PrimitiveType: ok = true
    }
    if !ok { return false }

    p := t.(PrimitiveType)
    if p == PrimitiveType.S8  { return true }
    if p == PrimitiveType.S16 { return true }
    if p == PrimitiveType.S32 { return true }
    if p == PrimitiveType.S64 { return true }
    if p == PrimitiveType.U8  { return true }
    if p == PrimitiveType.U16 { return true }
    if p == PrimitiveType.U32 { return true }
    if p == PrimitiveType.U64 { return true }
    return false
}

check_bin_op :: proc(ctx: ^ParseCtx, it: ^AstBinOp) {
    semantic_analize(ctx, it.lhs)
    semantic_analize(ctx, it.rhs)
    lhs_type := get_type_of(ctx, it.lhs)
    defer free_type(&lhs_type)
    rhs_type := get_type_of(ctx, it.rhs)
    defer free_type(&rhs_type)

    if !compare_types(lhs_type, rhs_type) {
        msg := "Invalid operands to binary %v types %v and %v. Operator overloading is currently not supported. Sorry!"
        compiler_error(ctx.file, it.span, msg, op_to_string(it.op), type_to_string(lhs_type), type_to_string(rhs_type))
        return
    }

    if !is_number_like(lhs_type) {
        msg := "Can not '%v' type %v. You can only do math on numbers as of now. Operator overloading is currently not supported. Sorry!"
        compiler_error(ctx.file, it.span, msg, op_to_string(it.op), type_to_string(lhs_type))
        return
    }
}

// Infers types and type checks. @TODO: Should compute size of structs.
semantic_analize :: proc(ctx: ^ParseCtx, ast: Ast) {
    switch _ in ast {
        case ^AstBinOp:
            it := ast.(^AstBinOp);
            check_bin_op(ctx, it)

        case ^AstIndex:
            it := ast.(^AstIndex);
            semantic_analize(ctx, it.array)
            semantic_analize(ctx, it.index)
            array_type := get_type_of(ctx, it.array)
            index_type := get_type_of(ctx, it.index)

            array_ok := false
            #partial switch _ in array_type {
            case ArrayType:
                array_ok = true
            }
            if !array_ok {
                compiler_error(ctx.file, get_span(it)^, "Trying to index on non-array type. Indexing on pointers is not supported yet. Sorry!")
            }

            if !is_integer(&index_type) {
                compiler_error(ctx.file, get_span(it.index)^, "Trying to index with non-integer value.")
            }

        case ^AstAssignment:
            it := ast.(^AstAssignment);
            semantic_analize(ctx, it.lhs)
            semantic_analize(ctx, it.rhs)
            lhs_type := get_type_of(ctx, it.lhs)
            defer free_type(&lhs_type)
            rhs_type := get_type_of(ctx, it.rhs)
            defer free_type(&rhs_type)
            if !compare_types(lhs_type, rhs_type) {
                msg := "Trying to assign value of type %v to variable of type %v"
                compiler_error(ctx.file, it.span, msg, type_to_string(rhs_type), type_to_string(lhs_type))
                return
            }
            if it.op != .ASSIGN && !is_number_like(lhs_type) {
                msg := "Can not '%v' %v to %v. You can only do math on numbers as of now. Operator overloading is currently not supported. Sorry!"
                compiler_error(ctx.file, it.span, msg, op_to_string(it.op), type_to_string(rhs_type), type_to_string(lhs_type))
                return
            }
            // if decl.ast.flags & int(DeclFlags.IMMUTABLE) != 0 || decl.ast.flags & int(DeclFlags.CONSTANT) != 0 {
            //     msg := "Can not modify immutable variable '%v'"
            //     compiler_error(ctx.file, it.span, msg, decl.ast.name)
            //     return
            // }

        case ^AstUnaryOp:
            it := ast.(^AstUnaryOp);
            semantic_analize(ctx, it.inner)
            
        case ^AstVariable: 
            it := ast.(^AstVariable)
            decl, scope := symbol_table_lookup(ctx, it.value)
            if decl == nil || (scope == ctx.current_scope && decl.n > it.n) {
                compiler_error(ctx.file, get_span(it)^, "Use of undeclared variable '%v'", it.value)
            }

        case ^AstNumberLit: 
        case ^AstStringLit:

        case ^AstArrayLit:
            it := ast.(^AstArrayLit);
            inner_type := it.type.(ArrayType).inner^
            for val in it.values {
                t := get_type_of(ctx, val);
                if !compare_types(t, inner_type) {
                    compiler_error(ctx.file, get_span(val)^, "Found value of type %v in array of %v", 
                        type_to_string(t), type_to_string(it.type.(ArrayType).inner^))
                }
            }
            
        case ^AstDeclaration:
            it := ast.(^AstDeclaration);
            #partial switch _ in it.type {
            case UnparsedType: it.type = StructType{it.type.(UnparsedType).ast.(^AstDeclaration).value.(^AstStruct)} // @Hack @Unfinished Properly check what type it is and use that.
            }
            if it.value == nil {
                if it.type == PrimitiveType.UNKNOWN {
                    compiler_error(ctx.file, it.span, "Declaration without value needs a type.")
                }
            } else {
                semantic_analize(ctx, it.value)
                type := get_type_of(ctx, it.value)
                defer free_type(&type)
                if it.type == PrimitiveType.UNKNOWN {
                    it.type = type
                    if it.type == .NUMBER {
                        if should_default_to_float(it.value) {
                            it.type = .F64
                        } else {
                            it.type = .S64
                        }
                    }
                } else {
                    if !compare_types(it.type, type) {
                        compiler_error(ctx.file, get_span(it.value)^, 
                            "Value is expected to have type %v but is %v", type_to_string(it.type), type_to_string(type))
                    }
                }
            }

        case ^AstProcedureCall:
            it := ast.(^AstProcedureCall);
            decl, _ := symbol_table_lookup(ctx, it.procedure)
            if decl == nil {
                compiler_error(ctx.file, get_span(it)^, "Use of undeclared function '%v'.", it.procedure)
            }

            is_function := false
            #partial switch _ in decl.ast.value {
            case ^AstProcedure: is_function = true
            }

            if !is_function {
                compiler_error(ctx.file, get_span(it)^, "Trying to call the symbol '%v' that is not a function.", it.procedure)
                break
            }

            types: [dynamic] Type
            defer delete(types)

            for &arg in it.args {
                semantic_analize(ctx, arg)
                append(&types, get_type_of(ctx, arg))
            }

            fn := decl.ast.value.(^AstProcedure)

            if fn.flags & int(ProcFlags.VARIADIC) != 0 {
                if len(types) < len(fn.args) {
                    msg := "Variadic function '%v' requires at least %v arguments but supplied with %v."
                    compiler_error(ctx.file, get_span(it)^, msg, it.procedure, len(fn.args), len(types))
                    break
                }
            } else {
                if len(types) != len(fn.args) {
                    msg: string
                    if len(fn.args) == 0 {
                        msg = "Function '%v' requires no arguments but supplied with %v."
                        compiler_error(ctx.file, get_span(it)^, msg, it.procedure, len(types))
                    } else if len(fn.args) == 1 {
                        msg = "Function '%v' requires one argument but supplied with %v."
                        compiler_error(ctx.file, get_span(it)^, msg, it.procedure, len(types))
                    } else {
                        msg = "Function '%v' requires %v arguments but supplied with %v."
                        compiler_error(ctx.file, get_span(it)^, msg, it.procedure, len(fn.args), len(types))
                    }
                    break
                }
            }

            for i := 0; i < len(fn.args); i += 1 {
                if !compare_types(types[i], fn.args[i].type) {
                    msg := "Expected argument to be of type %v but is of type %v"
                    compiler_error(ctx.file, get_span(it.args[i])^, msg, type_to_string(fn.args[i].type), type_to_string(types[i]))
                }
            }
            
        case ^AstScope:
            it := ast.(^AstScope);

            ctx.current_scope = it.table
            for &stmt in it.stmts {
                semantic_analize(ctx, stmt)
            }
            ctx.current_scope = get_current_scope(ctx).parent
            
        case ^AstProcedure:
            it := ast.(^AstProcedure);
            semantic_analize(ctx, it.body)

        case ^AstStruct:
            it := ast.(^AstStruct);
            for &field in it.fields {
                #partial switch _ in field.type {
                case UnparsedType: field.type = StructType{field.type.(UnparsedType).ast.(^AstDeclaration).value.(^AstStruct)} // @Hack @Unfinished Properly check what type it is and use that.
                case PrimitiveType: 
                    if field.type == PrimitiveType.UNKNOWN {
                        fmt.println("TODO: infer struct fields.")
                    }
                }

                #partial switch _ in field.type {
                case StructType:
                    if field.type.(StructType).ast == it {
                        compiler_error(ctx.file, field.span, "Cylic dependancy found. Would result in infinite size.")
                    }
                }
                // @Unfinished @Todo check the default value that it's the same type as the field type + infer if unspecified
            }
            // @Unfinished @Todo Sizing for structs

        case ^AstAccess:
            it := ast.(^AstAccess);
            semantic_analize(ctx, it.node)
            // @Unfinished @Todo check that the thing has the field

        case ^AstIf: 
            it := ast.(^AstIf);
            semantic_analize(ctx, it.predicate)
            pred_type := get_type_of(ctx, it.predicate)
            if !compare_types(pred_type, PrimitiveType.BOOL) {
                msg := "Expected predicate to if statement to be boolean but is of type %v"
                compiler_error(ctx.file, get_span(it.predicate)^, msg, type_to_string(pred_type))
            }
            semantic_analize(ctx, it.body)

        case ^AstWhile: 
            it := ast.(^AstWhile);
            semantic_analize(ctx, it.predicate)
            pred_type := get_type_of(ctx, it.predicate)
            if !compare_types(pred_type, PrimitiveType.BOOL) {
                msg := "Expected predicate to while statement to be boolean but is of type %v"
                compiler_error(ctx.file, get_span(it.predicate)^, msg, type_to_string(pred_type))
            }
            semantic_analize(ctx, it.body)

        case ^AstReturn: 
            it := ast.(^AstReturn);
            semantic_analize(ctx, it.value)

        case ^AstEmitCode: 
    }
}

