package zeus

import "core:fmt"
import "core:strings"
import "core:os"
import "core:c/libc"

C_ARRAY_STRUCT_NAME :: "ZeusArray___"
C_DYNAMIC_ARRAY_STRUCT_NAME :: "ZeusDynamicArray___"

c_type :: proc(ctx: ^ParseCtx, type: ^Type) -> string {
    switch _ in type {
    case PrimitiveType: 
        it := type.(PrimitiveType)
        switch it {
        case .UNKNOWN: return "<UNKNOWN>"
        case .VOID: return "void"
        case .NUMBER: return "<NUMBER>"
        case .STRING: return "const char *"
        case .CHAR: return "char"
        case .BOOL: return "int" // Use int for bool in C.

        case .F32: return "float"
        case .F64: return "double"
        case .U8: return "unsigned char"
        case .U16: return "unsigned short"
        case .U32: return "unsigned int"
        case .U64: return "unsigned long"
        case .S8: return "signed char"
        case .S16: return "signed short"
        case .S32: return "signed int"
        case .S64: return "signed long"
        case .VARIADIC: return "..."
        }

    case PointerType: return fmt.aprintf("%v*", c_type(ctx, type.(PointerType).inner))
    case ArrayType: return type.(ArrayType).is_dynamic ? C_DYNAMIC_ARRAY_STRUCT_NAME : C_ARRAY_STRUCT_NAME
    case StructType: return fmt.aprintf("struct %v", type.(StructType).ast.name)
    case FunctionType: return "<FUNCTION TYPE>"
    case UnparsedType: return "<UNPARSED TYPE>"
    }
    return "<UNREACHABLE>"
}

CGenCtx :: struct {
    parser: ^ParseCtx,
    indent: int,
    forward_declare: bool,
    is_in_func_args: bool,
}

c_function_gen :: proc(ctx: ^CGenCtx, ast: Ast) -> string {
    it := ast.(^AstProcedure);

    code: strings.Builder

    ctx.is_in_func_args = true
    fmt.sbprint(&code, "(")
    for i := 0; i < len(it.args); i += 1 {
        fmt.sbprint(&code, c_code_gen(ctx, &it.args[i]))
        if i != len(it.args) - 1 {
            fmt.sbprint(&code, ", ")
        }
    }
    fmt.sbprint(&code, ")")
    ctx.is_in_func_args = false

    if it.body != nil && !ctx.forward_declare {
        fmt.sbprint(&code, " ")
        fmt.sbprint(&code, c_code_gen(ctx, it.body))
    } else {
        fmt.sbprint(&code, ";\n")
    }
    return strings.to_string(code)
}

c_array_data_name :: proc(lines: ^strings.Builder, ctx: ^CGenCtx, span: Span) {
    // @Unfinished Should include filename as well 
    fmt.sbprintf(lines, "ARRAY_DATA__%v_%v", span.start, span.end)
}

c_declare_array_data :: proc(lines: ^strings.Builder, ctx: ^CGenCtx, ast: Ast) {
    switch _ in ast {
    case ^AstScope:
        it := ast.(^AstScope);
        for stmt in it.stmts {
            c_declare_array_data(lines, ctx, stmt)
        }

    case ^AstNumberLit:
    case ^AstVariable:
    case ^AstStringLit:
    case ^AstBinOp:
    case ^AstUnaryOp:
    case ^AstDeclaration:
        it := ast.(^AstDeclaration)
        #partial switch _ in it.value {
        case ^AstArrayLit:
            arr := it.value.(^AstArrayLit)
            fmt.sbprintf(lines, "static const %v ", c_type(ctx.parser, arr.type.(ArrayType).inner))
            c_array_data_name(lines, ctx, get_span(it.value)^)
            fmt.sbprint(lines, "[]")

            fmt.sbprint(lines, " = ")

            fmt.sbprint(lines, "{")
            for &val in arr.values {
                fmt.sbprintf(lines, "%v, ", c_code_gen(ctx, val))
            }
            fmt.sbprint(lines, "};\n");
        case ^AstProcedure:
             c_declare_array_data(lines, ctx, it.value)
        }
    case ^AstAssignment:
        it := ast.(^AstAssignment)
        #partial switch _ in it.rhs {
        case ^AstArrayLit:
            fmt.sbprintf(lines, "static const %v ", c_type(ctx.parser, it.rhs.(^AstArrayLit).type.(ArrayType).inner))
        }
    case ^AstProcedureCall:
    case ^AstProcedure:
         c_declare_array_data(lines, ctx, ast.(^AstProcedure).body)
    case ^AstStruct:
    case ^AstIf:
    case ^AstWhile:
    case ^AstReturn:
    case ^AstAccess:
    case ^AstEmitCode:
    case ^AstIndex:
    case ^AstArrayLit:
        fmt.println("TODO: array literals in non declaration or assignments")
    }
}

c_scope_gen :: proc(ctx: ^CGenCtx, ast: Ast) -> string {
    it := ast.(^AstScope);

    old_scope := ctx.parser.current_scope
    ctx.parser.current_scope = it.table

    is_file := it.flags & int(ScopeFlags.FILE) != 0
    lines : strings.Builder
    pad := strings.repeat("    ", ctx.indent)
    defer delete(pad)

    outside_pad := ""
    if ctx.indent > 1 { 
        outside_pad = strings.repeat("    ", ctx.indent - 1) 
    }
    defer delete(outside_pad)

    if is_file { 
        fmt.sbprintln(&lines, "// ----------- PRELUDE -----------")

        fmt.sbprintln(&lines, "typedef struct {")
        fmt.sbprintln(&lines, "    void *items;")
        fmt.sbprintln(&lines, "    long count;")
        fmt.sbprintln(&lines, "}", C_ARRAY_STRUCT_NAME, ";\n")

        fmt.sbprintln(&lines, "typedef struct {")
        fmt.sbprintln(&lines, "    void *items;")
        fmt.sbprintln(&lines, "    long count;")
        fmt.sbprintln(&lines, "    long capacity;")
        fmt.sbprintln(&lines, "} DynamicArray;\n")

        fmt.sbprintln(&lines, "// ----------- FORWARD DECLARATIONS (TYPES) -----------")

        ctx.forward_declare = true;

        for stmt in it.stmts {
            #partial switch _ in stmt {
            case ^AstDeclaration: 
                decl := stmt.(^AstDeclaration)
                #partial switch _ in decl.value {
                case ^AstStruct: 
                    // fmt.sbprintf(&lines, "struct %v;\n", decl.name)
                    fmt.sbprint(&lines, c_declaration_gen(ctx, stmt))
                    fmt.sbprint(&lines, ";\n")
                }
            }
        }

        fmt.sbprintln(&lines, "\n// ----------- FORWARD DECLARATIONS (FUNCTIONS) -----------")

        for stmt in it.stmts {
            #partial switch _ in stmt {
            case ^AstDeclaration: 
                decl := stmt.(^AstDeclaration)
                #partial switch _ in decl.value {
                case ^AstProcedure: 
                    if decl.value.(^AstProcedure).flags & int(ProcFlags.FOREIGN) == 0 {
                        fmt.sbprint(&lines, c_code_gen(ctx, stmt))
                    }
                }
            }
        }
        ctx.forward_declare = false;

        fmt.sbprintln(&lines, "\n// ----------- ARRAY DATA -----------")
        c_declare_array_data(&lines, ctx, it)

        fmt.sbprintln(&lines, "\n// ----------- PROGRAM CODE -----------")
    } else {
        // TODO: there should be some pad here for just scopes that do not belong to something.
        fmt.sbprintln(&lines, "{") 
    }

    ctx.indent += 1
    for stmt in it.stmts {
        fmt.sbprint(&lines, pad)

        needs_semicolon := true
        no_output := false

        #partial switch _ in stmt {
        case ^AstDeclaration: 
            #partial switch _ in stmt.(^AstDeclaration).value {
            case ^AstProcedure: 
                needs_semicolon = false
                if stmt.(^AstDeclaration).value.(^AstProcedure).flags & int(ProcFlags.FOREIGN) != 0 {
                    no_output = true
                }
            }
            case ^AstIf: needs_semicolon = false
            case ^AstWhile: needs_semicolon = false
        case ^AstEmitCode: needs_semicolon = false
        }

        if !no_output {
            fmt.sbprint(&lines, c_code_gen(ctx, stmt))
            if needs_semicolon {
                fmt.sbprintln(&lines, ";")
            } else {
                fmt.sbprintln(&lines)
            }
        }
    }
    ctx.indent -= 1

    if !is_file { 
        fmt.sbprint(&lines, outside_pad)
        fmt.sbprintln(&lines, "}") 
    }

    ctx.parser.current_scope = old_scope
    return strings.to_string(lines)
}

c_proc_call_gen :: proc(ctx: ^CGenCtx, ast: Ast) -> string {
    it := ast.(^AstProcedureCall);

    // @Todo make this use strings.Builder
    code: strings.Builder
    fmt.sbprint(&code, it.procedure)

    fmt.sbprint(&code, "(")
    for i in 0..<len(it.args) {
        fmt.sbprint(&code, c_code_gen(ctx, it.args[i]))
        if i != len(it.args) - 1 {
            fmt.sbprint(&code, ", ")
        }
    }
    fmt.sbprint(&code, ")")

    return strings.to_string(code)
}

c_unary_op_gen :: proc(ctx: ^CGenCtx, ast: Ast) -> string {
    it := ast.(^AstUnaryOp);
    if it.op == .ADDRESS {
        return fmt.aprintf("(&%v)", c_code_gen(ctx, it.inner));
    } else {
        return fmt.aprintf("(%v%v)", op_to_string(it.op), c_code_gen(ctx, it.inner));
    }
}

c_declaration_gen :: proc(ctx: ^CGenCtx, ast: Ast) -> string {
    it := ast.(^AstDeclaration);
    if it.value == nil {
        if ctx.is_in_func_args {
            return fmt.aprintf("%v %v", c_type(ctx.parser, &it.type), it.name)
        } else {
            return fmt.aprintf("%v %v = {{0}}", c_type(ctx.parser, &it.type), it.name)
        }
    } else {
        #partial switch _ in it.value {
        case ^AstProcedure: 
            ret := &it.value.(^AstProcedure).return_type
            return fmt.aprintf("%v %v%v", c_type(ctx.parser, ret), it.name, c_code_gen(ctx, it.value))
        case ^AstStruct: 
            if ctx.forward_declare {
                return fmt.aprintf("struct %v %v", it.name, c_code_gen(ctx, it.value))
            } else {
                return ""
            }
        }
        return fmt.aprintf("%v %v = %v", c_type(ctx.parser, &it.type), it.name, c_code_gen(ctx, it.value))
    }
}

c_code_gen :: proc(ctx: ^CGenCtx, ast: Ast) -> string {
    switch _ in ast {
        case ^AstBinOp:
            it := ast.(^AstBinOp);
            return fmt.aprintf("(%v %v %v)", c_code_gen(ctx, it.lhs), op_to_string(it.op), c_code_gen(ctx, it.rhs))
        case ^AstAccess:
            it := ast.(^AstAccess);

            t := get_type_of(ctx.parser, it.node)
            // defer free_type(&t) @Leak
            #partial switch _ in t {
            case PointerType: return fmt.aprintf("%v->%v", c_code_gen(ctx, it.node), it.value)
            }
            // @Unfinished @Todo check if the node is a pointer and if so, deref it.
            return fmt.aprintf("%v.%v", c_code_gen(ctx, it.node), it.value)
        case ^AstAssignment:
            it := ast.(^AstAssignment);
            return fmt.aprintf("%v %v %v", c_code_gen(ctx, it.lhs), op_to_string(it.op), c_code_gen(ctx, it.rhs))
        case ^AstUnaryOp:
            return c_unary_op_gen(ctx, ast)
        case ^AstIndex:
            it := ast.(^AstIndex);
            inner_type := get_type_of(ctx.parser, it.array).(ArrayType).inner
            return fmt.aprintf("(((%v*) %v.items)[%v])", c_type(ctx.parser, inner_type), c_code_gen(ctx, it.array), c_code_gen(ctx, it.index))
        case ^AstVariable: 
            it := ast.(^AstVariable)
            return it.value
        case ^AstNumberLit: 
            it := ast.(^AstNumberLit)
            return it.value
        case ^AstStringLit:
            it := ast.(^AstStringLit)
            return fmt.aprintf("\"%v\"", it.value)
        case ^AstArrayLit: 
            it := ast.(^AstArrayLit)
            code: strings.Builder
            fmt.sbprintf(&code, "(%v){{", C_ARRAY_STRUCT_NAME);
            fmt.sbprint(&code, ".items = (void*)");
            c_array_data_name(&code, ctx, it.span);
            fmt.sbprint(&code, ", ");

            fmt.sbprintf(&code, ".count = %v", len(it.values));

            fmt.sbprint(&code, "}");
            return strings.to_string(code)

        case ^AstDeclaration:
            return c_declaration_gen(ctx, ast)
        case ^AstProcedureCall:
            return c_proc_call_gen(ctx, ast)
        case ^AstScope:
            return c_scope_gen(ctx, ast)
        case ^AstProcedure:
            return c_function_gen(ctx, ast)
        case ^AstStruct:
            it := ast.(^AstStruct);
            code: strings.Builder
            fmt.sbprintln(&code, "{")
            for &field in it.fields {
                t := field.type
                fmt.sbprintln(&code, c_type(ctx.parser, &t), field.name, ";")
            }
            fmt.sbprint(&code, "}");
            return strings.to_string(code)
        case ^AstIf: 
            it := ast.(^AstIf);
            return fmt.aprintf("if (%v) %v", c_code_gen(ctx, it.predicate), c_code_gen(ctx, it.body))
        case ^AstWhile: 
            it := ast.(^AstWhile);
            return fmt.aprintf("while (%v) %v", c_code_gen(ctx, it.predicate), c_code_gen(ctx, it.body))
        case ^AstReturn: 
            return fmt.aprintf("return %v", c_code_gen(ctx, ast.(^AstReturn).value))

        case ^AstEmitCode: 
            return ast.(^AstEmitCode).code
    }
    return "<TODO>"
}

