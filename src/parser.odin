package zeus

import "core:fmt"
import "core:strings"
import "core:os"
import "core:c/libc"

AstVariable :: struct {
    value: string,
    span: Span,
    n: int,
}

AstNumberLit :: struct {
    value: string,
    span: Span,
}

AstStringLit :: struct {
    value: string,
    span: Span,
}

AstArrayLit :: struct {
    values: [dynamic] Ast,
    type: Type,
    span: Span,
}

AstBinOp :: struct {
    lhs: Ast,
    op: TokenType,
    rhs: Ast,
    span: Span,
}

AstUnaryOp :: struct {
    op: TokenType,
    inner: Ast,
    span: Span,
}

DeclFlags :: enum {
    CONSTANT = 1 << 0,
    IMMUTABLE = 1 << 1,
}

AstDeclaration :: struct {
    name: string,
    type: Type,
    value: Ast,
    flags: int, // DeclFlags
    span: Span,
}

AstAssignment :: struct {
    lhs: Ast,
    op: TokenType,
    rhs: Ast,
    span: Span,
}

ScopeFlags :: enum {
    FILE = 1 << 0, // Is an entire file.
}

AstScope :: struct {
    stmts: [dynamic] Ast,
    table: int,
    flags: int,
    span: Span,
}

// TODO: Replace 'procedure' with just another ast node 
// to eventually allow for namespaces and function pointers
AstProcedureCall :: struct {
    procedure: string,
    args: [dynamic] Ast,
    span: Span,
}

ProcFlags :: enum {
    FOREIGN = 1 << 0,
    VARIADIC = 1 << 1,
}

AstProcedure :: struct {
    args: [dynamic] AstDeclaration,
    body: Ast,
    return_type: Type,
    flags: int,
    span: Span,
}

StructField :: struct {
    name: string,
    type: Type,
    default_value: Ast,
    span: Span,
}

AstStruct :: struct {
    fields: [dynamic] StructField,
    name: string,
    flags: int,
    size: int,
    span: Span,
}

AstIf :: struct {
    predicate: Ast,
    body: Ast,
    span: Span,
}

AstWhile :: struct {
    predicate: Ast,
    body: Ast,
    span: Span,
}

AstReturn :: struct {
    value: Ast,
    span: Span,
}

AstAccess :: struct {
    node: Ast,
    value: string,
    span: Span,
}

AstIndex :: struct {
    array: Ast,
    index: Ast,
    span: Span,
}

// @Temporary
AstEmitCode :: struct {
    code: string,
    span: Span,
}

Ast :: union {
    ^AstNumberLit,
    ^AstVariable,
    ^AstStringLit,
    ^AstArrayLit,
    ^AstBinOp,
    ^AstUnaryOp,
    ^AstDeclaration,
    ^AstAssignment,
    ^AstProcedureCall,
    ^AstScope,
    ^AstProcedure,
    ^AstStruct,
    ^AstIf,
    ^AstWhile,
    ^AstReturn,
    ^AstAccess,
    ^AstIndex,

    ^AstEmitCode,
}

ParseCtx :: struct {
    tokens: [] Token,
    file: ^FileCtx,
    current_scope: int,
    scopes: [dynamic] SymbolTable
}

get_current_scope :: proc(ctx: ^ParseCtx) -> ^SymbolTable {
    return &ctx.scopes[ctx.current_scope];
}

advance_token :: proc(ctx: ^ParseCtx) {
    ctx.tokens = ctx.tokens[1:]
}

parser_error_at_next :: proc(ctx: ^ParseCtx, format: string, args: ..any) {
    span : Span
    if len(ctx.tokens) == 0 {
        span = Span{len(ctx.file.input) - 1, len(ctx.file.input)}
    } else {
        span = span_from_token(&ctx.tokens[0])
    }

    compiler_error(ctx.file, span, format, ..args)
}

span_from_token :: proc(token: ^Token) -> Span {
    res := Span{token.offset, token.offset + len(token.value)}
    if token.type == .STRING_LIT {
        res.start -= 1
        res.end += 2 // Shouldn't this be +1 ??
    }
    return res
}

span_stretch :: proc(from: ^Span, to: ^Span) -> Span {
    return Span{from.start, to.end}
}

is_assign_op :: proc(op: TokenType) -> bool {
    #partial switch op {
    case .ASSIGN: return true
    case .ADD_ASSIGN: return true
    case .SUB_ASSIGN: return true
    case .MUL_ASSIGN: return true
    case .DIV_ASSIGN: return true
    case .MOD_ASSIGN: return true
    }
    return false
}

is_compare_op :: proc(op: TokenType) -> bool {
    #partial switch op {
    case .EQUALITY: return true
    case .INEQUALITY: return true
    case .GT: return true
    case .GE: return true
    case .LT: return true
    case .LE: return true
    }
    return false
}

is_unary_op :: proc(t: TokenType) -> bool {
    #partial switch t {
    case .SUB: return true
    case .MUL: return true
    case .ADDRESS: return true
    }
    return false
}

parse_primary :: proc(ctx: ^ParseCtx) -> Ast {
    tokens := &ctx.tokens

    if is_unary_op(tokens[0].type) {
        op := tokens[0].type
        start := span_from_token(&tokens[0])
        advance_token(ctx)
        inner := parse_primary(ctx)

        it := new(AstUnaryOp)
        it^ = AstUnaryOp{op, inner, span_stretch(&start, get_span(inner))}
        return it
    }
    #partial switch tokens[0].type {
    case .NUMBER_LIT:
        it := new(AstNumberLit)
        it^ = AstNumberLit{tokens[0].value, span_from_token(&tokens[0])}
        advance_token(ctx)
        return it
    case .STRING_LIT:
        it := new(AstStringLit)
        it^ = AstStringLit{tokens[0].value, span_from_token(&tokens[0])}
        advance_token(ctx)
        return it
    case .OPEN_BRACKET:
        start := span_from_token(&tokens[0])
        end := start

        advance_token(ctx)

        is_dynamic := false
        if expect_token(ctx, .DOT_DOT) != nil {
            is_dynamic = true
        }
        if expect_token(ctx, .CLOSE_BRACKET) == nil {
            parser_error_at_next(ctx, "Expected ']' since fixed size arrays are not yet supported. Sorry!")
            return nil
        }

        inner_type := new(Type)
        inner_type^ = parse_type(ctx)
        if expect_token(ctx, .OPEN_BRACE) == nil {
            parser_error_at_next(ctx, "Expected '{{' to start array literal.")
            return nil
        }

        values: [dynamic] Ast
        if expect_token(ctx, .CLOSE_BRACE) == nil {
            for true {
                append(&values, parse_expr(ctx))
                if expect_token(ctx, .CLOSE_BRACE) != nil {
                    end = span_from_token(&tokens[0])
                    break
                }
                if expect_token(ctx, .COMMA) == nil {
                    parser_error_at_next(ctx, "Expected either ',' or '}' after value in array literal.")
                    break
                }
            }
        }

        type := ArrayType{inner_type, is_dynamic}

        it := new(AstArrayLit)
        it^ = AstArrayLit{values, type, span_stretch(&start, &end)}

        return it

    case .IDENT:
        name := &tokens[0]
        advance_token(ctx)

        args : [dynamic] Ast
        if expect_token(ctx, .OPEN_PAREN) != nil {
            start := span_from_token(name)
            end : Span
            if expect_token(ctx, .CLOSE_PAREN) == nil {
                for true {
                    append(&args, parse_expr(ctx))
                    if expect_token(ctx, .CLOSE_PAREN) != nil {
                        end = span_from_token(&tokens[0])
                        break
                    }
                    if expect_token(ctx, .COMMA) == nil {
                        parser_error_at_next(ctx, "Expected either ',' or ')' after parameter in procedure call.")
                        break
                    }
                }
            }
            it := new(AstProcedureCall)
            it^ = AstProcedureCall{name.value, args, span_stretch(&start, &end)}
            return it
        } else {
            it := new(AstVariable)
            it^ = AstVariable{name.value, span_from_token(name), get_current_scope(ctx).n}
            get_current_scope(ctx).n += 1
            return it
        }
    case .OPEN_PAREN:
        advance_token(ctx)
        res := parse_expr(ctx)
        if expect_token(ctx, .CLOSE_PAREN) == nil {
            parser_error_at_next(ctx, "Expected ')'");
        }
        return res
    }
    parser_error_at_next(ctx, "Unexpected token '%v' found while parsing primary expression.", tokens[0].value) // TODO: change this to compiler_error
    advance_token(ctx)
    return nil;
}

// Below 1 is only used in statements.
// Below 0 is seen as not a binary operator.
token_precedence :: proc(token: TokenType) -> int {
    if is_assign_op(token) || token == .COLON { return 0 }
    if is_compare_op(token) { return 1 }

    #partial switch token {
    case .ADD: return 2
    case .SUB: return 2

    case .MUL: return 3
    case .DIV: return 3
    case .MOD: return 3 // Same as in C for now but maybe this is not the right precedence.

    case .DOT: return 4
    case .OPEN_BRACKET: return 4
    }
    return -1 
}

get_span :: proc(ast: Ast) -> ^Span {
    switch _ in ast {
        case ^AstBinOp: return &ast.(^AstBinOp).span
        case ^AstAssignment: return &ast.(^AstAssignment).span
        case ^AstUnaryOp: return &ast.(^AstUnaryOp).span
        case ^AstVariable: return &ast.(^AstVariable).span 
        case ^AstNumberLit: return &ast.(^AstNumberLit).span 
        case ^AstStringLit: return &ast.(^AstStringLit).span
        case ^AstArrayLit: return &ast.(^AstArrayLit).span
        case ^AstDeclaration: return &ast.(^AstDeclaration).span
        case ^AstProcedureCall: return &ast.(^AstProcedureCall).span
        case ^AstScope: return &ast.(^AstScope).span
        case ^AstProcedure: return &ast.(^AstProcedure).span
        case ^AstStruct: return &ast.(^AstStruct).span
        case ^AstIf: return &ast.(^AstIf).span
        case ^AstWhile: return &ast.(^AstWhile).span
        case ^AstReturn: return &ast.(^AstReturn).span
        case ^AstAccess: return &ast.(^AstAccess).span
        case ^AstIndex: return &ast.(^AstIndex).span

        case ^AstEmitCode: return &ast.(^AstEmitCode).span
    }
    return nil
}

make_bin_op :: proc(lhs: Ast, op: TokenType, rhs: Ast) -> Ast {
    it := new(AstBinOp)
    it^ = AstBinOp{lhs, op, rhs, span_stretch(get_span(lhs), get_span(rhs))}
    return it
}

parse_declaration :: proc(ctx: ^ParseCtx, lhs: Ast) -> Ast {
    is_name := false
    #partial switch _ in lhs {
    case ^AstVariable: is_name = true
    }

    it := new(AstDeclaration)
    it.name = lhs.(^AstVariable).value
    symbol_table_insert(ctx, it)

    if !is_name {
        parser_error_at_next(ctx, "Expected variable name before ':'")
        return nil
    }

    type : Type = PrimitiveType.UNKNOWN
    if len(ctx.tokens) != 0 && !(ctx.tokens[0].type == .ASSIGN || ctx.tokens[0].type == .COLON) {
        type = parse_type(ctx)
    }

    flags: int = 0
    value: Ast = nil
    end: Span
    
    if len(ctx.tokens) == 0 {
        parser_error_at_next(ctx, "Unexpected end of stream found while parsing assignment.")
        return nil 
    }
    if ctx.tokens[0].type == .COLON {
        advance_token(ctx)
        flags |= int(DeclFlags.CONSTANT)
        value = parse_expr(ctx)
        end = get_span(value)^
    } else if ctx.tokens[0].type == .ASSIGN { 
        advance_token(ctx)
        value = parse_expr(ctx)
        end = get_span(value)^
    } else if ctx.tokens[0].type == .SEMI_COLON {
        end = span_from_token(&ctx.tokens[0])
    } else {
        parser_error_at_next(ctx, "Expected ':=' or '::' for declarations.")
        return nil 
    }

    if value != nil {
        #partial switch _ in value {
        case ^AstStruct: value.(^AstStruct).name = lhs.(^AstVariable).value
        }
    }
    it^ = AstDeclaration{lhs.(^AstVariable).value, type, value, flags, span_stretch(get_span(lhs), &end)}

    return it
}

parse_assignment :: proc(ctx: ^ParseCtx, lhs: Ast, op: ^Token) -> Ast {
    value := parse_expr(ctx)
    it := new(AstAssignment)

    span := span_stretch(get_span(lhs), get_span(value))
    
    it^ = AstAssignment{lhs, op.type, value, span}
    return it
}

parse_expr_1 :: proc(ctx: ^ParseCtx, _lhs: Ast, min_precedence: int) -> Ast {
    tokens := &ctx.tokens
    lhs := _lhs 
    for len(tokens) > 1 && token_precedence(tokens[0].type) >= min_precedence {
        op := &tokens[0]
        advance_token(ctx)

        if op.type == .COLON {
            return parse_declaration(ctx, lhs)
        } else if is_assign_op(op.type) {
            return parse_assignment(ctx, lhs, op)
        } else if op.type == .DOT {
            value := expect_token(ctx, .IDENT)
            if value == nil {
                parser_error_at_next(ctx, "Expected identifier after '.'")
                return nil
            }
            end_span := span_from_token(value)
            it := new(AstAccess)
            it^ = AstAccess{lhs, value.value, span_stretch(get_span(lhs), &end_span)}
            lhs = it
            continue
        } else if op.type == .OPEN_BRACKET {
            index := parse_expr(ctx)

            end_bracket := expect_token(ctx, .CLOSE_BRACKET)
            if end_bracket == nil {
                parser_error_at_next(ctx, "Expected ']'.")
                return nil
            }
            end_span := span_from_token(end_bracket)

            it := new(AstIndex)
            it^ = AstIndex{lhs, index, span_stretch(get_span(lhs), &end_span)}
            lhs = it
            continue
        }

        rhs := parse_primary(ctx)

        for len(tokens) > 1 && token_precedence(tokens[0].type) > token_precedence(op.type) {
            rhs = parse_expr_1(ctx, rhs, token_precedence(op.type) + 1)
        }
        lhs = make_bin_op(lhs, op.type, rhs)
    }
    return lhs
}

parse_function :: proc(ctx: ^ParseCtx) -> Ast {
    start := span_from_token(&ctx.tokens[0])    
    advance_token(ctx)
    
    if expect_token(ctx, .OPEN_PAREN) == nil {
        parser_error_at_next(ctx, "Expected '(' after 'fn'.")
        return nil
    }

    flags := 0
    args : [dynamic] AstDeclaration;
    for len(ctx.tokens) != 0 && ctx.tokens[0].type != .CLOSE_PAREN {
        name := &ctx.tokens[0]
        start := span_from_token(name)
        if name.type != .IDENT {
            parser_error_at_next(ctx, "Expected argument name but found '%v'.", name.value)
            return nil
        }
        advance_token(ctx)

        if expect_token(ctx, .COLON) == nil {
            parser_error_at_next(ctx, "Expected ':' after argument name.")
            return nil
        }

        type_span: Span
        type := parse_type(ctx, &type_span)
        if (type == PrimitiveType.VARIADIC) {
            flags |= int(ProcFlags.VARIADIC)
            // @ErrorMessages Give an error if there are more arguments "variadic has to be the last argument"
            break
        }
        
        append(&args, AstDeclaration{name.value, type, nil, int(DeclFlags.IMMUTABLE), span_stretch(&start, &type_span)})

        if len(ctx.tokens) == 0 {
            parser_error_at_next(ctx, "Unexpected end of stream found while parsing function arguments.")
            return nil
        }

        if ctx.tokens[0].type == .CLOSE_PAREN {
            break
        } 

        if ctx.tokens[0].type == .COMMA {
            advance_token(ctx)
        } else {
            parser_error_at_next(ctx, "Expected ',' or ')' after argument.")
            return nil
        }
    }
    advance_token(ctx) // Consume the closing ')'

    return_type: Type = PrimitiveType.VOID

    for len(ctx.tokens) > 0 && ctx.tokens[0].type != .OPEN_BRACE && ctx.tokens[0].type != .SEMI_COLON {
        if ctx.tokens[0].type == .DIRECTIVE_FOREIGN {
            flags |= int(ProcFlags.FOREIGN)
            advance_token(ctx)
        } else {
            return_type = parse_type(ctx)
        }
    }

    body: Ast = nil
    end: Span = start
    if flags & int(ProcFlags.FOREIGN) == 0 {
        body = parse_scope(ctx, args[:])
        end = get_span(body)^
    } else {
        res := expect_token(ctx, .SEMI_COLON)
        if res == nil {
            parser_error_at_next(ctx, "Expected semicolon after #foreign marked function.")
        } else {
            end = span_from_token(res)
        }
    }

    it := new(AstProcedure)
    it^ = AstProcedure{args, body, return_type, flags, span_stretch(&start, &end)}
    return it
}

parse_struct :: proc(ctx: ^ParseCtx) -> Ast {
    start := span_from_token(&ctx.tokens[0])    
    advance_token(ctx)
    
    if expect_token(ctx, .OPEN_BRACE) == nil {
        parser_error_at_next(ctx, "Expected '{' after 'struct'.")
        return nil
    }

    flags := 0
    fields : [dynamic] StructField;
    for len(ctx.tokens) != 0 && ctx.tokens[0].type != .CLOSE_BRACE {
        name := &ctx.tokens[0]
        start := span_from_token(name)
        if name.type != .IDENT {
            parser_error_at_next(ctx, "Expected field name but found '%v'.", name.value)
            return nil
        }
        advance_token(ctx)

        if expect_token(ctx, .COLON) == nil {
            parser_error_at_next(ctx, "Expected ':' after field name.")
            return nil
        }

        type_span: Span
        type := parse_type(ctx, &type_span)
        
        append(&fields, StructField{name.value, type, nil, span_stretch(&start, &type_span)})

        if len(ctx.tokens) == 0 {
            parser_error_at_next(ctx, "Unexpected end of stream found while parsing structure.")
            return nil
        }

        if ctx.tokens[0].type == .CLOSE_BRACE {
            break
        } 

        if ctx.tokens[0].type == .COMMA {
            advance_token(ctx)
        } else {
            parser_error_at_next(ctx, "Expected ',' or '}' after field.")
            return nil
        }
    }
    close_brace := expect_token(ctx, .CLOSE_BRACE)
    if close_brace == nil { return nil }

    end := span_from_token(close_brace)

    it := new(AstStruct)
    it^ = AstStruct{fields, "", flags, -1, span_stretch(&start, &end)}
    return it
}

parse_expr :: proc(ctx: ^ParseCtx, min_prec := 1) -> Ast {
    if len(ctx.tokens) != 0 {
        if ctx.tokens[0].type == .KEYWORD_FN {
            return parse_function(ctx)
        } else if ctx.tokens[0].type == .KEYWORD_STRUCT {
            return parse_struct(ctx)
        }
    }
    return parse_expr_1(ctx, parse_primary(ctx), min_prec)
}

expect_token :: proc(ctx: ^ParseCtx, args: ..TokenType) -> ^Token {
    if len(ctx.tokens) == 0 {
        return nil
    }

    for arg in args {
        if arg == ctx.tokens[0].type {
            res := &ctx.tokens[0] 
            advance_token(ctx)
            return res
        }
    }

    return nil
}

// @Unfinished This doesnt even arrays.
parse_type :: proc(ctx: ^ParseCtx, span: ^Span = nil) -> Type {
    if len(ctx.tokens) == 0 { return PrimitiveType.UNKNOWN }
    token := &ctx.tokens[0]

    if span != nil { span^ = span_from_token(&ctx.tokens[0]) }

    advance_token(ctx)

    if token.type == .ADDRESS {
        end_span: Span
        inner := new(Type)
        inner^ = parse_type(ctx, &end_span)
        if span != nil { span^ = span_stretch(span, &end_span) }

        return PointerType{inner}
    }

    if token.type == .OPEN_BRACKET {
        is_dynamic := false
        if expect_token(ctx, .DOT_DOT) != nil {
            is_dynamic = true
        }
        if expect_token(ctx, .CLOSE_BRACKET) == nil {
            parser_error_at_next(ctx, "Expected ']' since fixed size arrays are not yet supported. Sorry!")
            return nil
        }

        end_span: Span
        inner := new(Type)
        inner^ = parse_type(ctx, &end_span)
        if span != nil { span^ = span_stretch(span, &end_span) }

        return ArrayType{inner, is_dynamic}
    }

    if token.type == .IDENT {
        sym, _ := symbol_table_lookup(ctx, token.value)
        if sym == nil {
            parser_error_at_next(ctx, "Unknown type '%v'.", token.value)
        }
        if sym.ast.value == nil {
            return UnparsedType{sym.ast}
        }
        // @Unfinished @Todo check that sym.ast.value is a struct and give an error if it isn't.
        return StructType{sym.ast.value.(^AstStruct)}
    }

    if token.type == .KEYWORD_INT { return PrimitiveType.S64 }
    if token.type == .KEYWORD_FLOAT { return PrimitiveType.F64 }
    if token.type == .KEYWORD_STRING { return PrimitiveType.STRING }
    if token.type == .KEYWORD_VOID { return PrimitiveType.VOID }
    if token.type == .DOT_DOT { return PrimitiveType.VARIADIC }
	if token.type == .KEYWORD_F32 { return PrimitiveType.F32 }
	if token.type == .KEYWORD_F64 { return PrimitiveType.F64 }
	if token.type == .KEYWORD_U8 { return PrimitiveType.U8 }
	if token.type == .KEYWORD_U16 { return PrimitiveType.U16 }
	if token.type == .KEYWORD_U32 { return PrimitiveType.U32 }
	if token.type == .KEYWORD_U64 { return PrimitiveType.U64 }
	if token.type == .KEYWORD_S8 { return PrimitiveType.S8 }
	if token.type == .KEYWORD_S16 { return PrimitiveType.S16 }
	if token.type == .KEYWORD_S32 { return PrimitiveType.S32 }
	if token.type == .KEYWORD_S64 { return PrimitiveType.S64 }

    parser_error_at_next(ctx, "Could not parse type '%v'.", token.value)
    return PrimitiveType.UNKNOWN
}

needs_semicolon_after :: proc(ast: Ast) -> bool {
    #partial switch _ in ast {
    case ^AstDeclaration: 
        #partial switch _ in ast.(^AstDeclaration).value {
        case ^AstProcedure: return false
        case ^AstStruct: return false
        }
    }

    return true
}

parse_while_stmt :: proc(ctx: ^ParseCtx) -> Ast {
    if len(ctx.tokens) == 0 { return nil }
    start := span_from_token(&ctx.tokens[0])
    advance_token(ctx)

    predicate := parse_expr(ctx)
    if predicate == nil { return nil }
    body := parse_scope(ctx)
    if body == nil { return nil }

    it := new(AstWhile)
    it^ = AstWhile{predicate, body, span_stretch(&start, get_span(body))}
    return it
}

parse_if_stmt :: proc(ctx: ^ParseCtx) -> Ast {
    if len(ctx.tokens) == 0 { return nil }
    start := span_from_token(&ctx.tokens[0])
    advance_token(ctx)

    predicate := parse_expr(ctx)
    if predicate == nil { return nil }
    body := parse_scope(ctx)
    if body == nil { return nil }

    it := new(AstIf)
    it^ = AstIf{predicate, body, span_stretch(&start, get_span(body))}
    return it
}

parse_return_stmt :: proc(ctx: ^ParseCtx) -> Ast {
    if len(ctx.tokens) == 0 { return nil }
    start := span_from_token(&ctx.tokens[0])
    advance_token(ctx)

    value := parse_expr(ctx)

    it := new(AstReturn)
    it^ = AstReturn{value, span_stretch(&start, get_span(value))}
    return it
}

parse_stmt :: proc(ctx: ^ParseCtx) -> Ast {
    #partial switch ctx.tokens[0].type {
    case .SEMI_COLON: 
        advance_token(ctx)
        return parse_stmt(ctx)
    case .OPEN_BRACE:
        return parse_scope(ctx)
    case .KEYWORD_RETURN:
        res := parse_return_stmt(ctx)
        if expect_token(ctx, .SEMI_COLON) == nil { 
            s := get_span(res)
            compiler_error(ctx.file, Span{s.end, s.end + 1}, "Expected ';' after statement but found %v. Forgot a semicolon?", next_token_value(ctx))
        }
        return res
    case .KEYWORD_IF:
        return parse_if_stmt(ctx)
    case .KEYWORD_WHILE:
        return parse_while_stmt(ctx)
    case .DIRECTIVE_EMIT:
        advance_token(ctx)
        val := expect_token(ctx, .STRING_LIT)
        if val == nil { 
            s := span_from_token(val)
            compiler_error(ctx.file, Span{s.end, s.end + 1}, "Expected string literal after #emit but found %v.", next_token_value(ctx))
        }
        it := new(AstEmitCode)
        it^ = AstEmitCode{val.value, span_from_token(val)}
        return it
    }
    res := parse_expr(ctx, 0)

    if needs_semicolon_after(res) && expect_token(ctx, .SEMI_COLON) == nil { 
        s := get_span(res)
        compiler_error(ctx.file, Span{s.end, s.end + 1}, "Expected ';' after statement but found %v. Forgot a semicolon?", next_token_value(ctx))
    }

    return res
}

next_token_value :: proc(ctx: ^ParseCtx) -> string {
    if len(ctx.tokens) == 0 {
        return "EOF"
    } else {
        return ctx.tokens[0].value
    }
}

push_scope :: proc(ctx: ^ParseCtx) -> int {
    append(&ctx.scopes, SymbolTable{})

    index := len(ctx.scopes) - 1
    ctx.scopes[index].parent = ctx.current_scope
    ctx.current_scope = index
    return index
}

pop_scope :: proc(ctx: ^ParseCtx) -> bool {
    if get_current_scope(ctx).parent == -1 { 
        parser_error_at_next(ctx, "Trying to pop scope on global scope.")
        return false 
    }

    ctx.current_scope = get_current_scope(ctx).parent
    return true
}

parse_scope :: proc(ctx: ^ParseCtx, extra: [] AstDeclaration = {}) -> Ast {
    start : Span
    end : Span

    open_brace := expect_token(ctx, .OPEN_BRACE)
    if open_brace == nil { 
        parser_error_at_next(ctx, "Expected '{{' for opening scope but found %v.", next_token_value(ctx))
        return nil
    } else {
        start = span_from_token(open_brace)
    }

    symbol_table := push_scope(ctx)
    for &decl in extra {
        symbol_table_insert(ctx, &decl)
    }

    stmts: [dynamic] Ast
    for {
        if len(ctx.tokens) == 0 {
            parser_error_at_next(ctx, "Expected '}}' but found EOF.")
            return nil
        }
        if ctx.tokens[0].type == .CLOSE_BRACE {
            end = span_from_token(&ctx.tokens[0])
            advance_token(ctx)
            break
        }
        
        append(&stmts, parse_stmt(ctx))
    }

    pop_scope(ctx)

    it := new(AstScope)
    it^ = AstScope{stmts, symbol_table, 0, span_stretch(&start, &end)}
    return it
}

// Returns global scope.
parse :: proc(ctx: ^ParseCtx) -> Ast {
    global_scope := push_scope(ctx)

    stmts : [dynamic] Ast
    for len(ctx.tokens) != 0 {
        append(&stmts, parse_stmt(ctx))
    }

    flags := int(ScopeFlags.FILE)

    it := new(AstScope)
    it^ = AstScope{stmts, global_scope, flags, Span{0, len(ctx.file.input) - 1}}

    return it
}

