package main

import "core:fmt"
import "core:strings"
import "core:os"
import "core:c/libc"

TokenType :: enum u8 {
    NUMBER_LIT,
    STRING_LIT,
    IDENT,

    COLON, 
    SEMI_COLON, 
    COMMA,

    ASSIGN,   // =
    ADD_ASSIGN, // +=
    SUB_ASSIGN, // -=
    MUL_ASSIGN, // *=
    DIV_ASSIGN, // /=
    MOD_ASSIGN, // %=

    EQUALITY, // ==
    INEQUALITY, // !=
    GT, // >
    GE, // >=
    LT, // <
    LE, // <=

    OPEN_PAREN, CLOSE_PAREN, // ()
    OPEN_BRACE, CLOSE_BRACE, // {}
    OPEN_BRACKET, CLOSE_BRACKET, // []

    DOT_DOT, // ..
    DOT, // .

    ADD,
    SUB,
    MUL,
    DIV,

    MOD, // %

    NOT, // !
    ADDRESS, // @

    KEYWORD_VOID,
    KEYWORD_INT,
    KEYWORD_FLOAT,
    KEYWORD_STRING,

    KEYWORD_F32,
    KEYWORD_F64,
    KEYWORD_U8,
    KEYWORD_U16,
    KEYWORD_U32,
    KEYWORD_U64,
    KEYWORD_S8,
    KEYWORD_S16,
    KEYWORD_S32,
    KEYWORD_S64,

    KEYWORD_FN,
    KEYWORD_IF,
    KEYWORD_WHILE,
    KEYWORD_RETURN,
    KEYWORD_STRUCT,

    DIRECTIVE_FOREIGN,
    DIRECTIVE_IF,
    DIRECTIVE_EMIT, // @Temporary while we still directly output to C.
}

Token :: struct {
    value: string,
    type: TokenType,
    offset: int,
}

is_digit :: proc(ch: byte) -> bool {
    return ch >= '0' && ch <= '9'
}

is_numeric :: proc(ch: byte) -> bool {
    return is_digit(ch) || ch == '.'
}

is_space :: proc(ch: byte) -> bool {
    return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\v' || ch == '\f' || ch == '\r'
}

is_ident_start :: proc(ch: byte) -> bool {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

is_ident :: proc(ch: byte) -> bool {
    return is_ident_start(ch) || is_digit(ch)
}

TokenMap :: struct {
    key: string,
    value: TokenType 
}

OPERATOR_MAP :: [] TokenMap{
    TokenMap{":", .COLON},
    TokenMap{";", .SEMI_COLON},
    TokenMap{",", .COMMA},

    TokenMap{"..", .DOT_DOT},
    TokenMap{".", .DOT},

    TokenMap{"==", .EQUALITY},
    TokenMap{"!=", .INEQUALITY},
    TokenMap{">=", .GE},
    TokenMap{">", .GT},
    TokenMap{"<=", .LE},
    TokenMap{"<", .LT},
    TokenMap{"=", .ASSIGN},
    TokenMap{"+=", .ADD_ASSIGN},
    TokenMap{"-=", .SUB_ASSIGN},
    TokenMap{"*=", .MUL_ASSIGN},
    TokenMap{"/=", .DIV_ASSIGN},
    TokenMap{"%=", .MOD_ASSIGN},

    TokenMap{"(", .OPEN_PAREN},
    TokenMap{")", .CLOSE_PAREN},

    TokenMap{"{", .OPEN_BRACE},
    TokenMap{"}", .CLOSE_BRACE},

    TokenMap{"[", .OPEN_BRACKET},
    TokenMap{"]", .CLOSE_BRACKET},

    TokenMap{"+", .ADD},
    TokenMap{"-", .SUB},
    TokenMap{"*", .MUL},
    TokenMap{"/", .MUL},
    TokenMap{"%", .MOD},

    TokenMap{"!", .NOT},
    TokenMap{"@", .ADDRESS},
};

KEYWORD_MAP :: [] TokenMap{
    TokenMap{"void", .KEYWORD_VOID},
    TokenMap{"int", .KEYWORD_INT},
    TokenMap{"float", .KEYWORD_FLOAT},
    TokenMap{"string", .KEYWORD_STRING},
    TokenMap{"if", .KEYWORD_IF},
    TokenMap{"while", .KEYWORD_WHILE},
    TokenMap{"return", .KEYWORD_RETURN},
    TokenMap{"fn", .KEYWORD_FN},
    TokenMap{"struct", .KEYWORD_STRUCT},

    TokenMap{"f32", .KEYWORD_F32},
    TokenMap{"f64", .KEYWORD_F64},
    TokenMap{"u8", .KEYWORD_U8},
    TokenMap{"u16", .KEYWORD_U16},
    TokenMap{"u32", .KEYWORD_U32},
    TokenMap{"u64", .KEYWORD_U64},
    TokenMap{"s8", .KEYWORD_S8},
    TokenMap{"s16", .KEYWORD_S16},
    TokenMap{"s32", .KEYWORD_S32},
    TokenMap{"s64", .KEYWORD_S64},
};

DIRECTIVE_MAP :: [] TokenMap{
    TokenMap{"#foreign", .DIRECTIVE_FOREIGN},
    TokenMap{"#if", .DIRECTIVE_IF},
    TokenMap{"#emit", .DIRECTIVE_EMIT},
}

tokenize :: proc(ctx: ^FileCtx) -> [dynamic] Token {
    tokens : [dynamic] Token

    input := ctx.input

    i := 0
    for {
        if i >= len(input) { break }

        if is_space(input[i]) {
            i += 1
        } else if is_ident_start(input[i]) {
            start := i
            i += 1

            for i < len(input) && is_ident(input[i]) { i += 1 }

            type := TokenType.IDENT
            value := input[start:i]
            for m in KEYWORD_MAP {
                if m.key == value {
                    type = m.value
                    break;
                } 
            }

            append(&tokens, Token{value, type, start})
        } else if is_digit(input[i]) {
            start := i
            i += 1

            for i < len(input) && is_numeric(input[i]) { i += 1 }

            append(&tokens, Token{input[start:i], .NUMBER_LIT, start})
        } else if input[i] == '"' {
            start := i
            i += 1

            escaped := false
            for i < len(input) && !(input[i] == '"' && !escaped) { 
                escaped = input[i] == '\\'
                i += 1 
            }
            i += 1 // grab that ending "

            append(&tokens, Token{input[start+1 : i-1], .STRING_LIT, start})
        } else if i + 1 < len(input) && input[i] == '/' && input[i + 1] == '/' {
            for i < len(input) && input[i] != '\n' {
                i += 1
            }
        } else if i + 1 < len(input) && input[i] == '/' && input[i + 1] == '*' {
            for i + 1 < len(input) && !(input[i] == '*' && input[i + 1] == '/') {
                i += 1
            }
            i += 1
        } else if input[i] == '#' {
            start := i
            i += 1

            for i < len(input) && is_ident(input[i]) { i += 1 }

            invalid :: TokenType.IDENT
            type := invalid
            value := input[start:i]
            for m in DIRECTIVE_MAP {
                if m.key == value {
                    type = m.value
                    break;
                } 
            }

            if type == invalid {
                compiler_error(ctx, Span{start, i}, "Unknown compiler directive '%v'", value)
            } else {
                append(&tokens, Token{value, type, start})
            }
        } else {
            found := false;
            for m in OPERATOR_MAP {
                if i + len(m.key) > len(input) {
                    continue;
                }

                if m.key == input[i:i+len(m.key)] {
                    found = true

                    append(&tokens, Token{input[i : i+len(m.key)], m.value, i})
                    i += len(m.key)
                    break;
                }
            }

            if !found {
                compiler_error(ctx, Span{i, i + 1}, "Unknown character '%c'", input[i])
                break;
            }
        }
    }

    return tokens
}

get_pos_from_offset :: proc(input: string, offset: int) -> (int, int) {
    row, col := 1, 1
    for i := 0; i < offset; i += 1 {
        if input[i] == '\n' {
            row += 1
            col = 0
        } else {
            col += 1
        }
    }
    return row, col
}

Span :: struct {
    start: int,
    end: int,
}

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

op_to_string :: proc(op: TokenType) -> string {
    #partial switch op {
    case .ADD: return "+" 
    case .SUB: return "-" 
    case .MUL: return "*" 
    case .DIV: return "/" 
    case .ASSIGN: return "="
    case .MOD: return "%"
    case .ADD_ASSIGN: return "+="
    case .SUB_ASSIGN: return "-="
    case .MUL_ASSIGN: return "*="
    case .DIV_ASSIGN: return "/="
    case .EQUALITY: return "=="
    case .INEQUALITY: return "!="
    case .GT: return ">"
    case .GE: return ">="
    case .LT: return "<"
    case .LE: return "<="
    case .ADDRESS: return "@"
    }
    return "<Unknown>"
}

type_to_string :: proc(type: Type) -> string {
    return fmt.aprint(type)
}

// @Leak This procedure leaks some memory but who cares, it's just for debugging.
ast_to_string :: proc(ast: Ast, indent := 0) -> string {
    if ast == nil { return "<nil>" }
    switch _ in ast {
        case ^AstBinOp:
            it := ast.(^AstBinOp);
            lhs_str := ast_to_string(it.lhs)
            rhs_str := ast_to_string(it.rhs)
            return fmt.aprintf("(%v %v %v)", lhs_str, op_to_string(it.op), rhs_str);

        case ^AstAssignment:
            it := ast.(^AstAssignment);
            lhs_str := ast_to_string(it.lhs)
            rhs_str := ast_to_string(it.rhs)
            return fmt.aprintf("%v %v %v", lhs_str, op_to_string(it.op), rhs_str);

        case ^AstUnaryOp: 
            it := ast.(^AstUnaryOp);
            return fmt.aprintf("(%v%v)", op_to_string(it.op), ast_to_string(it.inner));

        case ^AstAccess:
            it := ast.(^AstAccess);
            return fmt.aprintf("%v.%v", ast_to_string(it.node), it.value)

        case ^AstIndex:
            it := ast.(^AstIndex);
            return fmt.aprintf("%v[%v]", ast_to_string(it.array), ast_to_string(it.index))

        case ^AstNumberLit: return ast.(^AstNumberLit).value;
        case ^AstVariable: return ast.(^AstVariable).value;
        case ^AstStringLit: return strings.concatenate({"\"", ast.(^AstStringLit).value, "\""});
        case ^AstArrayLit: return "<ARRAY LITERAL>"
        
        case ^AstDeclaration: 
            it := ast.(^AstDeclaration);
            op := (it.flags & int(DeclFlags.CONSTANT) != 0) ? ":" : "="
            return fmt.aprintf("(%v : %v %v %v)", it.name, it.type, op, ast_to_string(it.value))

        case ^AstProcedureCall:
            it := ast.(^AstProcedureCall);

            args : [dynamic] string
            defer delete(args)
            for stmt in it.args {
                append(&args, ast_to_string(stmt))
            }

            return fmt.aprintf("%v(%v)", it.procedure, strings.join(args[:], ", "))

        case ^AstScope:
            it := ast.(^AstScope);
            lines : strings.Builder
            for stmt in it.stmts {
                fmt.sbprintln(&lines, ast_to_string(stmt))
            }
            return strings.to_string(lines)

        case ^AstProcedure:
            it := ast.(^AstProcedure);

            args : strings.Builder
            defer strings.builder_destroy(&args)
            for i in 0..<len(it.args) {
                fmt.sbprintf(&args, "%v: %v", it.args[i].name, it.args[i].type)
                if i != len(it.args) - 1 {
                    fmt.sbprint(&args, ", ")
                }
            }

            return fmt.aprintf("fn(%v) %v {{\n%v\n}}", strings.to_string(args), it.return_type, ast_to_string(it.body))

        case ^AstStruct:
            it := ast.(^AstStruct);
            return "struct { TODO }"

        case ^AstIf: 
            it := ast.(^AstIf);
            return fmt.aprintf("if %v {{\n%v\n}}", ast_to_string(it.predicate), ast_to_string(it.body))

        case ^AstWhile: 
            it := ast.(^AstWhile);
            return fmt.aprintf("while %v {{\n%v\n}}", ast_to_string(it.predicate), ast_to_string(it.body))

        case ^AstReturn: 
            it := ast.(^AstReturn);
            return fmt.aprintf("return %v", ast_to_string(it.value))

        case ^AstEmitCode:
            it := ast.(^AstEmitCode);
            return fmt.aprintf("#emit \"%v\"", it.code)
    }
    return "<Unknown>"
}

FileCtx :: struct {
    input: string,
    filename: string,
    error_count: int,
}

find_line_start :: proc(input: string, _offset: int) -> int {
    offset := _offset
    for offset > 0 && input[offset - 1] != '\n' {
        offset -= 1
    }
    return offset
}

find_line_end :: proc(input: string, _offset: int) -> int {
    offset := _offset
    for offset > 0 && input[offset] != '\n' {
        offset += 1
    }
    return offset
}

TERM_RED := "\033[31m"
TERM_RESET := "\033[0m"
TERM_RED_BOLD := "\033[1;31m"

compiler_error :: proc(ctx: ^FileCtx, span: Span, format: string, args: ..any) {
    ctx.error_count += 1
    row, col := get_pos_from_offset(ctx.input, span.start)

    { // Print error message.
        fmt.eprintf("%v:%v:%v", ctx.filename, row, col + 1);
        fmt.eprint(TERM_RED_BOLD, "Error:", TERM_RESET)
        fmt.eprintfln(format, ..args)
    }

    line_start := find_line_start(ctx.input, span.start)
    line_end := find_line_end(ctx.input, span.start)
    line_text := ctx.input[line_start:line_end]

    fmt.eprintfln(" %v | %v", row, line_text)

    { // Print leading whitespace before carrots
        line_str := fmt.aprint(row);
        defer delete(line_str)

        line_ws_str := strings.repeat(" ", len(line_str))
        defer delete(line_ws_str)

        fmt.eprintf(" %v   ", line_ws_str)

        for i in 0..<col {
            fmt.eprint(" ")
        }
    }

    { // Print carrots
        fmt.eprint(TERM_RED);
        for _ in span.start..<span.end {
            fmt.eprint("^")
        }
        fmt.eprint(TERM_RESET);
        fmt.eprintln()
    }
}

get_main_file_data :: proc() -> ([] u8, string) {
    if len(os.args) < 2 {
        fmt.eprintln("Error: No input files.")
        return nil, "";
    }

    file := os.args[1]
    data, ok := os.read_entire_file(file)
    if !ok {
        fmt.eprintfln("Error: Could not open file '%v'", file)
        return nil, "";
    }

    return data, file
}

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
    // c := table
    // fmt.println("Symbols:")
    // indent := 1;
    // for c != nil {
    //     for i in c.symbols {
    //         fmt.println(strings.repeat("    ", indent), "-", i)
    //     }
    //     indent += 1
    //     c = c.parent
    // }
    
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

        case ^AstNumberLit: return PrimitiveType.NUMBER
        case ^AstStringLit: return PrimitiveType.STRING

        case ^AstArrayLit: return ast.(^AstArrayLit).type

        case ^AstDeclaration: return PrimitiveType.VOID

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
                        // @Hack We should probably not just cast the value to a number literal here.
                        if strings.contains_rune(it.value.(^AstNumberLit).value, '.') {
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

main :: proc() {
    data, file := get_main_file_data()
    if len(data) == 0 { return; }
    defer delete(data)

    input := string(data)
    ctx := FileCtx{input, file, 0}

    tokens := tokenize(&ctx)
    defer delete(tokens)

    parse_ctx : ParseCtx;
    parse_ctx.tokens = tokens[:]
    parse_ctx.file = &ctx

    ast := parse(&parse_ctx)
    if len(parse_ctx.tokens) != 0 {
        compiler_error(&ctx, span_from_token(&parse_ctx.tokens[0]), "Could not parse rest of input. Found '%v'", parse_ctx.tokens[0])
    }
    // if get_current_scope(parse_ctx).parent != nil {
    //     compiler_error(&ctx, Span{0, 0}, "Compiler error: left with non-global scope after parsing.")
    // }

    semantic_analize(&parse_ctx, ast)

    if ctx.error_count == 0 {
        fmt.println("Success!")
    } else if ctx.error_count == 1 {
        fmt.printfln("Compilation failed with 1 error.")
    } else {
        fmt.printfln("Compilation failed with %v errors.", ctx.error_count)
    }

    if ctx.error_count == 0 {
        gen_ctx := CGenCtx{&parse_ctx, 0, false, false}
        res := c_code_gen(&gen_ctx, ast)

        c_file, _ := strings.replace_all(ctx.filename, ".zeus", ".c")
        os.write_entire_file(c_file, transmute([]u8)res)

        exe_file, _ := strings.replace_all(ctx.filename, ".zeus", "")
        command := fmt.aprintf("gcc -o %v %v", exe_file, c_file)

        status := libc.system(strings.unsafe_string_to_cstring(command))
    }
}
