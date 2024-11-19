package zeus

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

            contains_dot := false
            for i < len(input) && is_numeric(input[i]) { 
                if input[i] == '.' {
                    if contains_dot {
                         compiler_error(ctx, Span{i, i + 1}, "Found stray second '.' in number literal.")
                    }
                    if i + 1 < len(input) && input[i + 1] == '.' { // 1..10
                        break
                    }
                    contains_dot = true
                }
                i += 1 
            }

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
