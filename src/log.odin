package zeus

import "core:fmt"
import "core:strings"
import "core:os"
import "core:c/libc"

Span :: struct {
    start: int,
    end: int,
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

