package zeus

import "core:fmt"
import "core:strings"
import "core:os"
import "core:c/libc"

import "stack"

FileCtx :: struct {
    input: string,
    filename: string,
    error_count: int,
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
    } else {
        fmt.printfln("Compilation failed with %v %v.", ctx.error_count, ctx.error_count == 1 ? "error" : "errors")
        os.exit(1)
    }

    gen_mode := "stack"

    if gen_mode == "c" {
        gen_ctx := CGenCtx{&parse_ctx, 0, false, false}
        res := c_code_gen(&gen_ctx, ast)

        c_file, _ := strings.replace_all(ctx.filename, ".zeus", ".c")
        os.write_entire_file(c_file, transmute([]u8)res)

        exe_file, _ := strings.replace_all(ctx.filename, ".zeus", "")
        command := fmt.aprintf("gcc -o %v %v", exe_file, c_file)

        status := libc.system(strings.unsafe_string_to_cstring(command))
    } else if gen_mode == "stack" {
        gen_ctx: StackGenCtx
        gen_ctx.parser = &parse_ctx
        stack_code_gen(&gen_ctx, ast)
        append(&gen_ctx.code, stack.opcode(.HALT))
        gen_ctx.machine.instructions = gen_ctx.code[:]
        fmt.println("Instructions:", gen_ctx.code)
        stack.run(&gen_ctx.machine)

    }
}
