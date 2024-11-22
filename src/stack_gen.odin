package zeus

import "stack"

import "core:strconv"
import "core:fmt"

StackGenCtx :: struct {
    parser: ^ParseCtx,
    machine: stack.Machine,
    code: [dynamic] int,

}

token_bin_op_to_stack :: proc(t: TokenType) -> int {
    #partial switch t {
    case .EQUALITY: return stack.opcode(.EQ)
    case .INEQUALITY: return stack.opcode(.NEQ)
    case .GT: return stack.opcode(.GT)
    case .GE: return stack.opcode(.GE)
    case .LT: return stack.opcode(.LT)
    case .LE: return stack.opcode(.LE)
    case .ADD: return stack.opcode(.ADD)
    case .SUB: return stack.opcode(.SUB)
    case .MUL: return stack.opcode(.MUL)
    case .DIV: return stack.opcode(.DIV)
    case .MOD: return stack.opcode(.MOD)

    case .LOG_AND: return stack.opcode(.AND)
    case .LOG_OR: return stack.opcode(.OR)

    case .ADD_ASSIGN: return stack.opcode(.ADD)
    case .SUB_ASSIGN: return stack.opcode(.SUB)
    case .MUL_ASSIGN: return stack.opcode(.MUL)
    case .DIV_ASSIGN: return stack.opcode(.DIV)
    case .MOD_ASSIGN: return stack.opcode(.MOD)
    }
    panic("Unreachable")
}


stack_code_gen :: proc(ctx: ^StackGenCtx, ast: Ast) {
    switch _ in ast {
    case ^AstNumberLit:
		n, ok := strconv.parse_i64_of_base(ast.(^AstNumberLit).value, 10)
        append(&ctx.code, stack.opcode(.CONST_INT), int(n))
    case ^AstVariable:
        it := ast.(^AstVariable)
        decl, scope := symbol_table_lookup(ctx.parser, it.value)

        i := 0
        current_scope := ctx.parser.current_scope
        for current_scope != scope {
            i += 1
            current_scope = ctx.parser.scopes[current_scope].parent
        }
        if i == 0 {
            append(&ctx.code, stack.opcode(.LLOAD), decl.n)
        } else {
            append(&ctx.code, stack.opcode(.LLOAD_FROM), i, decl.n)
        }
        
    case ^AstStringLit: panic("todo")
    case ^AstArrayLit: panic("todo")
    case ^AstBinOp:
        it := ast.(^AstBinOp)
        stack_code_gen(ctx, it.lhs)
        stack_code_gen(ctx, it.rhs)
        append(&ctx.code, token_bin_op_to_stack(it.op))
    case ^AstUnaryOp: panic("todo")
    case ^AstDeclaration:
        it := ast.(^AstDeclaration)
        stack_code_gen(ctx, it.value)

        decl, _ := symbol_table_lookup(ctx.parser, it.name)
        append(&ctx.code, stack.opcode(.LSTORE), decl.n)
        
    case ^AstAssignment:
        it := ast.(^AstAssignment)
        if it.op == .ASSIGN {
            stack_code_gen(ctx, it.rhs)
        } else {
            stack_code_gen(ctx, it.lhs)
            stack_code_gen(ctx, it.rhs)
            append(&ctx.code, token_bin_op_to_stack(it.op))
        }

        decl, scope := symbol_table_lookup(ctx.parser, it.lhs.(^AstVariable).value)
        i := 0
        current_scope := ctx.parser.current_scope
        for current_scope != scope {
            i += 1
            current_scope = ctx.parser.scopes[current_scope].parent
        }
        if i == 0 {
            append(&ctx.code, stack.opcode(.LSTORE), decl.n)
        } else {
            append(&ctx.code, stack.opcode(.LSTORE_FROM), i, decl.n)
        }

    case ^AstProcedureCall: panic("todo")
    case ^AstScope:
        it := ast.(^AstScope)

        append(&ctx.code, stack.opcode(.PUSH_FRAME))

        ctx.parser.current_scope = it.table
        for &stmt in it.stmts {
            stack_code_gen(ctx, stmt)
        }
        ctx.parser.current_scope = get_current_scope(ctx.parser).parent

        append(&ctx.code, stack.opcode(.LOCALS_TRACE)) // @Temporary
        append(&ctx.code, stack.opcode(.POP_FRAME))
    case ^AstProcedure: panic("todo")
    case ^AstStruct: panic("todo")
    case ^AstIf:
        it := ast.(^AstIf)

        stack_code_gen(ctx, it.predicate)

        append(&ctx.code, stack.opcode(.NOT), stack.opcode(.JMP_IF), 0)
        jmp_addr := &ctx.code[len(ctx.code) - 1]

        stack_code_gen(ctx, it.body)

        jmp_addr^ = len(ctx.code)
        
    case ^AstWhile: panic("todo")
    case ^AstReturn: panic("todo")
    case ^AstAccess: panic("todo")
    case ^AstIndex: panic("todo")
    case ^AstEmitCode: panic("todo")
    }
}
