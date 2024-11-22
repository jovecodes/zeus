package stack

import "core:fmt"
import "core:strings"
import "core:os"

Instruction :: enum int {
    // arithmetic
    HALT,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,

    // These don't short circuit
    NOT,
    OR,
    AND,

    // are faster and in place
    INCR,
    DECR,

    // memory
    COPY, // (index)
    DELETE, // (index)

    // takes field index
    GET_FIELD, // [structure, index]
    SET_FIELD, // [structure, index, value]

    PUSH_FRAME,
    POP_FRAME, // Just removes the current frame and does not jump out. If you want to jump to the return address use BRK.

    // constants
    CONST_INT, // (value)
    CONST_FLOAT, // (index of data in the machine 'float_data')
    // CONST_STRING, // takes index of data in the machine 'string_data' 

    // comparison
    EQ,
    NEQ,
    LT,
    LE,
    GT,
    GE,

    LLOAD, // (variable index: n) local load
    LSTORE, // (variable index: n) local store

    LLOAD_FROM, // (relative scope, variable index: n) local load from relative scope
    LSTORE_FROM, // (relative scope, variable index: n) local store from relative scope

    // jumps use the control_stack for return addresses
    BRK, // jumps unconditionally
    BRK_IF, // [value] jumps if non-zero

    // absolute: these use compile time constants
    JMP, // jumps unconditionally
    JMP_IF, // [value] jumps if non-zero

    // relative: these use compile time constants
    RJMP, // jumps unconditionally
    RJMP_IF, // [value] jumps if non-zero


    // temporary
    STACK_TRACE, // prints the entire operand stack
    LOCALS_TRACE, // prints all the locals in the current frame
}

Structure :: struct {
    fields: [] Operand
}

Operand :: union {
    int,
    f64,
    Structure,
}

Frame :: struct {
    return_address: int,
    locals: [dynamic] Operand,
}

Machine :: struct {
    operands: [dynamic] Operand,
    frames: [dynamic] Frame,
    instructions: [] int,
    ip: int,

    float_data: [dynamic] f64,
    string_data: [dynamic] string,
}

back :: proc(array: ^$T/[dynamic]$E, loc := #caller_location) -> ^E {
	assert(len(array) > 0, loc=loc)
    return &array[len(array)-1]
}

// returns return address
pop_frame :: proc(machine: ^Machine) -> int {
    assert(len(machine.frames) > 0)
    delete(back(&machine.frames).locals)
    return pop(&machine.frames).return_address
}

opcode :: proc(instruction: Instruction) -> int {
    return int(instruction)
}

get_next_instruction :: proc(machine: ^Machine) -> int {
    if len(machine.instructions) <= machine.ip { return -1 }
    return int(machine.instructions[machine.ip])
}

can_do_arithmetic :: proc(a: ^Operand, b: ^Operand) -> bool {
    #partial switch _ in a {
    case int:
        #partial switch _ in b {
        case int:
            return true
        }
    case f64:
        #partial switch _ in b {
        case f64:
            return true
        }
    }

    return false
}

do_bin_op :: proc(
    machine: ^Machine, 
    name: string,
    op: proc(a: Operand, b: Operand) -> Operand
) -> bool {
    if len(machine.operands) < 2 {
        print_error("Not enough operands for %v", name)
        return true
    }

    b := pop(&machine.operands)
    a := pop(&machine.operands)

    if !can_do_arithmetic(&a, &b) {
        print_error("Type mismatch in %v", name)
        return true
    }

    append(&machine.operands, op(a, b))

    // Move to the next instruction
    machine.ip += 1
    return false
}

TERM_RED_BOLD := "\033[1;31m"
TERM_RESET := "\033[0m"

print_error :: proc(format: string, args: ..any) {
    fmt.eprint(TERM_RED_BOLD, "Runtime error", TERM_RESET, ": ", sep="")
    fmt.eprintfln(format, ..args)
}

// returns if the program should halt
step :: proc(machine: ^Machine) -> bool {
    instruction := get_next_instruction(machine)
    if instruction == -1 { 
        print_error("Found end of input before Halt.")
        return true
    }

    switch instruction {
    case int(Instruction.HALT):
        fmt.println("halting...")
        return true
    case int(Instruction.ADD):
        return do_bin_op(machine, "Add", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) + b.(int)
            case f64: return a.(f64) + b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
    case int(Instruction.SUB):
        return do_bin_op(machine, "Sub", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) - b.(int)
            case f64: return a.(f64) - b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
    case int(Instruction.MUL):
        return do_bin_op(machine, "Mul", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) * b.(int)
            case f64: return a.(f64) * b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
    case int(Instruction.DIV):
        return do_bin_op(machine, "Mul", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) / b.(int)
            case f64: return a.(f64) / b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })

    case int(Instruction.MOD):
        return do_bin_op(machine, "Mod", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) % b.(int)
            // case f64: return a.(f64) / b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })

    case int(Instruction.NOT):
        it := back(&machine.operands)
        if it.(int) == 0 {
            it^ = 1
        } else {
            it^ = 0
        }
        machine.ip += 1
        return false

    case int(Instruction.OR):
        return do_bin_op(machine, "Or", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) | b.(int)
            // case f64: return a.(f64) / b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
    case int(Instruction.AND):
        return do_bin_op(machine, "And", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) & b.(int)
            // case f64: return a.(f64) / b.(f64)
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
	case int(Instruction.EQ):
        return do_bin_op(machine, "EQ", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) == b.(int) ? 1 : 0
            case f64: return a.(f64) == b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
	case int(Instruction.NEQ):
        return do_bin_op(machine, "NEQ", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) != b.(int) ? 1 : 0
            case f64: return a.(f64) != b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
	case int(Instruction.LT):
        return do_bin_op(machine, "LT", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) < b.(int) ? 1 : 0
            case f64: return a.(f64) < b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
	case int(Instruction.LE):
        return do_bin_op(machine, "LE", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) <= b.(int) ? 1 : 0
            case f64: return a.(f64) <= b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
	case int(Instruction.GT):
        return do_bin_op(machine, "GT", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) > b.(int) ? 1 : 0
            case f64: return a.(f64) > b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })
	case int(Instruction.GE):
        return do_bin_op(machine, "GE", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) >= b.(int) ? 1 : 0
            case f64: return a.(f64) >= b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })

    case int(Instruction.INCR):
        it := back(&machine.operands)
        #partial switch _ in it {
        case int: 
            it^ = it.(int) + 1
            machine.ip += 1
            return false
        }

        print_error("INCR: %v is not an integer", it)
        return true

    case int(Instruction.DECR):
        it := back(&machine.operands)
        #partial switch _ in it {
        case int: 
            it^ = it.(int) - 1
            machine.ip += 1
            return false
        }

        print_error("INCR: %v is not an integer", it)
        return true

    case int(Instruction.LLOAD):
        machine.ip += 1
        index := machine.instructions[machine.ip]
        append(&machine.operands, back(&machine.frames).locals[index])
        machine.ip += 1
        return false

    case int(Instruction.LSTORE):
        machine.ip += 1
        index := machine.instructions[machine.ip]
        value := pop(&machine.operands)

        locals := &back(&machine.frames).locals
        if len(locals) <= index {
            // fmt.println(index, "=", value)
            resize(locals, index + 1)
        }
        locals[index] = value

        machine.ip += 1
        return false

    case int(Instruction.LLOAD_FROM):
        machine.ip += 1
        scope := machine.instructions[machine.ip]
        machine.ip += 1
        index := machine.instructions[machine.ip]
        locals := &machine.frames[len(machine.frames) - 1 - scope].locals
        append(&machine.operands, locals[index])
        machine.ip += 1
        return false

    case int(Instruction.LSTORE_FROM):
        machine.ip += 1
        scope := machine.instructions[machine.ip]
        machine.ip += 1
        index := machine.instructions[machine.ip]
        value := pop(&machine.operands)

        locals := &machine.frames[len(machine.frames) - 1 - scope].locals
        if len(locals) <= index {
            // fmt.println(index, "=", value)
            resize(locals, index + 1)
        }
        locals[index] = value

        machine.ip += 1
        return false

    case int(Instruction.BRK):
        target := pop_frame(machine)
        assert(target >= 0 && target < len(machine.instructions), "BRK: Invalid jump target")
        machine.ip = target
        return false

    case int(Instruction.BRK_IF):
        assert(len(machine.operands) > 0)
        condition := pop(&machine.operands)

        if condition.(int) != 0 { // Non-zero condition means jump
            target := pop_frame(machine)
            assert(target >= 0 && target < len(machine.instructions), "BRK_IF: Invalid jump target")

            machine.ip = target
        } else {
            machine.ip += 1
        }
        return false

    case int(Instruction.JMP):
        machine.ip += 1
        assert(machine.ip < len(machine.instructions))
        target := machine.instructions[machine.ip]
        assert(target >= 0 && target < len(machine.instructions), "JMP: Invalid jump target")
        machine.ip = target
        return false

    case int(Instruction.RJMP):
        machine.ip += 1
        assert(machine.ip < len(machine.instructions))
        target := machine.instructions[machine.ip] + machine.ip
        assert(target >= 0 && target < len(machine.instructions), "RJMP: Invalid jump target")
        machine.ip = target
        return false

    case int(Instruction.JMP_IF):
        assert(len(machine.operands) > 0)
        condition := pop(&machine.operands)

        machine.ip += 1
        assert(machine.ip < len(machine.instructions))
        target := machine.instructions[machine.ip]
        assert(target >= 0 && target < len(machine.instructions), fmt.aprintf("JMP_IF: Invalid jump target %v (not between 0-%v)", target, len(machine.instructions) - 1))

        if condition.(int) != 0 { // Non-zero condition means jump
            machine.ip = target
        } else {
            machine.ip += 1
        }
        return false

    case int(Instruction.RJMP_IF):
        assert(len(machine.operands) > 0)
        condition := pop(&machine.operands)

        machine.ip += 1
        assert(machine.ip < len(machine.instructions))
        target := machine.instructions[machine.ip] + machine.ip
        assert(target >= 0 && target < len(machine.instructions), fmt.aprintf("RJMP_IF: Invalid jump target %v (not between 0-%v)", target, len(machine.instructions) - 1))

        if condition.(int) != 0 { // Non-zero condition means jump
            machine.ip = target
        } else {
            machine.ip += 1
        }
        return false

    case int(Instruction.COPY):
        machine.ip += 1
        addr := machine.instructions[machine.ip]
        if addr < 0 {
            addr = len(machine.operands) + addr
        }
        if addr < 0 || addr >= len(machine.operands) {
            print_error("COPY: Invalid memory address %v", addr)
            return true
        }
        append(&machine.operands, machine.operands[addr])

        machine.ip += 1
        return false

    case int(Instruction.DELETE):
        machine.ip += 1
        addr := machine.instructions[machine.ip]
        if addr < 0 {
            addr = len(machine.operands) + addr
        }
        if addr < 0 || addr >= len(machine.operands) {
            print_error("DELETE: Invalid memory address %v", addr)
            return true
        }
        
        ordered_remove(&machine.operands, addr)

        machine.ip += 1
        return false

    case int(Instruction.CONST_INT):
        machine.ip += 1
        val := machine.instructions[machine.ip]
        append(&machine.operands, val)

        machine.ip += 1
        return false

    case int(Instruction.PUSH_FRAME):
        machine.ip += 1
        frame := Frame{machine.ip, [dynamic] Operand {}}
        append(&machine.frames, frame)
        return false

    case int(Instruction.POP_FRAME):
        machine.ip += 1
        pop(&machine.frames)
        return false

    case int(Instruction.GET_FIELD):
        machine.ip += 1
        index     := pop(&machine.operands)
        structure := pop(&machine.operands)
        append(&machine.operands, structure.(Structure).fields[index.(int)])

    case int(Instruction.SET_FIELD):
        machine.ip += 1
        value     := pop(&machine.operands)
        index     := pop(&machine.operands)
        structure := pop(&machine.operands)
        structure.(Structure).fields[index.(int)] = value

    case int(Instruction.STACK_TRACE):
        fmt.println("Stack Trace:", machine.operands)
        machine.ip += 1
        return false

    case int(Instruction.LOCALS_TRACE):
        assert(len(machine.frames) > 0, "Can not trace locals with no frames!")
        fmt.println("Locals Trace:", back(&machine.frames).locals)
        machine.ip += 1
        return false
    }

    print_error("Unkown instruction %v", instruction)
    return true
}

run :: proc(machine: ^Machine) {
    for {
        if step(machine) {
            break
        }
    }
}

for_loop_start :: proc(code: ^[dynamic] int, low: int) {
    append(code, opcode(.CONST_INT), low - 1) // because increment
    append(code, opcode(.PUSH_FRAME))
    append(code, opcode(.INCR))
}

for_loop_end :: proc(code: ^[dynamic] int, high: int) {
    append(code, 
        opcode(.COPY), -1,
        opcode(.CONST_INT), high,
        opcode(.LT),
        opcode(.BRK_IF),
    )
}

print_even :: proc(code: ^[dynamic] int) {
    // for i: 0..10 {
    //     if i % 2 == 0 {
    //         println(i);
    //     }
    // }
    for_loop_start(code, 0)
        append(code, 
            opcode(.COPY), -1,
            opcode(.CONST_INT), 2,
            opcode(.MOD),

            opcode(.RJMP_IF), 2,
            opcode(.STACK_TRACE)
        )
    for_loop_end(code, 10)
}

main :: proc() {
    machine: Machine

    code: [dynamic] int
    defer delete(code)

    print_even(&code)

    // for_loop_start(&code, 0)
    //     append(&code, opcode(.STACK_TRACE))
    // for_loop_end(&code, 10)

    append(&code, opcode(.HALT))
    machine.instructions = code[:]

    // machine.instructions = {
    //     opcode(.CONST_INT), 0,
    //     opcode(.INCR),
    //     opcode(.STACK_TRACE),
    //
    //     opcode(.COPY), -1,
    //     opcode(.CONST_INT), 10,
    //     opcode(.LT),
    //     opcode(.BRK_IF), 2,
    //
    //     opcode(.HALT),
    // }

    run(&machine)
}
