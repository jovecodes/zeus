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

    INCR,
    DECR,

    // memory
    COPY, // (index)
    DELETE, // (index)

    // takes field index
    GET_FIELD, // [structure, index]
    SET_FIELD, // [structure, index, value]

    // constants
    CONST_INT, // (value)
    CONST_FLOAT, // (index of data in the machine 'float_data')
    // CONST_STRING, // takes index of data in the machine 'string_data' 

    // comparison
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,

    // absolute
    JMP, // jumps unconditionally
    JMP_IF, // [value] jumps if non-zero

    // relative
    RJMP,
    RJMP_IF,

    // temporary
    DEBUG_LOG, // [value]
}

Structure :: struct {
    fields: [] Operand
}

Operand :: union {
    int,
    f64,
    Structure,
}

StackMachine :: struct {
    operands: [dynamic] Operand,
    instructions: [] int,
    ip: int,

    float_data: [dynamic] f64,
    string_data: [dynamic] string,
}

opcode :: proc(instruction: Instruction) -> int {
    return int(instruction)
}

get_next_instruction :: proc(machine: ^StackMachine) -> int {
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
    machine: ^StackMachine, 
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

    append(&machine.operands, a, b)

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
step :: proc(machine: ^StackMachine) -> bool {
    instruction := get_next_instruction(machine)
    if instruction == -1 { 
        print_error("Found end of input before Halt.")
        return true
    }

    switch instruction {
    case int(Instruction.HALT):
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
	case int(Instruction.LTE):
        return do_bin_op(machine, "LTE", proc(a: Operand, b: Operand) -> Operand { 
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
	case int(Instruction.GTE):
        return do_bin_op(machine, "GTE", proc(a: Operand, b: Operand) -> Operand { 
            #partial switch _ in a {
            case int: return a.(int) >= b.(int) ? 1 : 0
            case f64: return a.(f64) >= b.(f64) ? 1 : 0
            }
            print_error("Can not do binary operation on %v and %v", a, b)
			return 0
        })

    case int(Instruction.INCR):
        it := &machine.operands[len(machine.operands) - 1]
        #partial switch _ in it {
        case int: 
            it^ = it.(int) + 1
            machine.ip += 1
            return false
        }

        print_error("INCR: %v is not an integer", it)
        return true

    case int(Instruction.DECR):
        it := &machine.operands[len(machine.operands) - 1]
        #partial switch _ in it {
        case int: 
            it^ = it.(int) - 1
            machine.ip += 1
            return false
        }

        print_error("INCR: %v is not an integer", it)
        return true

    case int(Instruction.JMP):
        machine.ip += 1
        if machine.ip >= len(machine.instructions) {
            print_error("JMP: Out of bounds")
            return true
        }
        target := machine.instructions[machine.ip]
        if target < 0 || target >= len(machine.instructions) {
            print_error("JMP: Invalid jump target %v", target)
            return true
        }
        machine.ip = target
        return false

    case int(Instruction.JMP_IF):
        if len(machine.operands) < 1 {
            print_error("JMP_IF: Stack underflow")
            return true
        }
        condition := pop(&machine.operands)

        // Validate condition operand type
        ok := false
        #partial switch _ in condition { case int: ok = true }
        if !ok {
            print_error("JMP_IF: Condition must be an int")
            return true
        }

        machine.ip += 1
        if machine.ip >= len(machine.instructions) {
            print_error("JMP_IF: Out of bounds")
            return true
        }
        target := machine.instructions[machine.ip]
        if target < 0 || target >= len(machine.instructions) {
            print_error("JMP_IF: Invalid jump target %v", target)
            return true
        }

        if condition.(int) != 0 { // Non-zero condition means jump
            machine.ip = target
        } else {
            machine.ip += 1
        }
        return false

    case int(Instruction.RJMP):
        machine.ip += 1
        if machine.ip >= len(machine.instructions) {
            print_error("RJMP: Out of bounds")
            return true
        }
        target := machine.instructions[machine.ip] + machine.ip
        if target < 0 || target >= len(machine.instructions) {
            print_error("RJMP: Invalid jump target %v", target)
            return true
        }
        machine.ip = target
        return false

    case int(Instruction.RJMP_IF):
        if len(machine.operands) < 1 {
            print_error("RJMP_IF: Stack underflow")
            return true
        }
        condition := pop(&machine.operands)

        // Validate condition operand type
        ok := false
        #partial switch _ in condition { case int: ok = true }
        if !ok {
            print_error("RJMP_IF: Condition must be an int")
            return true
        }

        machine.ip += 1
        if machine.ip >= len(machine.instructions) {
            print_error("RJMP_IF: Out of bounds")
            return true
        }
        target := machine.instructions[machine.ip] + machine.ip
        if target < 0 || target >= len(machine.instructions) {
            print_error("RJMP_IF: Invalid jump target %v", target)
            return true
        }

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

    case int(Instruction.DEBUG_LOG):
        assert(len(machine.operands) > 0, "Trying to debug value with empty stack.")
        op := machine.operands[len(machine.operands) - 1]
        fmt.println(op)

        machine.ip += 1
        return false
    }

    print_error("Unkown instruction %v", instruction)
    return true
}

run :: proc(machine: ^StackMachine) {
    for {
        if step(machine) {
            break
        }
    }
}

main :: proc() {
    machine: StackMachine
    machine.instructions = {
        opcode(.CONST_INT), 10,

        opcode(.DEBUG_LOG), 
        opcode(.DECR),
        opcode(.COPY), -1,
        opcode(.RJMP_IF), -5,

        opcode(.HALT),
    }

    run(&machine)
}
