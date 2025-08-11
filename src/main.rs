use std::{collections::HashMap, hash::Hash};

use iced_x86::{Code, Decoder, DecoderOptions, Formatter, Instruction, NasmFormatter, OpKind, Register};

#[derive(Clone)]
enum Expression{
    UnknownRegisterValue(Register, u64),
    Expression(Box<Expression>),
    Value(i64),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>)
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::UnknownRegisterValue(register, ip) => f.write_fmt(format_args!("{register:?}")),
            Expression::Expression(expression) => f.write_fmt(format_args!("{expression:?}")),
            Expression::Value(v) => if *v > 0xffff { f.write_fmt(format_args!("{v:x}")) } else { f.write_fmt(format_args!("{v}")) },
            Expression::Add(l, r) => f.write_fmt(format_args!("{l:?} + {r:?}")),
            Expression::Sub(l, r) => f.write_fmt(format_args!("{l:?} - {r:?}")),
            Expression::Multiply(l, r) => f.write_fmt(format_args!("{l:?}*{r:?}")),
        }
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::UnknownRegisterValue(l0, ip0), Self::UnknownRegisterValue(r0, ip1)) => l0 == r0 && ip0 == ip1,
            (Self::Expression(l0), Self::Expression(r0)) => l0 == r0,
            (Self::Value(l0), Self::Value(r0)) => l0 == r0,
            (Self::Add(l0, l1), Self::Add(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Sub(l0, l1), Self::Sub(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Multiply(l0, l1), Self::Multiply(r0, r1)) => l0 == r0 && l1 == r1,
            _ => false,
        }
    }
}

enum MemoryStateKind{
    SameAs(u64),
    Expression(Expression)
}

struct MemoryState{
    registers:HashMap<Register, MemoryStateKind>
}

struct Memory {
    mem:HashMap<u64, MemoryState>
}

impl MemoryState {
    pub fn new() -> Self {
        Self { registers: HashMap::new() }
    }

    pub fn get_register_state(&mut self, reg:Register, ip:u64) -> Expression {
        self.registers.entry((reg, ip)).or_insert(Expression::UnknownRegisterValue(reg, ip)).clone()
    }
}

fn op_to_expression(state:&mut MemoryState, instr:&Instruction, op_index:u8, ) -> Expression {
    let mut kind = OpKind::default();
    let mut reg = Register::default();
    match op_index {
        0 => {
            kind = instr.op0_kind();
            reg = instr.op0_register();
        },
        1 => {
            kind = instr.op1_kind();
            reg = instr.op1_register();
        },
        _ => panic!("Unsupported op count!")
    }
    match kind {
        OpKind::Register => state.get_register_state(reg, instr.ip()),
        OpKind::Memory => {
            let mut base = state.get_register_state(instr.memory_base(), instr.ip());

            let displacement = match instr.memory_displ_size() {
                4 => Expression::Value(instr.memory_displacement32() as i64),
                8 | 1 => Expression::Value(instr.memory_displacement64() as i64),
                n => todo!("Unexpected memory displacement size {n}.")
            };
            
            if instr.memory_base().is_ip() {
                displacement
            } else {
                if instr.memory_index() != Register::None {
                    let scale_reg = state.get_register_state(instr.memory_index(), instr.ip());
                    let scale_size = Expression::Value(instr.memory_index_scale() as i64);
                    let index = Expression::Multiply(Box::new(scale_reg), Box::new(scale_size));
                    base = Expression::Add(Box::new(base), Box::new(index));
                }
                Expression::Add(Box::new(base), Box::new(displacement))
            }
        },
        OpKind::Immediate32to64 => {
            Expression::Value(instr.immediate64() as i64)
        }
        _ => todo!("Unexpected op0 kind: {kind:?}")
    }
}

pub(crate) fn main() {
    let bytes = EXAMPLE_CODE;
    let mut decoder =
        Decoder::with_ip(EXAMPLE_CODE_BITNESS, bytes, EXAMPLE_CODE_RIP, DecoderOptions::NONE);

    // Formatters: Masm*, Nasm*, Gas* (AT&T) and Intel* (XED).
    // For fastest code, see `SpecializedFormatter` which is ~3.3x faster. Use it if formatting
    // speed is more important than being able to re-assemble formatted instructions.
    let mut formatter = NasmFormatter::new();

    // Change some options, there are many more
    formatter.options_mut().set_digit_separator("`");
    formatter.options_mut().set_first_operand_char_index(10);

    // String implements FormatterOutput
    let mut output = String::new();

    // Initialize this outside the loop because decode_out() writes to every field
    let mut instruction = Instruction::default();

    let mut state = MemoryState::new();
    while decoder.can_decode() {
        // There's also a decode() method that returns an instruction but that also
        // means it copies an instruction (40 bytes):
        //     instruction = decoder.decode();
        decoder.decode_out(&mut instruction);

        // Format the instruction ("disassemble" it)
        output.clear();
        formatter.format(&instruction, &mut output);

        // Eg. "00007FFAC46ACDB2 488DAC2400FFFFFF     lea       rbp,[rsp-100h]"
        print!("{:016X} ", instruction.ip());
        let start_index = (instruction.ip() - EXAMPLE_CODE_RIP) as usize;
        let instr_bytes = &bytes[start_index..start_index + instruction.len()];
        for b in instr_bytes.iter() {
            print!("{:02X}", b);
        }
        if instr_bytes.len() < HEXBYTES_COLUMN_BYTE_LENGTH {
            for _ in 0..HEXBYTES_COLUMN_BYTE_LENGTH - instr_bytes.len() {
                print!("  ");
            }
        }
        print!(" {output}\t;{:?}\t", instruction.code());
        match instruction.code() {
            Code::Mov_rm64_r64 | Code::Mov_r64_rm64 => {
                let left = op_to_expression(&mut state, &instruction, 0);
                let right = op_to_expression(&mut state,&instruction, 1);
                print!("{left:?} = {right:?}")
            },
            Code::Push_r64 => {
                let stack = state.get_register_state(Register::ESP, instruction.ip());
                let left = Expression::Sub(Box::new(stack), Box::new(Expression::Value(4)));
                let right = op_to_expression(&mut state,&instruction, 0);
                print!("{left:?} = {right:?}")
            },
            Code::Lea_r64_m => {
                let left = op_to_expression(&mut state,&instruction, 0);
                let right = op_to_expression(&mut state,&instruction, 1);
                print!("{left:?} = {right:?}")
            }
            Code::Sub_rm64_imm32 => {
                let left = op_to_expression(&mut state,&instruction, 0);
                let right = op_to_expression(&mut state,&instruction, 1);
                print!("{left:?} -= {right:?}")
            },
            Code::Xor_r64_rm64 | Code::Xor_r32_rm32 => {
                let left = op_to_expression(&mut state,&instruction, 0);
                let right = op_to_expression(&mut state,&instruction, 1);
                if left != right {
                    print!("{left:?} ^= {right:?}")
                } else {
                    print!("{left:?} = 0");
                }
            }
            _ => (),
        }
        println!()
    }
}

const HEXBYTES_COLUMN_BYTE_LENGTH: usize = 10;
const EXAMPLE_CODE_BITNESS: u32 = 64;
const EXAMPLE_CODE_RIP: u64 = 0x0000_7FFA_C46A_CDA4;
static EXAMPLE_CODE: &[u8] = &[
    0x48, 0x89, 0x5C, 0x24, 0x10, 0x48, 0x89, 0x74, 0x24, 0x18, 0x55, 0x57, 0x41, 0x56, 0x48, 0x8D,
    0xAC, 0x24, 0x00, 0xFF, 0xFF, 0xFF, 0x48, 0x81, 0xEC, 0x00, 0x02, 0x00, 0x00, 0x48, 0x8B, 0x05,
    0x18, 0x57, 0x0A, 0x00, 0x48, 0x33, 0xC4, 0x48, 0x89, 0x85, 0xF0, 0x00, 0x00, 0x00, 0x4C, 0x8B,
    0x05, 0x2F, 0x24, 0x0A, 0x00, 0x48, 0x8D, 0x05, 0x78, 0x7C, 0x04, 0x00, 0x33, 0xFF,
];