use std::collections::HashMap;
use std::env;
use std::fs;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
    W,
    X,
    Y,
    Z,
}

impl Register {
    fn parse(data: &str) -> Result<Self, String> {
        match data {
            "w" => Ok(Register::W),
            "x" => Ok(Register::X),
            "y" => Ok(Register::Y),
            "z" => Ok(Register::Z),
            _ => Err(format!("invalid register")),
        }
    }
}

#[derive(Clone, Copy)]
enum Value {
    Load(Register),
    Literal(i64),
}

impl Value {
    fn parse(data: &str) -> Result<Self, String> {
        if let Ok(reg) = Register::parse(data) {
            Ok(Value::Load(reg))
        } else if let Ok(n) = data.parse::<i64>() {
            Ok(Value::Literal(n))
        } else {
            Err(format!("invalid value"))
        }
    }

    fn as_register(&self) -> Result<Register, String> {
        if let Value::Load(reg) = self {
            Ok(*reg)
        } else {
            Err(format!("value is not a register"))
        }
    }
}

enum Instruction {
    Inp(Register),
    Add(Register, Value),
    Mul(Register, Value),
    Div(Register, Value),
    Mod(Register, Value),
    Eql(Register, Value),
}

impl Instruction {
    fn parse(data: &str) -> Result<Self, String> {
        let parts: Vec<&str> = data.split(" ").collect();
        if parts.len() == 0 {
            return Err(format!("empty line"));
        }
        let instruction = parts[0];
        let operands: Result<Vec<_>, _> = parts[1..].iter().map(|s| Value::parse(s)).collect();
        let operands = operands?;

        macro_rules! unary {
            ($inst: expr) => {{
                if operands.len() != 1 {
                    return Err(format!("invalid argument count"));
                }
                $inst(operands[0].as_register()?)
            }};
        }
        macro_rules! binary {
            ($inst: expr) => {{
                if operands.len() != 2 {
                    return Err(format!("invalid argument count"));
                }
                $inst(operands[0].as_register()?, operands[1])
            }};
        }

        match instruction {
            "inp" => Ok(unary!(Instruction::Inp)),
            "add" => Ok(binary!(Instruction::Add)),
            "mul" => Ok(binary!(Instruction::Mul)),
            "div" => Ok(binary!(Instruction::Div)),
            "mod" => Ok(binary!(Instruction::Mod)),
            "eql" => Ok(binary!(Instruction::Eql)),
            _ => Err(format!("unknown instruction")),
        }
    }
}

struct Program {
    instructions: Vec<Instruction>,
}

impl Program {
    fn parse(data: &str) -> Result<Program, String> {
        let instructions: Result<Vec<_>, _> = data.lines().map(Instruction::parse).collect();
        let instructions = instructions?;
        Ok(Self { instructions })
    }
}

trait Executor {
    fn exec(&mut self, instruction: &Instruction) -> Result<(), String>;
}

fn run<E: Executor>(program: &Program, executor: &mut E) -> Result<(), String> {
    for instruction in &program.instructions {
        executor.exec(instruction)?;
    }
    Ok(())
}

struct ConcreteExecutor {
    w: i64,
    x: i64,
    y: i64,
    z: i64,
    input: Vec<u8>,
}

impl ConcreteExecutor {
    fn new(input: &[u8]) -> Self {
        Self {
            input: input.iter().rev().cloned().collect(),
            w: 0,
            x: 0,
            y: 0,
            z: 0,
        }
    }
}

impl Executor for ConcreteExecutor {
    fn exec(&mut self, instruction: &Instruction) -> Result<(), String> {
        macro_rules! apply {
            ($lhs:expr, $op:tt, $rhs:expr) => {
                self.write_reg($lhs, (self.read_reg($lhs) $op self.read_val($rhs)) as i64)
            }
        }

        match instruction {
            Instruction::Inp(reg) => {
                let input = self.input.pop().ok_or(format!("no more input to read"))?;
                self.write_reg(*reg, input as i64);
            }
            Instruction::Add(reg, val) => apply!(*reg, +, *val),
            Instruction::Mul(reg, val) => apply!(*reg, *, *val),
            Instruction::Div(reg, val) => apply!(*reg, /, *val),
            Instruction::Mod(reg, val) => apply!(*reg, %, *val),
            Instruction::Eql(reg, val) => apply!(*reg, ==, *val),
        }

        Ok(())
    }
}

impl ConcreteExecutor {
    fn read_reg(&self, reg: Register) -> i64 {
        match reg {
            Register::W => self.w,
            Register::X => self.x,
            Register::Y => self.y,
            Register::Z => self.z,
        }
    }

    fn write_reg(&mut self, reg: Register, value: i64) {
        match reg {
            Register::W => self.w = value,
            Register::X => self.x = value,
            Register::Y => self.y = value,
            Register::Z => self.z = value,
        }
    }

    fn read_val(&self, val: Value) -> i64 {
        match val {
            Value::Load(reg) => self.read_reg(reg),
            Value::Literal(n) => n,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct SymbolicRegister {
    reg: Register,
    idx: usize,
}

enum SymbolicValue {
    Load(SymbolicRegister),
    Literal(i64),
    Input(usize),
}

enum Expression {
    Unit(SymbolicValue),
    Add(SymbolicValue, SymbolicValue),
    Mul(SymbolicValue, SymbolicValue),
    Div(SymbolicValue, SymbolicValue),
    Mod(SymbolicValue, SymbolicValue),
    Eql(SymbolicValue, SymbolicValue),
}

struct SymbolicExecutor {
    w_idx: usize,
    x_idx: usize,
    y_idx: usize,
    z_idx: usize,
    input_idx: usize,
    registers: HashMap<SymbolicRegister, Expression>,
}

impl SymbolicExecutor {
    fn new() -> Self {
        let mut registers = HashMap::new();
        for reg in [Register::W, Register::X, Register::Y, Register::Z] {
            registers.insert(
                SymbolicRegister { reg: reg, idx: 0 },
                Expression::Unit(SymbolicValue::Literal(0)),
            );
        }

        Self {
            w_idx: 0,
            x_idx: 0,
            y_idx: 0,
            z_idx: 0,
            input_idx: 0,
            registers,
        }
    }
}

impl Executor for SymbolicExecutor {
    fn exec(&mut self, instruction: &Instruction) -> Result<(), String> {
        macro_rules! apply {
            ($lhs:expr, $op:expr, $rhs:expr) => {
                self.write_reg(
                    $lhs,
                    $op(
                        SymbolicValue::Load(self.read_reg($lhs)),
                        self.read_val($rhs),
                    ),
                )
            };
        }

        match instruction {
            Instruction::Inp(reg) => {
                let input = SymbolicValue::Input(self.input_idx);
                self.input_idx += 1;
                self.write_reg(*reg, Expression::Unit(input));
            }
            Instruction::Add(reg, val) => apply!(*reg, Expression::Add, *val),
            Instruction::Mul(reg, val) => apply!(*reg, Expression::Mul, *val),
            Instruction::Div(reg, val) => apply!(*reg, Expression::Div, *val),
            Instruction::Mod(reg, val) => apply!(*reg, Expression::Mod, *val),
            Instruction::Eql(reg, val) => apply!(*reg, Expression::Eql, *val),
        }

        Ok(())
    }
}

impl SymbolicExecutor {
    fn read_reg(&self, reg: Register) -> SymbolicRegister {
        let idx = match reg {
            Register::W => self.w_idx,
            Register::X => self.x_idx,
            Register::Y => self.y_idx,
            Register::Z => self.z_idx,
        };
        SymbolicRegister { reg, idx }
    }

    fn write_reg(&mut self, reg: Register, value: Expression) {
        let idx = match reg {
            Register::W => {
                self.w_idx += 1;
                self.w_idx
            }
            Register::X => {
                self.x_idx += 1;
                self.x_idx
            }
            Register::Y => {
                self.y_idx += 1;
                self.y_idx
            }
            Register::Z => {
                self.z_idx += 1;
                self.z_idx
            }
        };
        self.registers.insert(SymbolicRegister { reg, idx }, value);
    }

    fn read_val(&self, val: Value) -> SymbolicValue {
        match val {
            Value::Load(reg) => SymbolicValue::Load(self.read_reg(reg)),
            Value::Literal(n) => SymbolicValue::Literal(n),
        }
    }
}

trait SMT {
    fn smt(&self) -> String;
}

impl SMT for SymbolicRegister {
    fn smt(&self) -> String {
        let reg = match self.reg {
            Register::W => "w",
            Register::X => "x",
            Register::Y => "y",
            Register::Z => "z",
        };
        format!("{}{}", reg, self.idx)
    }
}

impl SMT for SymbolicValue {
    fn smt(&self) -> String {
        match self {
            SymbolicValue::Load(reg) => reg.smt(),
            SymbolicValue::Literal(n) => {
                let literal = format!("(_ bv{} 64)", n.abs());
                let literal = if *n < 0 {
                    format!("(bvneg {})", literal)
                } else {
                    literal
                };
                literal
            }
            SymbolicValue::Input(idx) => format!("i{}", idx),
        }
    }
}

impl SMT for Expression {
    fn smt(&self) -> String {
        match self {
            Expression::Unit(unit) => unit.smt(),
            Expression::Add(left, right) => format!("(bvadd {} {})", left.smt(), right.smt()),
            Expression::Mul(left, right) => format!("(bvmul {} {})", left.smt(), right.smt()),
            Expression::Div(left, right) => format!("(bvsdiv {} {})", left.smt(), right.smt()),
            Expression::Mod(left, right) => format!("(bvsmod {} {})", left.smt(), right.smt()),
            Expression::Eql(left, right) => format!(
                "(ite (= {} {}) {} {})",
                left.smt(),
                right.smt(),
                SymbolicValue::Literal(1).smt(),
                SymbolicValue::Literal(0).smt()
            ),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!(
            "usage: {} <command>",
            args.get(0).unwrap_or(&"solver".to_string())
        );
        return;
    }

    let program = match fs::read_to_string("program.txt") {
        Ok(program) => program,
        Err(_) => {
            println!("error: could not open program.txt");
            return;
        }
    };
    let program = match Program::parse(program.trim()) {
        Ok(program) => program,
        Err(e) => {
            println!("error: {}", e);
            return;
        }
    };

    let command = &args[1];
    let result = match command.as_str() {
        "search" => {
            if args.len() != 2 {
                println!("usage: {} search", args[0]);
                return;
            }
            search(program)
        }
        "check" => {
            if args.len() != 3 {
                println!("usage: {} check <target>", args[0]);
                return;
            }
            check(program, &args[2])
        }
        _ => {
            println!("error: invalid command");
            return;
        }
    };

    if let Err(e) = result {
        println!("error: {}", e);
    }
}

fn search(program: Program) -> Result<(), String> {
    let mut exec = SymbolicExecutor::new();
    run(&program, &mut exec)?;

    // symbolic declarations
    for idx in 0..exec.input_idx {
        let var = SymbolicValue::Input(idx);
        println!("(declare-fun {} () (_ BitVec 64))", var.smt());
    }
    for (reg, _) in &exec.registers {
        println!("(declare-fun {} () (_ BitVec 64))", reg.smt());
    }

    // our assertions
    for idx in 0..exec.input_idx {
        let var = SymbolicValue::Input(idx);
        println!(
            "(assert (bvsge {} {}))",
            var.smt(),
            SymbolicValue::Literal(1).smt()
        );
        println!(
            "(assert (bvsle {} {}))",
            var.smt(),
            SymbolicValue::Literal(9).smt()
        );
    }
    println!(
        "(assert (= {} {}))",
        exec.read_reg(Register::Z).smt(),
        SymbolicValue::Literal(0).smt()
    );
    for (reg, expr) in &exec.registers {
        println!("(assert (= {} {}))", reg.smt(), expr.smt());
    }

    println!(
        "(maximize (bvadd {}))",
        (0..exec.input_idx)
            .map(|idx| {
                let var = SymbolicValue::Input(idx);
                let factor = 10_i64.pow((exec.input_idx - idx) as u32);
                Expression::Mul(var, SymbolicValue::Literal(factor)).smt()
            })
            .collect::<Vec<_>>()
            .join(" ")
    );

    println!("(check-sat)");
    println!(
        "(get-value ({}))",
        (0..exec.input_idx)
            .map(|idx| SymbolicValue::Input(idx).smt())
            .collect::<Vec<_>>()
            .join(" ")
    );

    Ok(())
}

fn check(program: Program, target: &str) -> Result<(), String> {
    let target: Result<Vec<_>, _> = target
        .chars()
        .map(|ch| ch.to_digit(10).map(|n| n as u8).ok_or("invalid digit"))
        .collect();
    let target = target?;
    let mut exec = ConcreteExecutor::new(&target);

    run(&program, &mut exec)?;
    if exec.read_reg(Register::Z) == 0 {
        println!("valid")
    } else {
        println!("invalid")
    }

    Ok(())
}
