mod chunk;
mod compiler;
mod debug;
mod gc;
mod native;
mod op_codes;
mod resolver;
mod value;
mod vm;

use liva_parser::print_ast;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;
use vm::{
    object::{LvObjectRef, ObjectStore, PrimitiveObjects},
    VMState,
};

use clap::{App, Arg};

use crate::vm::{ExecutionMode, VM};
use compiler::Compiler;

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    InterpretOK,
    InterpretCompileError,
    InterpretRuntimeError,
}

fn main() -> Result<(), String> {
    let matches = App::new("Liva Programming Language")
        .version("0.1.0")
        .author("Patrick Haller <patrickhaller40@googlemail.com>")
        .about("A language")
        .arg(Arg::with_name("FILE"))
        .arg(Arg::with_name("--debug").long("debug"))
        .arg(Arg::with_name("--ast").long("ast"))
        .arg(Arg::with_name("--stepper").long("stepper"))
        .get_matches();

    if let Some(file) = matches.value_of("FILE") {
        if matches.is_present("--ast") {
            print_ast(file);
        }
        run_file(
            file,
            matches.is_present("--debug"),
            matches.is_present("--stepper"),
        )
    }

    Ok(())
}

fn run_file(filename: &str, debug: bool, debug_stepper: bool) {
    let path = Path::new(&filename);
    let path_display = path.display();

    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(why) => {
            eprintln!("Failed to open {}: {}", path_display, why);
            exit(1);
        }
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Ok(_) => interpret(&s, debug, debug_stepper),
        Err(why) => {
            eprintln!("Failed to read {}: {}", path_display, why);
            exit(1);
        }
    };
}

/// The setup we do before calling the main code:
///
/// 1. Compile the code and turn it into a code object
/// 2. Init ObjectStore
/// 3. Init PrimitiveObjects
/// 4. Create a new VMState
/// 5. Pass state and main code object to `call_main_code`
///
pub fn interpret(source: &String, debug: bool, debug_stepper: bool) -> InterpretResult {
    let compiler = Compiler::new();
    let result = compiler.compile(source, debug);
    if result.is_none() {
        return InterpretResult::InterpretCompileError;
    }

    let result = result.unwrap();

    let mut store = ObjectStore::new();
    let primitve_objects = PrimitiveObjects::new(&mut store);

    match debug {
        true => VM::new(ExecutionMode::Trace, result, false, debug_stepper),
        false => VM::new(ExecutionMode::Default, result, false, false),
    }
    .run()
}

/// We do following steps:
/// 1. Load in builtins and run them with new call stack
/// 2. Load in user code and new call stack as main
/// 3. Run module
fn call_main_code(state: &mut VMState, module: LvObjectRef) {}
