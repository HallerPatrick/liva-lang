use crate::value::Value;
use std::process::exit as _exit;

pub type NativeFn = fn(usize, Vec<Value>) -> Value;

pub fn clock(_arg_count: usize, _args: Vec<Value>) -> Value {
    Value::Double(1.0)
}

pub fn print(_arg_count: usize, _args: Vec<Value>) -> Value {
    // println!("{:?}", _args);
    let arg = &_args[0];
    print!("{}", arg);
    Value::Nil
}

pub fn println(_arg_count: usize, _args: Vec<Value>) -> Value {
    // println!("{:?}", _args);
    let arg = &_args[0];
    println!("{}", arg);
    Value::Nil
}

pub fn exit(_arg_count: usize, _args: Vec<Value>) -> Value {
    match _args[0] {
        Value::Double(num) => _exit(num as i32),
        _ => _exit(0),
    }
}

// TODO: This is incosistent as fuck, but part of development
// Better error handling!
// Good way of typoe conversion etc.
// Looks also pretty expensive...
pub fn range(_arg_count: usize, _args: Vec<Value>) -> Value {
    let args: Vec<Value> = _args.into_iter().rev().collect();
    let mut min = 0;
    let mut max = 0;

    if _arg_count == 1 {
        min = 0;
        if let Value::Double(m) = args[0] {
            max = m as i32;
        } else {
            panic!("Expected a number for `end`")
        }
    } else if _arg_count == 2 {
        if let Value::Double(mi) = args[0] {
            min = mi as i32;
        }

        if let Value::Double(ma) = args[1] {
            max = ma as i32;
        }
    } else {
        panic!("Expected 1 or 2 arguments for function `range(start, end)`")
    }

    let mut vec: Vec<Value> = Vec::new();

    for i in min..max as i32 {
        vec.push(Value::Double(i as f64));
    }

    Value::List(vec)
}
