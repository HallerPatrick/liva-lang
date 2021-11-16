use liva_parser::parse_source;
use liva_parser::Span;
/// Run all liva source code examples from the examples directory.
use std::fs;
use std::path::PathBuf;

/// This test only checks if the parser does not crash
/// Not if the ast is correct
#[test]
fn run_all_examples() {
    let mut examples = Vec::new();
    for entry in fs::read_dir("examples").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            examples.push(path);
        }
    }
    let mut examples: Vec<&PathBuf> = examples
        .iter()
        .filter(|path| path.extension().is_some() && path.extension().unwrap() == "lv")
        .collect();
    examples.sort();

    println!("Running {} examples", examples.len());
    for example in examples {
        let source: String = fs::read_to_string(example.clone()).unwrap();
        println!("Testing example file: {}", example.display());
        let ast = parse_source(Span::new(source.as_str()));

        if !ast.is_ok() {
            println!("{:?}", ast);
        }

        assert_eq!(ast.is_ok(), true);
    }
}
