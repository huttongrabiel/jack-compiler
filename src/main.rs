use jack_compiler::syntax_analyzer;

fn main() {
    match syntax_analyzer::analyzer_main() {
        Ok(_) => (),
        Err(e) => println!("{}", e),
    };
}
