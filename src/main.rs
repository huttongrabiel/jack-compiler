use ansi_term::Color::Red;
use jack_compiler::syntax_analyzer;

fn main() {
    match syntax_analyzer::analyzer_main() {
        Ok(_) => (),
        Err(e) => println!("{} {}", Red.paint("Error:"), e),
    };
}
