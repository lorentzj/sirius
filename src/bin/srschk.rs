use std::fs;
use std::path::PathBuf;

use clap::Parser;

use sirius::parser::ParserOutput;
use sirius::solver::Solver;
use sirius::typechecker::check;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file
    file: PathBuf,

    /// Cache directory for z3 models
    #[arg(short, long)]
    cache_dir: Option<PathBuf>,
}

fn main() -> Result<(), std::io::Error> {
    let args = Args::parse();
    let code = fs::read_to_string(&args.file)?;
    let mut solver = Solver::new_cli(args.cache_dir).expect("z3 not found");

    let mut output = ParserOutput::parse(&code);
    let type_errors = check(&output, &mut solver);

    output.errors.extend(type_errors);

    if output.errors.is_empty() {
        println!("Ok")
    } else {
        for error in output.errors {
            let line = {
                if error.start > output.tokens.len() {
                    match output.tokens.last() {
                        Some(t) => t.line + 1,
                        None => 1,
                    }
                } else {
                    output.tokens[error.start].line + 1
                }
            };

            println!(
                "[Line {:5}] {}Error: {}",
                line, error.data.error_type, error.data.message
            )
        }
    }

    Ok(())
}
