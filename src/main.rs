use clap::{Parser as ClapParser, Subcommand};
use std::fs;
use std::path::PathBuf;

use codecrafters_interpreter::*;

#[derive(ClapParser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// tokenize a file
    Tokenize {
        /// source file
        filename: PathBuf,
    },
    /// Parse a file
    Parse {
        /// source file
        filename: PathBuf,
    },
    /// Run a file
    Run {
        /// source file
        filename: PathBuf,
    },
}

fn main() -> miette::Result<()> {
    let args = Cli::parse();
    let mut any_err = false;
    match args.command {
        Commands::Tokenize { ref filename } => {
            let file_contents = fs::read_to_string(filename).map_err(|err| {
                miette::miette!(err).context(format!("Failed to read file {}", filename.display()))
            })?;

            if file_contents.is_empty() {
                println!("EOF  null");
                return Ok(());
            }
            let scanner = Scanner::new(&file_contents);
            for token in scanner {
                let token = match token {
                    Ok(token) => token,
                    Err(e) => {
                        eprintln!("{e:?}");
                        if let Some(unrecognized) = e.downcast_ref::<SingleTokenError>() {
                            any_err = true;
                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                unrecognized.line(),
                                unrecognized.token
                            );
                        }
                        if let Some(unrecognized) = e.downcast_ref::<StringTermError>() {
                            any_err = true;
                            eprintln!("[line {}] Error: Unterminated string.", unrecognized.line());
                        }
                        continue;
                    }
                };
                println!("{}", token);
            }
            println!("EOF  null");
            if any_err {
                std::process::exit(65);
            }
        }
        Commands::Parse { ref filename } => {
            let file_contents = fs::read_to_string(filename).map_err(|err| {
                miette::miette!(err).context(format!("Failed to read file {}", filename.display()))
            })?;

            if file_contents.is_empty() {
                println!("EOF  null");
                return Ok(());
            }
            let parser = Parser::new(&file_contents);
            match parser.parse_expr() {
                Ok(tree) => {
                    println!("{tree}");
                }
                Err(e) => {
                    eprintln!("{e:?}");
                    std::process::exit(65);
                }
            }
        }
        Commands::Run { ref filename } => {
            let file_contents = fs::read_to_string(filename).map_err(|err| {
                miette::miette!(err).context(format!("Failed to read file {}", filename.display()))
            })?;

            if file_contents.is_empty() {
                println!("EOF  null");
                return Ok(());
            }
            let parser = Parser::new(&file_contents);
            match parser.parse() {
                Ok(tree) => {
                    println!("{tree}");
                }
                Err(e) => {
                    eprintln!("{e:?}");
                    std::process::exit(65);
                }
            }
        }
    }
    Ok(())
}
