use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;

use codecrafters_interpreter::*;

#[derive(Parser)]
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
    }
    Ok(())
}
