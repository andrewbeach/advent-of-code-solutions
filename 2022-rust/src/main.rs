use std::env;
use std::process;

mod day1;
mod util;

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = util::Config::build(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {err}");
        process::exit(1);
    });

    if let Err(e) = day1::part1(&config.file_path) {
        eprintln!("Application error: {e}");
        process::exit(1);
    }

    if let Err(e) = day1::part2(&config.file_path) {
        eprintln!("Application error: {e}");
        process::exit(1);
    }
}
