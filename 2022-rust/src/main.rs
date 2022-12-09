use std::env;
use std::process;
use std::fmt::Display;

mod day1;
mod day2;
mod util;

fn handle_day_results<
  T: Display, 
  U: Display,
  E: Display, 
>(result: Result<util::Day<T, U>, E>) {
    match result {
        Ok(day) => { 
            day.print_results();
        },
        Err(e) => { 
            eprintln!("Application error: {e}");
            process::exit(1);
         }         
    }
}

fn run_day(config: util::Config) {
    match config.day.as_str() {
        "1" => { handle_day_results(day1::run(config)) }
        "2" => { handle_day_results(day2::run(config)) }
        _ => { eprintln!("Day {} not solved yet", config.day) }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = util::Config::build(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {err}");
        process::exit(1);
    });

    run_day(config);
}
