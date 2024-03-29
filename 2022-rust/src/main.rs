use std::env;
use std::process;
use std::fmt::Display;

mod day1;
mod day2;
mod day3;
mod day4;
// mod day5;
mod day6;
mod day7;
mod util;
mod char;

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
        "3" => { handle_day_results(day3::run(config)) }
        "4" => { handle_day_results(day4::run(config)) }
        // "5" => { handle_day_results(day5::run(config)) }
        "6" => { handle_day_results(day6::run(config)) }
        "7" => { handle_day_results(day7::run(config)) }
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
