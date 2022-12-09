use std::fmt::Display;

pub struct Config { 
    pub file_path: String,
    pub day: String,
}

impl Config {
    pub fn build(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("not enough arguments");
        }

        let day = args[1].clone();
        let file_path = format!("data/day{}.txt", day);

        Ok(Config { day, file_path })
    }
}

pub struct Day<T, U> {
    pub day: i32,
    pub part1: Option<T>,
    pub part2: Option<U>,
}

impl<T: Display, U: Display> Day<T, U> {
    pub fn print_results(&self) {
        println!("-----------------");
        println!("| Day {}, Part 1 |", self.day);
        println!("-----------------");
        match &self.part1 {
            Some(result) => println!("{}", result),
            None => println!("WIP"),
        }

        println!("              ");
        println!("-----------------");
        println!("| Day {}, Part 2 |", self.day);
        println!("-----------------");
        match &self.part2 {
            Some(result) => println!("{}", result),
            None => println!("WIP"),
        }
        println!("              ");
    }
}
