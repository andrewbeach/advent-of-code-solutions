use std::fs;
use std::error::Error;
use crate::util::{self, Config};

#[derive(Debug, PartialEq)]
struct File {
    name: String,
    size: u32,
}

impl File {
    fn parse(input: &str) -> Self {
        let parts: Vec<&str> = input.split(' ').collect();
        File {
            name: parts[1].to_string(), 
            size: str::parse(&parts[0]).unwrap(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Command {
    ChangeDirectory(String),
    List,
}

impl Command { 
    fn parse(log: &str) -> Option<Self> {
        if log.starts_with("cd") {
            Some(Command::ChangeDirectory(String::from("/")))
        } else if log.starts_with("ls") {
            Some(Command::List)
        } else {
            None
        }
    }
}

fn parse_log(input: &str) -> Command | File {
    Command::List
}

pub fn run(config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    let contents = fs::read_to_string(config.file_path)?;

    // Part Two
    let day = util::Day {
        day: 7,
        part1: None,
        part2: None,
    };

    Ok(day)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_sack() {
        assert_eq!(1, 1);
    }

    #[test]
    fn parses_file() {
        assert_eq!(
            File::parse("123 test.rs"),
            File { name: "test.rs".to_string(), size: 123 },
        );
    }
}
