use std::fs;
use std::error::Error;
use crate::util::{self, Config};

#[derive(Debug, PartialEq)]
struct Grid<'a> {
    stacks: Vec<Vec<&'a str>>,
}

impl Grid<'_> {
    fn parse(input: &str) -> Grid {
        // let mut stacks: Vec<Vec<&str>> = vec![vec![]; stack_count];
        for line in input.lines() {
            let mut chars = line.chars();
            let row = (0..)
                .map(|_| chars.by_ref().take(4).collect::<String>())
                .take_while(|s| !s.is_empty());

            for item in row {
                println!("{:?}", item);
            }
        }

        Grid {
            stacks: vec![vec![]],
        }
    }
}

#[derive(Debug, PartialEq)]
struct Move {
    count: u32,
    from: u32,
    to: u32,
}

impl Move { 
    fn parse(input: &str) -> Self {
        let words: Vec<&str> = input.split(' ').collect();
        Move {
            count: str::parse(&words[1]).unwrap(),
            from: str::parse(&words[3]).unwrap(),
            to: str::parse(&words[5]).unwrap(),
        }
    }
}

pub fn run(config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    // let contents = fs::read_to_string(config.file_path)?;

    // Part Two
    let day = util::Day {
        day: 5,
        part1: None,
        part2: None,
    };

    Ok(day)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_move() {
        assert_eq!(
            Move::parse("move 1 from 1 to 2"),
            Move { count: 1, from: 1, to: 2 },
        );

        assert_eq!(
            Move::parse("move 10 from 1 to 20"),
            Move { count: 10, from: 1, to: 20 },
        );
    }

    #[test]
    fn parses_grid() {
        let input = 
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3";
        assert_eq!(
            Grid::parse(input),
            Grid { 
                stacks: vec![
                    vec!["N", "Z"],
                    vec!["D", "C", "M"],
                    vec!["P"],
                ],
            }
        );
    }
}
