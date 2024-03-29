use std::error::Error;
use std::fs;

use crate::util::{self, Config};

fn parse_input(input: &str) -> Vec<u32> {
    let mut calories_by_elf: Vec<u32> = vec![];
    let mut current_elf_calories = 0;
    for line in input.lines() {
        if line == "" {
            calories_by_elf.push(current_elf_calories);
            current_elf_calories = 0;
        } else {
            let calories = line.parse::<u32>().unwrap();
            current_elf_calories += calories;
        }
    }
    if current_elf_calories > 0 { 
        calories_by_elf.push(current_elf_calories); 
    }
    calories_by_elf
}

pub fn run(config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    let contents = fs::read_to_string(config.file_path)?;
    let calories_by_elf = parse_input(&contents);
    let part_one_result = calories_by_elf.into_iter().max();

    // Part Two
    let calories_by_elf = parse_input(&contents);
    let mut part_two_result_vec = calories_by_elf.clone();
    part_two_result_vec.sort_by(|a, b| b.cmp(a));
    let part_two_result: u32 = part_two_result_vec[0..3].iter().sum();

    let day = util::Day {
        day: 1,
        part1: part_one_result,
        part2: Some(part_two_result),
    };

    Ok(day)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses() {
        let input = "\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";
        let result = parse_input(input);
        assert_eq!(
            vec![6000, 4000, 11_000, 24_000, 10000],
            result,
        );
    }
}
