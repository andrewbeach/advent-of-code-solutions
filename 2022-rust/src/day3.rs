use std::fs;
use std::error::Error;
use crate::util::{self, Config};
use crate::char;

#[derive(Debug, PartialEq)]
struct Sack { 
    comp_one: Vec<char>,
    comp_two: Vec<char>,
}

impl Sack {
    fn build(input: &str) -> Sack {
        let (half_one, half_two) = input.split_at(input.len() / 2);
        let comp_one: Vec<char> = half_one.chars().collect();
        let comp_two: Vec<char> = half_two.chars().collect();
        Sack { comp_one, comp_two }
    }

    fn shared_item(&self) -> &char {
        self.comp_one
            .iter()
            .find(|c| self.comp_two.contains(&c))
            .unwrap_or_else(|| &' ')
    }

    fn shared_item_value(&self) -> u32 {
        let shared_item = self.shared_item();
        char::get_char_value(*shared_item)
    }
}

pub fn run(config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    let contents = fs::read_to_string(config.file_path)?;
    let mut part_one_result = 0;
    for line in contents.lines() {
        let sack = Sack::build(line);
        part_one_result += sack.shared_item_value();
    }
    
    // Part Two
    let part_two_result = None;
    
    let day = util::Day {
        day: 3,
        part1: Some(part_one_result),
        part2: part_two_result,
    };

    Ok(day)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_sack() {
        assert_eq!(
            Sack { 
                comp_one: vec!['t', 'e', 's', 't', 'c', 'o'],
                comp_two: vec!['n', 't', 'e', 'n', 't', 's'],
            },
            Sack::build("testcontents"),
        );
    }

    #[test]
    fn finds_shared_item_value() {
        assert_eq!(
            Sack::build("abcdefga").shared_item_value(),
            1,
        );

        assert_eq!(
            Sack::build("xyZazcdZ").shared_item_value(),
            52,
        );
    }
}
