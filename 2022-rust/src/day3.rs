use std::fs;
use std::error::Error;
use crate::util::{self, Config};
use crate::char;

#[derive(Debug, PartialEq)]
struct Sack<'a> { 
    comp_one: Vec<char>,
    comp_two: Vec<char>,
    input: &'a str,
}

impl<'a> Sack<'a> {
    fn build(input: &'a str) -> Sack<'a> {
        let (half_one, half_two) = input.split_at(input.len() / 2);
        let comp_one: Vec<char> = half_one.chars().collect();
        let comp_two: Vec<char> = half_two.chars().collect();
        Sack { comp_one, comp_two, input }
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

fn find_badge(sacks: Vec<Sack>) -> Option<char>{
    let first_sack = &sacks[0];
    let second_sack = &sacks[1];
    let third_sack = &sacks[2];
    for char in first_sack.input.chars() {
        if second_sack.input.contains(char) & third_sack.input.contains(char) {
            return Some(char);
        } 
    }
    None
}

fn find_badge_value(sacks: Vec<Sack>) -> Option<u32> {
    let badge = find_badge(sacks);
    badge.map(char::get_char_value)
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
    let mut group: Vec<Sack> = vec![];
    let mut part_two_result = 0;
    for sack_input in contents.lines() {
        group.push(Sack::build(sack_input));
        if group.len() == 3 {
            let badge_value = find_badge_value(group).unwrap_or_else(|| 0);
            part_two_result += badge_value;
            group = vec![];
        } 
    }
    
    let day = util::Day {
        day: 3,
        part1: Some(part_one_result),
        part2: Some(part_two_result),
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
                input: "testcontents",
            },
            Sack::build("testcontents"),
        );
    }

    #[test]
    fn finds_badge() {
        assert_eq!(
            find_badge(
                vec![
                    Sack::build("vJrwpWtwJgWrhcsFMMfFFhFp"),
                    Sack::build("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"),
                    Sack::build("PmmdzqPrVvPwwTWBwg"),
                ]
            ),
            Some('r'),
        );
    }

    #[test]
    fn finds_badge_value() {
        assert_eq!(
            find_badge_value(
                vec![
                    Sack::build("vJrwpWtwJgWrhcsFMMfFFhFp"),
                    Sack::build("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"),
                    Sack::build("PmmdzqPrVvPwwTWBwg"),
                ]
            ),
            Some(18),
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
