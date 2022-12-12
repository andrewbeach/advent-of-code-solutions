use std::fs;
use std::error::Error;
use crate::util::{self, Config};

#[derive(Debug, PartialEq)]
struct Range {
    start: u32,
    end: u32,
}

impl Range {
    fn contains(&self, other_range: &Range) -> bool {
        (&self.start <= &other_range.start) & (&self.end >= &other_range.end)
    }

    fn overlaps(&self, other_range: &Range) -> bool {
        (&self.start <= &other_range.end) & (&self.end >= &other_range.start) 
    }

    fn parse_pair(input: &str) -> (Range, Range) {
        let inputs: Vec<&str> = input.split(',').collect();
        let input1: Vec<&str> = inputs[0].split('-').collect();
        let input2: Vec<&str> = inputs[1].split('-').collect();
        let start1: u32 = str::parse(input1[0]).unwrap();
        let end1: u32 = str::parse(input1[1]).unwrap();
        let start2: u32 = str::parse(input2[0]).unwrap();
        let end2: u32 = str::parse(input2[1]).unwrap();

        let range1 = Range { start: start1, end: end1 };
        let range2 = Range { start: start2, end: end2 };
        (range1, range2)
    }
}

pub fn run(config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    let contents = fs::read_to_string(config.file_path)?;
    let mut part_one_result = 0;
    for line in contents.lines() {
        let (range1, range2) = Range::parse_pair(line); 
        if range1.contains(&range2) | range2.contains(&range1) {
            part_one_result += 1;
        }
    }

    // Part Two
    let mut part_two_result = 0;
    for line in contents.lines() {
        let (range1, range2) = Range::parse_pair(line);
        if range1.overlaps(&range2) {
            part_two_result += 1;
        }
    }

    let day = util::Day {
        day: 4,
        part1: Some(part_one_result),
        part2: Some(part_two_result),
    };

    Ok(day)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_pair_from_input() {
        assert_eq!(
            Range::parse_pair("1-4,5-8"),
            (
                Range { start: 1, end: 4 },
                Range { start: 5, end: 8 },
            )
        ); 
    }

    #[test]
    fn checks_if_range_contains_another() {
        assert_eq!(
            Range { start: 2, end: 4 }.contains(&Range { start: 6, end: 8 }),
            false,
        );
        assert_eq!(
            Range { start: 5, end: 7 }.contains(&Range { start: 7, end: 9 }),
            false,
        );
        assert_eq!(
            Range { start: 2, end: 8 }.contains(&Range { start: 3, end: 7 }),
            true,
        );
        assert_eq!(
            Range { start: 4, end: 6 }.contains(&Range { start: 6, end: 6 }),
            true,
        );
    }

    #[test]
    fn checks_if_range_overlaps_another() {
        assert_eq!(            
            Range { start: 2, end: 4 }.overlaps(&Range { start: 5, end: 8 }),
            false,
        );

        assert_eq!(
            Range { start: 2, end: 4 }.overlaps(&Range { start: 4, end: 8 }), 
            true,
        );

        assert_eq!(
            Range { start: 4, end: 8 }.overlaps(&Range { start: 2, end: 4 }),
            true,
        );

        assert_eq!(
            Range { start: 4, end: 4 }.overlaps(&Range { start: 2, end: 6 }),
            true,
        );
    }
}
