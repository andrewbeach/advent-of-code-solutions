// use std::fs;
use std::error::Error;
use crate::util::{self, Config};

pub fn run(_config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    // let contents = fs::read_to_string(config.file_path)?;

    // Part Two
    let day = util::Day {
        day: 4,
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
}
