use std::fs;
use std::error::Error;
use crate::util::{self, Config};

fn are_elements_unique(vec: &Vec<char>) -> bool {
    let mut deduped = vec.clone();
    deduped.sort();
    deduped.dedup();
    deduped.len() == vec.len()
}

fn find_marker(n: usize, input: &str) -> usize {
    let mut last_n: Vec<char> = vec![];
    for (i, char) in input.chars().enumerate() {
        last_n.push(char);
        if last_n.len() >= n {
            if are_elements_unique(&last_n) {
                return i + 1
            }
            last_n.drain(0..1);
        }
    }
    0
}

fn find_packet_marker(input: &str) -> usize { find_marker(4, input) }
fn find_message_marker(input: &str) -> usize { find_marker(14, input) }

pub fn run(config: Config) -> Result<util::Day<usize, usize>, Box<dyn Error>> {
    // Part One
    let contents = fs::read_to_string(config.file_path)?;
    let part_one_result = find_packet_marker(&contents);

    // Part Two
    let part_two_result = find_message_marker(&contents);
    let day = util::Day {
        day: 6,
        part1: Some(part_one_result),
        part2: Some(part_two_result),
    };

    Ok(day)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_unique() {
        assert_eq!(
            are_elements_unique(&vec!['a', 'b', 'c', 'b']),
            false,
        );
    }

    #[test]
    fn finds_packet_marker() {
        assert_eq!(
            find_packet_marker("bvwbjplbgvbhsrlpgdmjqwftvncz"), 
            5,
        );
        assert_eq!(
            find_packet_marker("nppdvjthqldpwncqszvftbrmjlhg"), 
            6,
        );
        assert_eq!(
            find_packet_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 
            10,
        );
        assert_eq!(
            find_packet_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 
            11,
        );
    }

    #[test]
    fn finds_message_marker() {
        assert_eq!(
            find_message_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb"),
            19,
        );
        assert_eq!(
            find_message_marker("bvwbjplbgvbhsrlpgdmjqwftvncz"),
            23,
        );
        assert_eq!(
            find_message_marker("nppdvjthqldpwncqszvftbrmjlhg"),
            23,
        );
        assert_eq!(
            find_message_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"),
            29,
        );
        assert_eq!(
            find_message_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"),
            26,
        );
    }
}
