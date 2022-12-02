use std::error::Error;
use std::fs;

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

pub fn part1(file_path: &str) -> Result<(), Box<dyn Error>> {
    println!("Day 1, Part 1:");
    let contents = fs::read_to_string(file_path)?;
    let calories_by_elf = parse_input(&contents);
    let result = calories_by_elf.iter().max();
    println!("{:?}", result);

    Ok(())
}

pub fn part2(file_path: &str) -> Result<(), Box<dyn Error>> {
    println!("Day 1, Part 2:");
    let contents = fs::read_to_string(file_path)?;
    let calories_by_elf = parse_input(&contents);
    let mut result_vec = calories_by_elf.clone();
    result_vec.sort_by(|a, b| b.cmp(a));
    let result: u32 = result_vec[0..3].iter().sum();
    println!("{:?}", result);

    Ok(())
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
