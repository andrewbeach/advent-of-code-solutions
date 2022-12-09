use std::fs;
use std::error::Error;
use crate::util::{self, Config};

#[derive(Debug, PartialEq)]
enum Outcome {
    Win, 
    Loss, 
    Draw,
}

#[derive(Clone, Debug, PartialEq)]
enum Throw {
    Rock,
    Paper,
    Scissors,
}

#[derive(Debug, PartialEq)]
struct Round {
    player: Throw,
    opponent: Throw,
}

impl Round {
    fn score(&self) -> u32 {
        let throw_score = match self.player {
            Throw::Rock => 1, 
            Throw::Paper => 2,
            Throw::Scissors => 3,
        };

        let round_score = {
            if self.player == self.opponent { 
                3
            } else {
                match (&self.player, &self.opponent) {
                    (Throw::Paper, Throw::Rock) => 6,
                    (Throw::Scissors, Throw::Paper) => 6,
                    (Throw::Rock, Throw::Scissors) => 6,
                    _ => 0,
                }
            }
        };

        throw_score + round_score
    }
}

fn choose_throw_for_outcome(opponent: Throw, outcome: Outcome) -> Throw {
    match outcome {
        Outcome::Draw => opponent,
        Outcome::Win => {
            match opponent {
                Throw::Rock => Throw::Paper,
                Throw::Paper => Throw::Scissors,
                Throw::Scissors => Throw::Rock,
            }
        },
        Outcome::Loss => {
            match opponent {
                Throw::Rock => Throw::Scissors,
                Throw::Paper => Throw::Rock, 
                Throw::Scissors => Throw::Paper,
            }
        },
    }
}

fn parse_round(round: &str, choose_throw: bool) -> Option<Round> {
    let throws: Vec<&str> = round.split(" ").collect();
    if throws.len() != 2 { 
        return None
    }

    let them: Option<Throw> = match throws[0] {
        "A" => Some(Throw::Rock),
        "B" => Some(Throw::Paper), 
        "C" => Some(Throw::Scissors),
        _ => None,
    };

    let us: Option<Throw> = {
        if choose_throw {
            match throws[1] {
                "X" => them.clone().map(|them| choose_throw_for_outcome(them, Outcome::Loss)),
                "Y" => them.clone().map(|them| choose_throw_for_outcome(them, Outcome::Draw)),
                "Z" => them.clone().map(|them| choose_throw_for_outcome(them, Outcome::Win)),
                _ => None,
            }
        } else {
            match throws[1] {
                "X" => Some(Throw::Rock),
                "Y" => Some(Throw::Paper), 
                "Z" => Some(Throw::Scissors),
                _ => None,
            }
        }
    };

    match (them, us) {
        (Some(them), Some(us)) => Some(Round { player: us, opponent: them }),
        _ => None,
    }
}

fn parse_rounds(input: &str, choose_throw: bool) -> Option<Vec<Round>> {
    let mut rounds = vec![];
    for line in input.lines() {
        let round = parse_round(line, choose_throw);
        rounds.push(round);
    }
    rounds.into_iter().collect()
}

fn score_rounds(rounds: Vec<Round>) -> u32 {
    let mut total_score = 0;
    for round in rounds {
        total_score += round.score();
    }
    total_score
}

pub fn run(config: Config) -> Result<util::Day<u32, u32>, Box<dyn Error>> {
    // Part One
    let contents = fs::read_to_string(config.file_path)?;
    let rounds = parse_rounds(&contents, false).unwrap();
    let part_one_result = score_rounds(rounds);

    // Part Two
    let rounds = parse_rounds(&contents, true).unwrap();
    let part_two_result = score_rounds(rounds);

    Ok(
        util::Day {
            day: 2,
            part1: Some(part_one_result),
            part2: Some(part_two_result),
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_rounds() {
        let input = "\
A Z
B Y
C X
A X
";
        assert_eq!(
            Some(vec![
                Round { opponent: Throw::Rock, player: Throw::Scissors },
                Round { opponent: Throw::Paper, player: Throw::Paper },
                Round { opponent: Throw::Scissors, player: Throw::Rock },
                Round { opponent: Throw::Rock, player: Throw::Rock },
            ]),
            parse_rounds(input, false),
        );
    }

    #[test]
    fn parses_rounds_with_desired_outcome() {
        let input = "\
A Y
B X
C Z
";

        assert_eq!(
            Some(vec![
                Round { opponent: Throw::Rock, player: Throw::Rock },
                Round { opponent: Throw::Paper, player: Throw::Rock },
                Round { opponent: Throw::Scissors, player: Throw::Rock },
            ]),
            parse_rounds(input, true),
        );
    }

    #[test]
    fn scores_rounds() {
        assert_eq!(
            15, 
            score_rounds(vec![
                Round { opponent: Throw::Rock, player: Throw::Paper },
                Round { opponent: Throw::Paper, player: Throw::Rock },
                Round { opponent: Throw::Scissors, player: Throw::Scissors },
            ]),
        );
    }
}
