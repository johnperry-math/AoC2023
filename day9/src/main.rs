// Advent of Code 2023
//
// John Perry
//
// Day 9: Mirage Maintenance
//
// part 1: extend a sequence one unit to the right
//
// part 2: extend the same sequence one unit to the left
//
// this basically translates the Ada code; see that for details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::io::{BufRead, BufReader};

fn main() {
    let sequences = read_input();
    println!(
        "the sum of extrapolated values on right is {}",
        part_1(&sequences)
    );
    println!(
        "the sum of extrapolated values on left is {}",
        part_2(&sequences)
    );
}

fn read_input() -> Vec<Vec<i64>> {
    let mut result = Vec::new();
    let input = std::fs::File::open("input.txt").expect("eh... where's my input?!?");
    let lines = BufReader::new(input).lines();
    for line in lines {
        let text: String = line.expect("kind of weird to have a line that isn't Ok...");
        result.push(
            text.split_ascii_whitespace()
                .map(|number| number.parse::<i64>().expect("unable to parse {number}..."))
                .collect(),
        );
    }
    result
}

#[derive(PartialEq, Clone, Copy)]
enum Direction {
    Left,
    Right,
}

fn extend(sequence: &Vec<i64>, from: Direction) -> i64 {
    let differences: Vec<i64> = sequence
        .iter()
        .take(sequence.len() - 1)
        .zip(sequence.iter().skip(1).take(sequence.len() - 1))
        .map(|(prev, curr)| curr - prev)
        .collect();
    let last_difference = if differences.iter().any(|value| *value != 0) {
        extend(&differences, from)
    } else {
        0
    };
    if from == Direction::Left {
        sequence
            .first()
            .expect("why are we working with an empty sequence?!?")
            - last_difference
    } else {
        sequence
            .last()
            .expect("why are we working with an empty sequence?!?")
            + last_difference
    }
}

fn part_1(sequences: &[Vec<i64>]) -> i64 {
    sequences
        .iter()
        .map(|sequence| extend(sequence, Direction::Right))
        .sum()
}

fn part_2(sequences: &[Vec<i64>]) -> i64 {
    sequences
        .iter()
        .map(|sequence| extend(sequence, Direction::Left))
        .sum()
}
