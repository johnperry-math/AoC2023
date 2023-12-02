// Advent of Code 2023
//
// John Perry
//
// Day 1: Trebuchet?!
//
//  part 1: Find the first and last digits in each string
//
//  part 2: First the first and last numbers in each string
//          (either a digit or the digit's spelling)

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use aho_corasick::{AhoCorasick, MatchKind};
use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
};

// SECTION
// IO and Parts 1, 2

fn main() {
    let result = read_input();
    println!(
        "Sum of calibrated values is {}",
        result.naive.iter().sum::<u32>()
    );
    println!(
        "Sum of corrected values is {}",
        result.corrected.iter().sum::<u32>()
    );
}

#[derive(Default)]
struct CalibratedValues {
    naive: Vec<u32>,     // digits only
    corrected: Vec<u32>, // accepting spelling
}

// reads the input, parses naively using std lib,
// and parses more sophisticatedly using aho-corasick lib
fn read_input() -> CalibratedValues {
    let input =
        std::fs::File::open("input.txt").expect("wait, where's input.txt?!?");
    let reader = BufReader::new(input);

    // build these once (it's a LOT slower if you don't)
    let ac = aho_corasick::AhoCorasick::builder()
        .match_kind(MatchKind::Standard)
        .build(PATTERNS)
        .expect("unable to setup Aho-Corasick with desired patterns");
    let map: HashMap<String, u32> = PATTERNS
        .iter()
        .enumerate()
        .map(|(idx, value)| {
            (
                (*value).to_string(),
                u32::try_from(idx).expect("won't happen") + 1,
            )
        })
        .map(|(value, idx)| (value, idx.min((idx % 10) + 1)))
        .collect();

    // iterate through each line, parsing both naively and sophisticatedly
    let mut result = CalibratedValues::default();
    reader.lines().for_each(|line| {
        result.naive.push(naive_parse(
            line.as_ref().expect("wait, where's my line?!?"),
        ));
        result.corrected.push(corrected_parse(
            line.as_ref().expect("wait, where's my corrected line?!?"),
            &ac,
            &map,
        ));
    });
    result
}

// SECTION
// Part 1

// returns a two-digit number
// whose tens digit is the first digit to appear in line and
// whose ones digit is the last digit to appear in line
fn naive_parse(line: &str) -> u32 {
    let first_digit: u32 = line
        .chars()
        .find(char::is_ascii_digit)
        .expect("hello, no first digit?!?")
        .to_digit(10)
        .expect("how did we try to convert a non-digit into a digit?");
    let last_digit: u32 = line
        .chars()
        .rev()
        .find(char::is_ascii_digit)
        .expect("hello, no last digit?!?")
        .to_digit(10)
        .expect("how did we try to convert a non-digit into a digit?!?");
    // println!("{}{}", first_digit, last_digit);
    first_digit * 10 + last_digit
}

// SECTION
// Part 2

// the patterns we match in part two
const PATTERNS: &[&str] = &[
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "1", "2", "3", "4", "5", "6", "7", "8", "9",
];

// returns a two-digit number
// whose tens digit is the first digit, possibly spelled, to appear in line
// and whose ones digit is the last digit, possibly spelled, to appear in line
fn corrected_parse(
    line: &str,
    ac: &AhoCorasick,
    map: &HashMap<String, u32>,
) -> u32 {
    let mut matches = ac.find_overlapping_iter(line);
    let first_match = matches.next().expect("what?!? no first match?");
    let last_match = matches.last().map_or(first_match, |value| value);
    let first_digit = map[&line[first_match.start()..first_match.end()]];
    let last_digit = map[&line[last_match.start()..last_match.end()]];
    // println!("{}{}", first_digit, last_digit);
    first_digit * 10 + last_digit
}
