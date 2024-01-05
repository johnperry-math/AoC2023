// Advent of Code 2023
//
// John Perry
//
// Day 12: Hot Springs
//
// part 1: how many ways could the recorded spring conditions
//         match the recorded sequences of damaged springs?
//
// part 2: repeat after quintupling each record's length
//
// this basically translates the Ada code; see that for details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
};

fn main() {
    let records = read_input();
    println!(
        "the number of possible arrangements is {}",
        part_1(&records)
    );
    println!("after unfolding, that number is {}", part_2(&records));
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Condition {
    Operational,
    Damaged,
    Unknown,
}

#[derive(Clone, Debug)]
struct ConditionRecord {
    springs: Vec<Condition>,
    contiguous: Vec<usize>,
}

fn read_input() -> Vec<ConditionRecord> {
    let mut condition_records = Vec::new();
    let file = std::fs::File::open("input.txt").expect("where's my input?");
    let lines = BufReader::new(file).lines();
    for line in lines {
        let mut new_conditions = Vec::new();
        let mut new_contiguous = Vec::new();
        let line = line.expect("how'd we read a line that's not ok?!?");
        let mut split = line.split([' ', ',']);
        let symbols = split.next().expect("how did we not read symbols?!?");
        for symbol in symbols.chars() {
            match symbol {
                '.' => new_conditions.push(Condition::Operational),
                '#' => new_conditions.push(Condition::Damaged),
                '?' => new_conditions.push(Condition::Unknown),
                _ => panic!("invalid character {symbol}"),
            }
        }
        for number in split {
            new_contiguous.push(number.parse::<usize>().unwrap_or_else(|_| {
                panic!("unable to convert {number} to an integer")
            }));
        }
        condition_records.push(ConditionRecord {
            springs: new_conditions,
            contiguous: new_contiguous,
        });
    }
    condition_records
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct DynamicState {
    remainder: Vec<usize>,
    first: usize,
}

fn first_fits_in_second(
    first: &Vec<Condition>,
    second: &Vec<Condition>,
    from: usize,
) -> bool {
    first.len() <= second.len()
        && first.iter().zip(second.iter().skip(from)).all(
            |(from_first, from_second)| {
                *from_first == *from_second
                    || *from_first == Condition::Unknown
                    || *from_second == Condition::Unknown
            },
        )
}

fn matching_slides(
    springs: &Vec<Condition>,
    state: DynamicState,
    cache: &mut HashMap<DynamicState, usize>,
) -> usize {
    if cache.contains_key(&state) {
        cache[&state]
    } else if state.remainder.is_empty() {
        if springs
            .iter()
            .skip(state.first)
            .all(|spring| *spring != Condition::Damaged)
        {
            cache.insert(state, 1);
            1
        } else {
            cache.insert(state, 0);
            0
        }
    } else {
        let my_length = state
            .remainder
            .first()
            .expect("tried to get first element of nonempty remainder...");
        let first = state.first;
        let last = springs.len();
        let mut remaining_min_len = 0;
        let mut remaining_lengths = Vec::new();

        for remainder in state.remainder.iter().skip(1) {
            remaining_min_len += 1 + *remainder;
            remaining_lengths.push(*remainder);
        }

        let my_last = last - remaining_min_len;

        if first + my_length - 1 > my_last {
            cache.insert(state, 0);
            return 0;
        }

        let mut matcher = vec![Condition::Unknown; my_last - first];
        matcher
            .iter_mut()
            .take(*my_length)
            .for_each(|x| *x = Condition::Damaged);

        let mut result = 0;

        for ith in first..=my_last - my_length {
            if first_fits_in_second(&matcher, springs, first) {
                if ith + my_length < springs.len()
                    && (springs[ith + my_length] == Condition::Operational
                        || springs[ith + my_length] == Condition::Unknown)
                {
                    result += matching_slides(
                        springs,
                        DynamicState {
                            remainder: remaining_lengths.clone(),
                            first: ith + my_length + 1,
                        },
                        cache,
                    );
                } else if remaining_lengths.is_empty()
                    && (ith + my_length >= springs.len()
                        || springs[ith + my_length] == Condition::Operational)
                {
                    result += 1;
                } else {
                }
            }

            if ith + my_length < my_last {
                matcher[ith - first + my_length] = Condition::Damaged;
                matcher[ith - first] = Condition::Operational;
            }
        }

        cache.insert(state, result);
        return result;
    }
}

fn part_1(records: &[ConditionRecord]) -> usize {
    let mut result = 0;
    for record in records {
        let mut cache = HashMap::new();
        let state = DynamicState {
            remainder: record.contiguous.clone(),
            first: 0,
        };
        let this_score = matching_slides(&record.springs, state, &mut cache);
        result += this_score;
    }
    result
}

fn part_2(records: &[ConditionRecord]) -> usize {
    let mut result = 0;
    let new_records: Vec<ConditionRecord> = records
        .iter()
        .map(|record| {
            let mut new_record = record.clone();
            for _ in 1..5 {
                new_record.springs.push(Condition::Unknown);
                new_record.springs.append(&mut record.springs.clone());
                new_record.contiguous.append(&mut record.contiguous.clone());
            }
            new_record
        })
        .collect();
    for record in new_records {
        let mut cache = HashMap::new();
        let state = DynamicState {
            remainder: record.contiguous.clone(),
            first: 0,
        };
        result += matching_slides(&record.springs, state, &mut cache);
    }
    result
}
