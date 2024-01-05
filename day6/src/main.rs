// Advent of Code 2023
//
// John Perry
//
// Day 6: Wait For It
//
// part 1: For how many times can you press the button on each boat
//         to guarantee a record-breaking win in the boat race?
//
// part 2: whoops! it's just 1 race with extraordinarily bad kerning
//
// this basically translates the Ada code; see that for details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::io::{BufRead, BufReader};

fn main() {
    let records = read_input();
    println!("you can beat the record in {} ways", part_1(&records));
    println!("no, it's actually {} ways", part_2(&records));
}

#[derive(Default, Clone, Copy)]
struct RaceRecord {
    race_time: u64,
    distance: u64,
}

fn read_input() -> [RaceRecord; 4] {
    let mut result = [RaceRecord::default(); 4];
    let input =
        std::fs::File::open("input.txt").expect("what, where's input.txt?!?");
    let reader = BufReader::new(input);
    let mut lines = reader.lines();

    let binding = lines
        .next()
        .expect("what, no race times?!?")
        .expect("why do i have to do this twice?");
    let mut race_times = binding.split_ascii_whitespace();
    let _ = race_times.next().expect("what, no label?!?");
    for race in &mut result {
        race.race_time = race_times
            .next()
            .expect("could not read {ith} time...")
            .parse()
            .expect("could not convert {ith} time to a string...");
    }

    let binding = lines
        .next()
        .expect("what, no distances?!?")
        .expect("why do i have to do this twice?");
    let mut race_times = binding.split_ascii_whitespace();
    let _ = race_times.next().expect("what, no label?!?");
    for race in &mut result {
        race.distance = race_times
            .next()
            .expect("could not read {ith} time...")
            .parse()
            .expect("could not convert {ith} time to a string...");
    }
    result
}

fn part_1(records: &[RaceRecord; 4]) -> u64 {
    let mut result = 1;

    #[allow(
        clippy::cast_precision_loss,
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        clippy::suboptimal_flops
    )]
    for race in records {
        let t_avail = race.race_time as f64;
        let d_beat = race.distance as f64;
        let mut lower = ((t_avail - (t_avail.powi(2) - 4. * d_beat).sqrt())
            / 2.)
            .round() as u64;
        let mut upper = ((t_avail + (t_avail.powi(2) - 4. * d_beat).sqrt())
            / 2.)
            .round() as u64;
        if (race.race_time - lower) * lower <= race.distance {
            lower += 1;
        }
        if (race.race_time - upper) * upper <= race.distance {
            upper -= 1;
        }
        result *= upper - lower + 1;
    }
    result
}

#[allow(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::suboptimal_flops
)]
fn part_2(records: &[RaceRecord; 4]) -> u64 {
    let t_avail: f64 = records.iter().rev().fold(0., |result, value| {
        result
            + (value.race_time * 10_u64.pow(result.log10().ceil() as u32))
                as f64
    });
    let d_beat: f64 = records.iter().rev().fold(0., |result, value| {
        result
            + (value.distance * 10_u64.pow(result.log10().ceil() as u32)) as f64
    });
    let mut lower = ((t_avail - (t_avail.powi(2) - 4. * d_beat).sqrt()) / 2.)
        .round() as u64;
    let mut upper = ((t_avail + (t_avail.powi(2) - 4. * d_beat).sqrt()) / 2.)
        .round() as u64;
    if (t_avail as u64 - lower) * lower <= d_beat as u64 {
        lower += 1;
    }
    if (t_avail as u64 - upper) * upper <= d_beat as u64 {
        upper -= 1;
    }
    upper - lower + 1
}
