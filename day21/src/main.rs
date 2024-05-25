//  Advent of Code 2023
//
//  John Perry
//
//  Day 21: Step Counter
//
//  part 1: an elf wants to know how many garden plots he would visit
//          after 64 steps
//
//  part 2: he actually meant 26501365 steps
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::collections::HashSet;

use common::two_dimensional_map::Location;
use common::two_dimensional_map_io;
use common::two_dimensional_motion::{delta, ALL_DIRECTIONS};

#[derive(PartialEq, Eq, Default, Clone, Copy)]
enum Object {
    #[default]
    Plot,
    Rock,
    Start,
}

fn deserialize(symbol: char) -> Object {
    match symbol {
        '.' => Object::Plot,
        '#' => Object::Rock,
        'S' => Object::Start,
        _ => panic!("Invalid symbol! {symbol}"),
    }
}

const SIDE_LENGTH: usize = 131;

type Map = common::two_dimensional_map::Map<SIDE_LENGTH, SIDE_LENGTH, Object>;

fn find_start(map: &mut Map) -> Location {
    for row in map.row_range() {
        for col in map.col_range() {
            if map[(row, col)] == Object::Start {
                map[(row, col)] = Object::Plot;
                return Location::new(row, col);
            }
        }
    }
    panic!("whoah! didn't find a start");
}

const PART_1_STEPS: usize = 64;

fn part_1(map: &Map, steps: usize, start: Location) -> usize {
    let mut to_do = Vec::new();
    for _ in 0..=steps {
        to_do.push(Vec::new());
    }
    to_do[0].push(start);
    let mut done = HashSet::new();

    for step in 0..to_do.len() {
        done.clear();
        if step < to_do.len() - 1 {
            let mut new_steps = Vec::new();
            for curr in &to_do[step] {
                for d in ALL_DIRECTIONS {
                    #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
                    let row = curr.row() as isize + isize::from(delta(d).d_row());
                    #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
                    let col = curr.col() as isize + isize::from(delta(d).d_col());
                    #[allow(clippy::cast_sign_loss)]
                    if map.row_range().contains(&(row as usize))
                        && map.col_range().contains(&(col as usize))
                    {
                        #[allow(clippy::cast_sign_loss)]
                        let next = Location::new(row as usize, col as usize);
                        #[allow(
                            clippy::cast_possible_truncation,
                            clippy::cast_possible_wrap,
                            clippy::cast_sign_loss
                        )]
                        if map[(row as usize, col as usize)] == Object::Plot
                            && !done.contains(&next)
                        {
                            new_steps.push(next);
                            done.insert(next);
                        }
                    }
                }
            }
            to_do[step + 1].append(&mut new_steps);
        } else {
            for curr in &to_do[step] {
                if !done.contains(curr) {
                    done.insert(*curr);
                }
            }
        }
    }

    done.len()
}

// too many lines from too many println's
#[allow(clippy::too_many_lines)]
fn part_2(map: &Map, number_of_steps: usize) -> usize {
    const CORNER_1_STEPS: usize = SIDE_LENGTH + SIDE_LENGTH / 2 - 1;
    const CORNER_2_STEPS: usize = SIDE_LENGTH / 2 - 1;

    let blocks_up = (number_of_steps - SIDE_LENGTH / 2) / SIDE_LENGTH;

    println!("Calculating progress; please wait a few seconds...");

    let from_top_ctr = part_1(
        map,
        SIDE_LENGTH - 1,
        Location::new(map.row_range().start, SIDE_LENGTH / 2),
    );
    let from_bot_ctr = part_1(
        map,
        SIDE_LENGTH - 1,
        Location::new(map.row_range().end - 1, SIDE_LENGTH / 2),
    );

    let from_right_mid = part_1(
        map,
        SIDE_LENGTH - 1,
        Location::new(SIDE_LENGTH / 2, map.col_range().end - 1),
    );
    let from_left_mid = part_1(
        map,
        SIDE_LENGTH - 1,
        Location::new(SIDE_LENGTH / 2, map.col_range().start),
    );

    let from_bot_right_1 = part_1(
        map,
        CORNER_1_STEPS,
        Location::new(map.row_range().end - 1, map.col_range().end - 1),
    );
    let from_bot_left_1 = part_1(
        map,
        CORNER_1_STEPS,
        Location::new(map.row_range().end - 1, map.col_range().start),
    );

    let from_top_right_1 = part_1(
        map,
        CORNER_1_STEPS,
        Location::new(map.row_range().start, map.col_range().end - 1),
    );
    let from_top_left_1 = part_1(
        map,
        CORNER_1_STEPS,
        Location::new(map.row_range().start, map.col_range().start),
    );

    let from_bot_right_2 = part_1(
        map,
        CORNER_2_STEPS,
        Location::new(map.row_range().end - 1, map.col_range().end - 1),
    );
    let from_bot_left_2 = part_1(
        map,
        CORNER_2_STEPS,
        Location::new(map.row_range().end - 1, map.col_range().start),
    );

    let from_top_right_2 = part_1(
        map,
        CORNER_2_STEPS,
        Location::new(map.row_range().start, map.col_range().end - 1),
    );
    let from_top_left_2 = part_1(
        map,
        CORNER_2_STEPS,
        Location::new(map.row_range().start, map.col_range().start),
    );

    let full_1 = part_1(
        map,
        SIDE_LENGTH + SIDE_LENGTH / 2,
        Location::new(map.row_range().end - 1, (map.col_range().end - 1) / 2 - 1),
    );
    let full_2 = part_1(
        map,
        SIDE_LENGTH + SIDE_LENGTH / 2 + 1,
        Location::new(map.row_range().end - 1, (map.col_range().end - 1) / 2 - 1),
    );

    println!(
        "visitation pattern every {} steps after the {}th",
        SIDE_LENGTH * 2,
        SIDE_LENGTH / 2
    );
    println!("     {from_bot_right_2:5}{from_bot_ctr:5}{from_bot_left_2:5}");
    println!(
        "{from_bot_right_2:5}{from_bot_right_1:5}{full_1:5}{from_bot_left_1:5}{from_bot_left_2:5}"
    );
    println!("{from_left_mid:5}{full_1:5}{full_2:5}{full_1:5}{from_right_mid:5}");
    println!(
        "{from_top_right_2:5}{from_top_right_1:5}{full_1:5}{from_top_left_1:5}{from_top_left_2:5}"
    );
    println!("     {from_top_right_2:5}{from_top_ctr:5}{from_top_left_2:5}");

    let mut result = 4 * (blocks_up / 2).pow(2) * full_1;
    result += (4 * blocks_up / 2 * (blocks_up / 2 - 1) + 1) * full_2;
    result += from_bot_ctr;
    result += from_top_ctr;
    result += from_left_mid;
    result += from_right_mid;
    result += from_bot_right_2 * blocks_up;
    result += from_top_right_2 * blocks_up;
    result += from_top_left_2 * blocks_up;
    result += from_bot_left_2 * blocks_up;
    result += from_bot_right_1 * (blocks_up - 1);
    result += from_top_right_1 * (blocks_up - 1);
    result += from_top_left_1 * (blocks_up - 1);
    result += from_bot_left_1 * (blocks_up - 1);

    result
}

fn main() {
    let mut map = two_dimensional_map_io::read_input("input.txt".to_string(), &deserialize);
    let start = find_start(&mut map);
    println!(
        "on the {PART_1_STEPS}th step, the elf can visit exactly {} plots",
        part_1(&map, PART_1_STEPS, start)
    );
    println!("{}", part_2(&map, 26_501_365));
}
