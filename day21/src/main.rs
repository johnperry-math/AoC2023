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
use std::ops::Range;

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
#[allow(clippy::cast_possible_wrap)]
const SIDE_RANGE: Range<isize> = 0..SIDE_LENGTH as isize;

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

fn part_1(map: &Map, start: Location) -> usize {
    let mut to_do: [Vec<Location>; PART_1_STEPS + 1] = std::array::from_fn(|_| Vec::new());
    to_do[0].push(start);
    let mut done = HashSet::new();

    println!("{}", to_do.len());
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
                    if SIDE_RANGE.contains(&row) && SIDE_RANGE.contains(&col) {
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

fn main() {
    let mut map = two_dimensional_map_io::read_input("input.txt".to_string(), &deserialize);
    let start = find_start(&mut map);
    println!(
        "on the {PART_1_STEPS}th step, the elf can visit exactly {} plots",
        part_1(&map, start)
    );
}
