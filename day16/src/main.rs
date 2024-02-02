// Advent of Code 2023
//
// John Perry
//
// Day 16: The Floor Will Be Lava
//
// part 1: count the number of tiles energized by a beam of light
//
// part 2: find the maximum number of tiles that a beam might energize
//
// this basically translates the Ada code;
// see that for problem-specific details
//
// remarks on implementing Index trait appear below

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
// need the following to silence clippy's warning when converting i8 to usize
#![allow(clippy::cast_sign_loss)]

use std::{
    collections::VecDeque,
    io::{BufRead, BufReader},
    ops::{Index, IndexMut},
};

// i am NOT putting up with endless repeated qualifiers for these enum's
#[allow(clippy::enum_glob_use)]
use Direction::*;
#[allow(clippy::enum_glob_use)]
use Object::*;

fn main() {
    let facility = read_input();
    let initial = Photon {
        row: 0,
        col: 0,
        dir: East,
    };
    println!("the facility energizes {}", part_1(initial, &facility));
    println!(
        "the best configuration energizes {} tiles",
        part_2(&facility)
    );
}

#[derive(Clone, Copy)]
enum Object {
    Empty,
    Vertical,
    Horizontal,
    ForeMirror,
    BackMirror,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    North,
    South,
    East,
    West,
}

#[derive(Clone, Copy)]
struct Diff2Dim {
    d_row: i8,
    d_col: i8,
}

const SIDE_LENGTH: i8 = 110;

type FacilityType = [[Object; SIDE_LENGTH as usize]; SIDE_LENGTH as usize];
type EnergizedDir = [bool; 4]; // ugh; i'd much prefer [bool; len()]
                               // or something like that
type EnergizedType = [[EnergizedDir; SIDE_LENGTH as usize]; SIDE_LENGTH as usize];

// SECTION
// Impl Index for various types
//
// in Ada it's common and useful to index an array by an enumeration.
// the language itself makes it quite easy.
//
// in Rust it's not common, but it would be immensely useful, even though
// the language doesn't make it easy. i've had issues translating the Ada
// to Rust in part because of indexing issues.
//
// this first impl helps me do something akin to the Ada solutions'
//      Deltas : constant array (Direction) of Diff_2Dim :=
//        [North => (-1, 0), South => (1, 0), ...and so forth
// which in turn allows expressions like Deltas (North),
// which is useful, natural, problem-oriented, and type-safe.
// (see function Moved of day16.adb.)
//
// to do the same in Rust requires a somewhat uglier start,
// but in the end it works. (see function moved of main.rs.)
// it would be nice if there were a cleaner way to do it.

struct Deltas {}
const DELTAS: Deltas = Deltas {};

impl Index<Direction> for Deltas {
    type Output = Diff2Dim;

    fn index(&self, index: Direction) -> &Self::Output {
        match index {
            North => &Self::Output {
                d_row: -1,
                d_col: 0,
            },
            South => &Self::Output { d_row: 1, d_col: 0 },
            East => &Self::Output { d_row: 0, d_col: 1 },
            West => &Self::Output {
                d_row: 0,
                d_col: -1,
            },
        }
    }
}

// the next two impl's allow immutable and mutable indexing
// of an array type (aliased as EnergizedDir).
// in several places you'll see something triple-indexed, where
// the last index is [photon.dir] or possibly even a constant Direction.

impl Index<Direction> for EnergizedDir {
    type Output = bool;
    fn index(&self, index: Direction) -> &Self::Output {
        match index {
            North => &self[0],
            South => &self[1],
            East => &self[2],
            West => &self[3],
        }
    }
}

impl IndexMut<Direction> for EnergizedDir {
    fn index_mut(&mut self, index: Direction) -> &mut Self::Output {
        match index {
            North => &mut self[0],
            South => &mut self[1],
            East => &mut self[2],
            West => &mut self[3],
        }
    }
}

struct Photon {
    row: i8,
    col: i8,
    dir: Direction,
}

fn read_input() -> FacilityType {
    let mut facility = [[Empty; SIDE_LENGTH as usize]; SIDE_LENGTH as usize];
    let input = std::fs::File::open("input.txt").expect("can't work without input!");
    let lines = BufReader::new(input).lines();
    for (row, line) in lines.enumerate() {
        for (col, symbol) in line
            .expect("oddly, we have a line that doesn't become a string")
            .chars()
            .enumerate()
        {
            facility[row][col] = match symbol {
                '.' => Empty,
                '|' => Vertical,
                '-' => Horizontal,
                '/' => ForeMirror,
                '\\' => BackMirror,
                _ => panic!("invalid symbol {symbol}"),
            }
        }
    }
    facility
}

fn moved(photon: &mut Photon, energized: &EnergizedType) -> bool {
    let d_xy = DELTAS[photon.dir];
    let mut still_moving = false;
    if (0..SIDE_LENGTH).contains(&(photon.row + d_xy.d_row))
        && (0..SIDE_LENGTH).contains(&(photon.col + d_xy.d_col))
    {
        still_moving = true;
        photon.row += d_xy.d_row;
        photon.col += d_xy.d_col;
    }
    still_moving && !energized[photon.row as usize][photon.col as usize][photon.dir]
}

fn moved_foremirror(photon: &mut Photon, energized: &EnergizedType) -> bool {
    match photon.dir {
        North => photon.dir = East,
        South => photon.dir = West,
        East => photon.dir = North,
        West => photon.dir = South,
    }
    moved(photon, energized)
}

fn moved_backmirror(photon: &mut Photon, energized: &EnergizedType) -> bool {
    match photon.dir {
        North => photon.dir = West,
        South => photon.dir = East,
        East => photon.dir = South,
        West => photon.dir = North,
    }
    moved(photon, energized)
}

fn split_horizontal(photon: &Photon, energized: &EnergizedType, queue: &mut VecDeque<Photon>) {
    if (0..SIDE_LENGTH).contains(&(photon.col - 1))
        && !energized[photon.row as usize][photon.col as usize - 1][West]
    {
        queue.push_back(Photon {
            col: photon.col - 1,
            dir: West,
            ..*photon.clone()
        });
    }
    if (0..SIDE_LENGTH).contains(&(photon.col + 1))
        && !energized[photon.row as usize][photon.col as usize + 1][East]
    {
        queue.push_back(Photon {
            col: photon.col + 1,
            dir: East,
            ..*photon
        });
    }
}

fn split_vertical(photon: &Photon, energized: &EnergizedType, queue: &mut VecDeque<Photon>) {
    if (0..SIDE_LENGTH).contains(&(photon.row - 1))
        && !energized[photon.row as usize - 1][photon.col as usize][North]
    {
        queue.push_back(Photon {
            row: photon.row - 1,
            dir: North,
            ..*photon.clone()
        });
    }
    if (0..SIDE_LENGTH).contains(&(photon.row + 1))
        && !energized[photon.row as usize + 1][photon.col as usize][South]
    {
        queue.push_back(Photon {
            row: photon.row + 1,
            dir: South,
            ..*photon
        });
    }
}

fn part_1(initial: Photon, facility: &FacilityType) -> usize {
    let mut photons = VecDeque::new();
    photons.push_back(initial);
    let mut energized = [[[false; 4]; SIDE_LENGTH as usize]; SIDE_LENGTH as usize];
    let mut max_beams = 0;
    while !photons.is_empty() {
        max_beams = max_beams.max(photons.len());
        let mut photon = photons.pop_front().unwrap(); // already verified nonempty
        let (row, col) = (photon.row as usize, photon.col as usize);
        if !energized[photon.row as usize][photon.col as usize][photon.dir] {
            energized[photon.row as usize][photon.col as usize][photon.dir] = true;
            match facility[row][col] {
                Empty => {
                    if moved(&mut photon, &energized) {
                        photons.push_back(photon);
                    }
                }
                Horizontal => match photon.dir {
                    East | West => {
                        if moved(&mut photon, &energized) {
                            photons.push_back(photon);
                        }
                    }
                    North | South => {
                        split_horizontal(&photon, &energized, &mut photons);
                    }
                },
                Vertical => match photon.dir {
                    North | South => {
                        if moved(&mut photon, &energized) {
                            photons.push_back(photon);
                        }
                    }
                    East | West => {
                        split_vertical(&photon, &energized, &mut photons);
                    }
                },
                ForeMirror => {
                    if moved_foremirror(&mut photon, &energized) {
                        photons.push_back(photon);
                    }
                }
                BackMirror => {
                    if moved_backmirror(&mut photon, &energized) {
                        photons.push_back(photon);
                    }
                }
            }
        }
    }
    energized.iter().fold(0, |sum, row| {
        sum + {
            row.iter().fold(0, |sum, cell| {
                if cell.iter().any(|&x| x) {
                    sum + 1
                } else {
                    sum
                }
            })
        }
    })
}

fn part_2(facility: &FacilityType) -> usize {
    let mut result = 0;
    for row in 0..SIDE_LENGTH {
        result = result.max(part_1(
            Photon {
                row,
                col: 0,
                dir: East,
            },
            facility,
        ));
        result = result.max(part_1(
            Photon {
                row,
                col: SIDE_LENGTH - 1,
                dir: West,
            },
            facility,
        ));
    }
    for col in 0..SIDE_LENGTH {
        result = result.max(part_1(
            Photon {
                row: 0,
                col,
                dir: South,
            },
            facility,
        ));
        result = result.max(part_1(
            Photon {
                row: SIDE_LENGTH - 1,
                col,
                dir: North,
            },
            facility,
        ));
    }
    result
}
