// Advent of Code 2023
//
// John Perry
//
// Day 3: Gear Ratios
//
// part 1: determine which entries in the map are part numbers;
//         sum their values
//
// part 2: determine which locations in the map are gears;
//         sum their gear "ratios"

use std::io::{BufRead, BufReader};

// SECTION
// global types and variables
//
// I wanted to imitate the Ada, which uses an array,
// so I used an array here, too.
// I had a LOT of trouble making the range checks work.

// SUBSECTION
// indexing challenges
//
// Rust doesn't let me add a usize and -1;
// it also doesn't allow me to index arrays by anything other than usize.
// as a result, I had to denote the index as both i32 and as usize
const MAX_IDX: i32 = 140;
const MAX_IDX_USIZE: usize = MAX_IDX as usize;

// SUBSECTION
// type for part 1
// indicates whether a part has been counted already
type UsedArray = [[bool; MAX_IDX_USIZE]; MAX_IDX_USIZE];

// SUBSECTION
// types for part 2

// potential location of a part used with a gear
#[derive(PartialEq, Debug, Clone)]
struct Location {
    row: usize,
    col: usize,
}

// state machine for checking whether a symbol is a gear
#[derive(Clone)]
enum Locations {
    None,
    One { first: Location },
    Two { first: Location, second: Location },
    TooMany,
}

// SECTION
// Parts 1 and 2, embedded in our type

struct Schematic {
    schematic: [[char; MAX_IDX_USIZE]; MAX_IDX_USIZE],
}

// instead of defining a read_input() i defined a default()
// not sure that was the right idea, but it was worth doing once
impl Default for Schematic {

    fn default() -> Self {
        // set up input file
        // in a "real" program the filename would be an argument
        // to a function named "new"
        let input = std::fs::File::open("input.txt")
            .expect("what, where's input.txt?!?");
        let reader = BufReader::new(input);

        // now read things into the schematic
        // had i used vectors i wouldn't have to do this,
        // but i also wouldn't be able to count on having the correct sizes
        let mut schematic = [['.'; MAX_IDX_USIZE]; MAX_IDX_USIZE];
        reader.lines().enumerate().for_each(|(row, line_in)| {
            let line_in = line_in.expect("why don't we have line #{row}?!?");
            // ugh
            // i must be doing something wrong
            // i can't believe ada is easier on the eyes while doing this,
            // but maybe it is
            line_in
                .as_bytes()
                .iter()
                .enumerate()
                .for_each(|(col, value)| {
                    schematic[row][col] = char::from(*value)
                });
        });

        Self { schematic }
    }
}
    

impl Schematic {
    // SUBSECTION
    // Part 1

    // converts the string in the given row
    // and between the given column values
    // to its numerical value
    fn evaluate_over(&self, row: usize, left: usize, right: usize) -> u32 {
        self.schematic[row][left..right]
            .iter()
            .fold(0, |acc, value| {
                acc * 10
                    + value.to_digit(10).unwrap_or_else(|| {
                        panic!("{value} should have been a digit!")
                    })
            })
    }

    // similar to evaluate_over and evaluate_right_of,
    // but expands left and right from col to find bounds,
    // then calls evaluate_over
    // AS LONG AS used_locations says the resulting position is not yet used
    //
    // also updates used_locations
    fn expand_unused(
        &self,
        row: usize,
        col: usize,
        used_locations: &mut [[bool; MAX_IDX_USIZE]; MAX_IDX_USIZE],
    ) -> Option<u32> {
        let mut result = None;
        let (mut left, mut right) = (col, col);

        while left > 0 && self.schematic[row][left - 1].is_ascii_digit() {
            left -= 1;
        }

        while right < MAX_IDX_USIZE
            && self.schematic[row][right].is_ascii_digit()
        {
            right += 1;
        }

        if !used_locations[row][left] {
            used_locations[row][left] = true;
            result = Some(self.evaluate_over(row, left, right));
        }

        result
    }

    // returns the sum of parts neighboring the given row and col
    // AS LONG AS used_locations reports that they have not been used yet
    //
    // also updates used_locations
    fn unused_neighbors(
        &mut self,
        row: i32,
        col: i32,
        used_locations: &mut UsedArray,
    ) -> u32 {
        (-1..2)
            // only valid rows
            .filter(|offset| (0..MAX_IDX).contains(&(row + offset)))
            // sum...
            .fold(0, |acc, row_offset| {
                acc + (-1..2)
                    // only valid columns and numbers
                    .filter(|offset| {
                        (0..MAX_IDX).contains(&(col + offset))
                            && self.schematic[(row + row_offset) as usize]
                                [(col + offset) as usize]
                                .is_ascii_digit()
                    })
                    // sum...
                    .fold(0, |acc, col_offset| {
                        acc + self
                            .expand_unused(
                                (row + row_offset) as usize,
                                (col + col_offset) as usize,
                                used_locations,
                            )
                            .unwrap_or(0)
                    })
            })
    }

    pub fn part_1(&mut self, mut used_locations: UsedArray) -> u32 {
        let mut result = 0;
        for row in 0..MAX_IDX as u16 {
            for col in 0..MAX_IDX as u16 {
                let c = self.schematic[usize::from(row)][usize::from(col)];
                if !(c.is_ascii_digit() || c == '.') {
                    result += self.unused_neighbors(
                        i32::from(row),
                        i32::from(col),
                        &mut used_locations,
                    );
                }
            }
        }
        result
    }

    // similar to evaluate_over, but starts at left and proceeds right
    // until it can proceed no more
    fn evaluate_right_of(&self, row: usize, left: usize) -> u32 {
        self.schematic[row]
            .iter()
            .skip(left)
            .take_while(|c| c.is_ascii_digit())
            .fold(0, |acc, c| {
                acc * 10
                    + c.to_digit(10).unwrap_or_else(|| {
                        panic!("{c} should have been a digit!")
                    })
            })
    }

    // finds the leftmost numerical position
    // to the left of the indicated position
    fn find_left(&self, row: usize, col: usize) -> usize {
        let mut left = col;
        while left > 0 && self.schematic[row][left].is_ascii_digit() {
            left -= 1;
        }
        if !self.schematic[row][left].is_ascii_digit() {
            left += 1
        }
        left
    }

    // returns an appropriate value for whether the given location
    // has two adjacent parts
    //
    // i gave up trying to write this functionally but may try again later;
    // it already has some functional features
    fn two_adjacencies(&self, row: i32, col: i32) -> Locations {
        // guilty until proven innocent
        let mut result = Locations::None;
        for row_offset in
            (-1..2).filter(|offset| (0..MAX_IDX).contains(&(row + offset)))
        {
            for col_offset in
                (-1..2).filter(|offset| (0..MAX_IDX).contains(&(col + offset)))
            {
                if self.schematic[(row + row_offset) as usize]
                    [(col + col_offset) as usize]
                    .is_ascii_digit()
                {
                    let this_row = (row + row_offset) as usize;
                    let new_location = Location {
                        row: this_row,
                        col: self
                            .find_left(this_row, (col + col_offset) as usize),
                    };
                    // work the state machine
                    result = match result {
                        Locations::None => Locations::One {
                            first: new_location,
                        },
                        Locations::One { first } => {
                            if first != new_location {
                                Locations::Two {
                                    first,
                                    second: new_location,
                                }
                            } else {
                                Locations::One { first }
                            }
                        }
                        Locations::Two { first, second } => {
                            if first != new_location && second != new_location {
                                Locations::TooMany
                            } else {
                                Locations::Two { first, second }
                            }
                        }
                        Locations::TooMany => Locations::TooMany,
                    }
                }
            }
        }
        result
    }

    fn part_2(&self) -> u32 {
        (0..MAX_IDX_USIZE).fold(0, |acc, row| {
            acc + (0..MAX_IDX_USIZE)
                .filter(|col| self.schematic[row][*col] == '*')
                .fold(0, |acc, col| {
                    acc + match self.two_adjacencies(row as i32, col as i32) {
                        Locations::Two { first, second } => {
                            self.evaluate_right_of(first.row, first.col)
                                * self.evaluate_right_of(second.row, second.col)
                        }
                        _ => 0,
                    }
                })
        })
    }
}

fn main() {
    let mut schematic = Schematic::default();
    let used_locations = [[false; MAX_IDX_USIZE]; MAX_IDX_USIZE];
    println!(
        "sum of part numbers is {}",
        schematic.part_1(used_locations)
    );
    println!("sum of gear ratios is {}", schematic.part_2());
}
