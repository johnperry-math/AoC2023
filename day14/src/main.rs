// Advent of Code 2023
//
// John Perry
//
// Day 14: Parabolic Reflector Dish
//
// part 1: compute the load incurred by the movable rocks after tilting north
//
// part 2: compute the load after 1 billion spin cycles
//
// this basically translates the Ada code; see that for details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error(
        "Insufficient Iterations! recompile with more spin cycles or a higher potential period"
    )]
    InsufficientIterations,
}

fn main() {
    let mut system = read_input();
    println!("the total load is {}", part_1(&system));
    println!(
        "after 1 billion spin cycles, the load is {}",
        part_2(&mut system).unwrap()
    );
}

#[derive(Default, Clone, Copy, PartialEq)]
enum Object {
    Movable,
    Immovable,
    #[default]
    Ash,
}

const DOING_EXAMPLE: bool = false;
const SIDE_LENGTH: usize = if DOING_EXAMPLE { 10 } else { 100 };
type System = [[Object; SIDE_LENGTH]; SIDE_LENGTH];

fn read_input() -> System {
    use std::io::BufRead;
    let mut result = [[Object::default(); SIDE_LENGTH]; SIDE_LENGTH];
    let input = std::fs::File::open(if DOING_EXAMPLE {
        "example.txt"
    } else {
        "input.txt"
    })
    .expect("what? no input?!?");
    let mut lines = std::io::BufReader::new(input).lines();
    for row in &mut result {
        let line = lines
            .next()
            .expect("a line without a line? how?!?")
            .expect("double result? to quote Neo from the matrix: whoa.");
        line.chars().zip(row).for_each(|(symbol, entry)| {
            *entry = match symbol {
                '#' => Object::Immovable,
                '.' => Object::Ash,
                'O' => Object::Movable,
                _ => panic!("unexpected symbol!"),
            }
        });
    }
    result
}

fn part_1(system: &System) -> usize {
    system.iter().enumerate().fold(0, |result, (ith, row)| {
        result
            + row
                .iter()
                .enumerate()
                .filter(|(_, object)| **object == Object::Movable)
                .map(|(jth, _)| {
                    let (travel_row, movable_in_the_way) = (0..ith)
                        .rev()
                        .take_while(|kth| system[*kth][jth] != Object::Immovable)
                        .fold((ith, 0), |(row, num_movable), kth| {
                            (
                                row - 1,
                                if system[kth][jth] == Object::Movable {
                                    num_movable + 1
                                } else {
                                    num_movable
                                },
                            )
                        });
                    SIDE_LENGTH - ((travel_row + 1) + movable_in_the_way) + 1
                })
                .sum::<usize>()
    })
}

fn tilt_north(system: &mut System) {
    for row in 0..SIDE_LENGTH {
        for col in 0..SIDE_LENGTH {
            if system[row][col] == Object::Movable {
                let mut travel_row = row;
                while travel_row > 0 && system[travel_row - 1][col] == Object::Ash {
                    travel_row -= 1;
                }
                system[row][col] = Object::Ash;
                system[travel_row][col] = Object::Movable;
            }
        }
    }
}

fn tilt_south(system: &mut System) {
    for row in (0..SIDE_LENGTH).rev() {
        for col in 0..SIDE_LENGTH {
            if system[row][col] == Object::Movable {
                let mut travel_row = row;
                while travel_row < SIDE_LENGTH - 1 && system[travel_row + 1][col] == Object::Ash {
                    travel_row += 1;
                }
                system[row][col] = Object::Ash;
                system[travel_row][col] = Object::Movable;
            }
        }
    }
}

fn tilt_west(system: &mut System) {
    for row in system.iter_mut() {
        for col in 0..SIDE_LENGTH {
            if row[col] == Object::Movable {
                let mut travel_col = col;
                while travel_col > 0 && row[travel_col - 1] == Object::Ash {
                    travel_col -= 1;
                }
                row[col] = Object::Ash;
                row[travel_col] = Object::Movable;
            }
        }
    }
}

fn tilt_east(system: &mut System) {
    for row in system.iter_mut() {
        for col in (0..SIDE_LENGTH).rev() {
            if row[col] == Object::Movable {
                let mut travel_col = col;
                while travel_col < SIDE_LENGTH - 1 && row[travel_col + 1] == Object::Ash {
                    travel_col += 1;
                }
                row[col] = Object::Ash;
                row[travel_col] = Object::Movable;
            }
        }
    }
}

fn part_2(system: &mut System) -> Result<usize, Error> {
    const ITERATIONS: usize = 200;
    const POTENTIAL_PERIOD: usize = 100;
    let mut results = Vec::new();

    for _ in 0..ITERATIONS {
        tilt_north(system);
        tilt_west(system);
        tilt_south(system);
        tilt_east(system);
        results.push((0..SIDE_LENGTH).fold(0, |result, row| {
            result
                + (0..SIDE_LENGTH)
                    .filter(|col| system[row][*col] == Object::Movable)
                    .map(|_| SIDE_LENGTH - (row + 1) + 1)
                    .sum::<usize>()
        }));
    }

    let mut final_load = 0;
    for period in 3..POTENTIAL_PERIOD {
        if (0..(period - 1))
            .all(|ith| results[ITERATIONS - (ith + 1)] == results[ITERATIONS - (ith + 1) - period])
        {
            println!("period is {period}");
            final_load =
                results[ITERATIONS - (period + 1) + ((1_000_000_000 - ITERATIONS) % period)];
            break;
        }
    }
    if final_load == 0 {
        Err(Error::InsufficientIterations)
    } else {
        Ok(final_load)
    }
}
