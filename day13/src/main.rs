#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::fmt::Display;
use std::io::{BufRead, BufReader};

fn main() {
    let all_maps = read_input();
    println!("summary of notes is {}", part_1(&all_maps));
    println!("after cleaning smudges, summary is {}", part_2(&all_maps));
}

#[derive(PartialEq, Eq, Debug)]
enum Object {
    Ash,
    Rock,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ash => '.',
                Self::Rock => '#',
            }
        )
    }
}

struct Map {
    rows: usize,
    cols: usize,
    values: Vec<Vec<Object>>,
}

fn finalize(map_input: &[String]) -> Map {
    let rows = map_input.len();
    let cols = map_input
        .first()
        .expect("how'd we get a map with no rows?!?")
        .len();
    let values = map_input
        .iter()
        .map(|row| {
            row.chars()
                .map(|symbol| match symbol {
                    '.' => Object::Ash,
                    '#' => Object::Rock,
                    _ => panic!("unexpected symbol!"),
                })
                .collect()
        })
        .collect();
    Map { rows, cols, values }
}

#[allow(dead_code)]
fn put_map(map: &Map) {
    for row in &map.values {
        for value in row {
            print!("{value}");
        }
        println!();
    }
}

fn read_input() -> Vec<Map> {
    let mut result = Vec::new();
    let input = std::fs::File::open("input.txt").expect("what? no input?");
    let lines = BufReader::new(input).lines();
    let mut map_input = Vec::new();
    for line in lines {
        let line = line.expect("line without a line?!? what?");
        if line.is_empty() {
            result.push(finalize(&map_input));
            map_input = Vec::new();
        } else {
            map_input.push(line);
        }
    }
    result.push(finalize(&map_input));
    result
}

fn detect_horizontal_axis(map: &Map) -> Option<usize> {
    // for row in 1..map.rows {
    //     let mut has_symmetry = true;
    //     for offset in 1..=row.min(map.rows - row) {
    //         has_symmetry = true;
    //         if (0..map.cols).any(|col| {
    //             map.values[row - offset][col]
    //                 != map.values[row + offset - 1][col]
    //         }) {
    //             has_symmetry = false;
    //             break;
    //         }
    //     }
    //     if has_symmetry {
    //         return Some(row);
    //     }
    // }
    // None
    map.values
        .iter()
        .enumerate()
        .skip(1)
        .find(|(row, _)| {
            (1..=*row.min(&(map.rows - row))).all(|offset| {
                (0..map.cols).all(|col| {
                    map.values[row - offset][col]
                        == map.values[row + offset - 1][col]
                })
            })
        })
        .map(|(row, _)| row)
}

fn detect_vertical_axis(map: &Map) -> Option<usize> {
    // for col in 1..map.cols {
    //     let mut has_symmetry = true;
    //     for offset in 1..=col.min(map.cols - col) {
    //         has_symmetry = true;
    //         if (0..map.rows).any(|row| {
    //             map.values[row][col - offset]
    //                 != map.values[row][col + offset - 1]
    //         }) {
    //             has_symmetry = false;
    //             break;
    //         }
    //     }
    //     if has_symmetry {
    //         return Some(col);
    //     }
    // }
    // None
    (1..map.cols).find(|col| {
        (1..=*col.min(&(map.cols - col))).all(|offset| {
            map.values
                .iter()
                .all(|row| row[col - offset] == row[col + offset - 1])
        })
    })
}

fn part_1(all_maps: &[Map]) -> usize {
    all_maps.iter().fold(0, |result, map| {
        result
            + 100 * detect_horizontal_axis(map).map_or(0, |x| x)
            + detect_vertical_axis(map).map_or(0, |x| x)
    })
}

fn find_horizontal_axis(map: &Map) -> Option<usize> {
    map.values
        .iter()
        .enumerate()
        .skip(1)
        .find(|(row, _)| {
            (1..=*row.min(&(map.rows - row)))
                .map(|offset| {
                    (0..map.cols)
                        .filter(|col| {
                            map.values[row - offset][*col]
                                != map.values[row + offset - 1][*col]
                        })
                        .count()
                })
                .sum::<usize>()
                == 1
        })
        .map(|(row, _)| row)

    // let mut result = None;
    // for row in 1..map.rows {
    //     let mut number_of_inconsistencies = 0;
    //     for offset in 1..=row.min(map.rows - row) {
    //         if number_of_inconsistencies > 1 {
    //             break;
    //         }
    //         for col in 0..map.cols {
    //             if map.values[row - offset][col]
    //                 != map.values[row + offset - 1][col]
    //             {
    //                 number_of_inconsistencies += 1;
    //                 if number_of_inconsistencies == 1 {
    //                     result = Some(row);
    //                 } else {
    //                     result = None;
    //                     break;
    //                 }
    //             }
    //         }
    //     }
    //     if number_of_inconsistencies == 1 {
    //         break;
    //     }
    // }
    // result
}

fn find_vertical_axis(map: &Map) -> Option<usize> {
    (1..map.cols).find(|col| {
        (1..=*col.min(&(map.cols - col)))
            .map(|offset| {
                (0..map.rows)
                    .filter(|row| {
                        map.values[*row][col - offset]
                            != map.values[*row][col + offset - 1]
                    })
                    .count()
            })
            .sum::<usize>()
            == 1
    })
    // let mut result = None;
    // for col in 1..map.cols {
    //     let mut number_of_inconsistencies = 0;
    //     for offset in 1..=col.min(map.cols - col) {
    //         if number_of_inconsistencies > 1 {
    //             break;
    //         }
    //         for row in 0..map.rows {
    //             if map.values[row][col - offset]
    //                 != map.values[row][col + offset - 1]
    //             {
    //                 number_of_inconsistencies += 1;
    //                 if number_of_inconsistencies == 1 {
    //                     result = Some(col);
    //                 } else {
    //                     result = None;
    //                     break;
    //                 }
    //             }
    //         }
    //     }
    //     if number_of_inconsistencies == 1 {
    //         break;
    //     }
    // }
    // result
}

fn part_2(all_maps: &[Map]) -> usize {
    all_maps.iter().fold(0, |result, map| {
        result
            + 100 * find_horizontal_axis(map).map_or(0, |x| x)
            + find_vertical_axis(map).map_or(0, |x| x)
    })
}
