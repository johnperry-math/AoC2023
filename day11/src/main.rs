use std::io::BufRead;

// Advent of Code 2023
//
// John Perry
//
// Day 11: Cosmic Expansion
//
// part 1: sum the distances between pairs of galaxies
//         when each empty row/col of space expands by 1
//
// part 2: repeat, but expand by 1 million
//
// this basically translates the Ada code; see that for details

fn main() {
    let (universe, galaxies) = read_input();
    println!(
        "the sum of distances between galaxies is {}",
        sum_distances_expanded_by(&universe, &galaxies, 2)
    );
    println!(
        "the sum of distances between galaxies is {}",
        sum_distances_expanded_by(&universe, &galaxies, 1_000_000)
    );
}

#[derive(Clone, Copy, PartialEq)]
enum Object {
    Galaxy,
    Space,
}

const INITIAL_DIM: usize = 140;

type Universe = [[Object; INITIAL_DIM]; INITIAL_DIM];
type Galaxies = Vec<Location>;

struct Location {
    row: usize,
    col: usize,
}

fn read_input() -> (Universe, Galaxies) {
    let mut universe = [[Object::Space; INITIAL_DIM]; INITIAL_DIM];
    let mut galaxies = Vec::<Location>::default();
    let file =
        std::fs::File::open("input.txt").expect("er... where's my input?");
    for (row, line) in std::io::BufReader::new(file).lines().enumerate() {
        for (col, symbol) in line
            .expect("how'd we read a line that isn't ok?")
            .chars()
            .enumerate()
        {
            if symbol == '#' {
                universe[row][col] = Object::Galaxy;
                galaxies.push(Location { row, col });
            }
        }
    }

    (universe, galaxies)
}

fn sum_distances_expanded_by(
    universe: &Universe,
    galaxies: &Galaxies,
    expansion: usize,
) -> usize {
    let mut result = 0;
    let (mut empty_row, mut empty_col) =
        ([false; INITIAL_DIM], [false; INITIAL_DIM]);
    universe
        .iter()
        .zip(&mut empty_row)
        .filter(|(row, _)| row.iter().all(|entry| *entry == Object::Space))
        .for_each(|(_, entry)| *entry = true);
    (0..INITIAL_DIM)
        .zip(&mut empty_col)
        .filter(|(col, _)| {
            (0..INITIAL_DIM).all(|row| universe[row][*col] == Object::Space)
        })
        .for_each(|(_, entry)| *entry = true);

    for (ith, first) in galaxies.iter().enumerate() {
        for second in &galaxies[ith + 1..] {
            let min_row = first.row.min(second.row);
            let min_col = first.col.min(second.col);
            let max_row = first.row.max(second.row);
            let max_col = first.col.max(second.col);
            let mut pair_distance = (max_row - min_row) + (max_col - min_col);
            pair_distance +=
                (min_row..=max_row).filter(|ith| empty_row[*ith]).count()
                    * (expansion - 1);
            pair_distance +=
                (min_col..=max_col).filter(|ith| empty_col[*ith]).count()
                    * (expansion - 1);
            result += pair_distance;
        }
    }

    result
}
