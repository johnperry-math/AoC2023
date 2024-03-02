// Advent of Code 2023
//
// John Perry
//
// Day 17: Clumsy Crucible
//
// part 1: find the least heat loss possible
//
// part 2: repeat, using an ultra crucible
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
// need the following to silence clippy's warning when converting i8 to usize

const DOING_EXAMPLE: bool = false;
const SIDE_LENGTH: usize = if DOING_EXAMPLE { 13 } else { 141 };

use std::collections::{btree_map::Entry, BTreeMap, VecDeque};

use common::{
    two_dimensional_map::{Location, Map},
    two_dimensional_map_io::read_input,
    two_dimensional_motion::{delta, opposite, Direction, ALL_DIRECTIONS},
};

const FILENAME: &str = if DOING_EXAMPLE {
    "example.txt"
} else {
    "input.txt"
};

fn deserialize(symbol: char) -> usize {
    match symbol {
        '1'..='9' => symbol
            .to_digit(10)
            .expect("this really should not have happened") as usize,
        _ => panic!("Whoah, nelly -- this REALLY should not have happened"),
    }
}

fn main() {
    let map = read_input::<SIDE_LENGTH, SIDE_LENGTH, usize>(FILENAME.to_string(), &deserialize);
    println!("minimum heat loss to traverse the map is {}", part_1(&map));
    println!("with super crucible it's {}", part_2(&map));
}

#[derive(Clone, Debug)]
struct State {
    location: Location,
    dir: Direction,
    repeated: usize,
    path: Vec<Location>,
}

struct NeighborCost<const MIN_RUN: usize, const LENGTH: usize> {
    north: [usize; LENGTH],
    south: [usize; LENGTH],
    east: [usize; LENGTH],
    west: [usize; LENGTH],
}

impl<const MIN_RUN: usize, const LENGTH: usize> Default for NeighborCost<MIN_RUN, LENGTH> {
    fn default() -> Self {
        Self {
            north: [usize::MAX; LENGTH],
            south: [usize::MAX; LENGTH],
            east: [usize::MAX; LENGTH],
            west: [usize::MAX; LENGTH],
        }
    }
}

impl<const MIN_RUN: usize, const LENGTH: usize> NeighborCost<MIN_RUN, LENGTH> {
    const fn value(&self, dir: Direction, repeated: usize) -> usize {
        #[allow(clippy::enum_glob_use)]
        use Direction::*;
        match dir {
            North => self.north[repeated - MIN_RUN],
            South => self.south[repeated - MIN_RUN],
            East => self.east[repeated - MIN_RUN],
            West => self.west[repeated - MIN_RUN],
        }
    }
    fn assign(&mut self, dir: Direction, repeated: usize, value: usize) {
        #[allow(clippy::enum_glob_use)]
        use Direction::*;
        match dir {
            North => self.north[repeated - MIN_RUN] = value,
            South => self.south[repeated - MIN_RUN] = value,
            East => self.east[repeated - MIN_RUN] = value,
            West => self.west[repeated - MIN_RUN] = value,
        }
    }
}

fn part_1(map: &Map<SIDE_LENGTH, SIDE_LENGTH, usize>) -> usize {
    const MIN_RUN: usize = 1;
    const LENGTH: usize = 3;

    let mut result = 0;

    let mut to_do = Vec::default();
    for _ in 0..5_000 {
        to_do.push(VecDeque::new());
    }
    let mut visited = BTreeMap::<Location, NeighborCost<MIN_RUN, LENGTH>>::default();

    to_do[0].push_back(State {
        location: Location::new(0, 0),
        dir: Direction::East,
        repeated: MIN_RUN,
        path: Vec::new(),
    });

    loop {
        while result <= to_do.len() && to_do[result].is_empty() {
            result += 1;
        }

        let mut curr = to_do[result]
            .pop_front()
            .expect("tried to pop from an empty queue...");
        if let Entry::Vacant(e) = visited.entry(curr.location) {
            e.insert(NeighborCost::default());
        }

        if visited[&curr.location].value(curr.dir, curr.repeated) > result {
            curr.path.push(curr.location);
            visited
                .get_mut(&curr.location)
                .expect("how did we not have this location?")
                .assign(curr.dir, curr.repeated, result);

            for dir in ALL_DIRECTIONS {
                if map.in_range(curr.location, dir)
                    && (curr.dir != dir || curr.repeated < MIN_RUN + LENGTH - 1)
                {
                    let new_location = curr.location.delta(dir);
                    if new_location.row() == SIDE_LENGTH - 1
                        && new_location.col() == SIDE_LENGTH - 1
                    {
                        return result + map[new_location];
                    }
                    if !curr.path.contains(&new_location) {
                        let new_state = State {
                            location: new_location,
                            dir,
                            repeated: if dir == curr.dir {
                                curr.repeated + 1
                            } else {
                                MIN_RUN
                            },
                            path: curr.path.clone(),
                        };
                        to_do[result + map[new_location]].push_back(new_state);
                    }
                }
            }
        }
    }
}

#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
fn part_2(map: &Map<SIDE_LENGTH, SIDE_LENGTH, usize>) -> usize {
    const MIN_RUN: usize = 4;
    const LENGTH: usize = 7;
    let goal = Location::new(SIDE_LENGTH - 1, SIDE_LENGTH - 1);

    let mut result = usize::MAX;
    let mut current_heat = 0;

    let mut to_do = Vec::default();
    for _ in 0..5_000 {
        to_do.push(VecDeque::new());
    }
    let mut visited = BTreeMap::<Location, NeighborCost<4, 7>>::default();

    let mut start = State {
        location: Location::new(0, 0),
        dir: Direction::East,
        repeated: 10,
        path: vec![Location::new(0, 0)],
    };
    to_do[0].push_back(start.clone());
    start.dir = Direction::South;
    to_do[0].push_back(start);

    while current_heat < result {
        while current_heat < to_do.len() && to_do[current_heat].is_empty() {
            current_heat += 1;
        }

        let curr = to_do[current_heat]
            .pop_front()
            .expect("this should not have happened...");
        if let Entry::Vacant(e) = visited.entry(curr.location) {
            e.insert(NeighborCost::default());
        }

        if visited[&curr.location].value(curr.dir, curr.repeated) > current_heat {
            visited
                .get_mut(&curr.location)
                .expect("how did we not have this location?")
                .assign(curr.dir, curr.repeated, current_heat);

            for dir in ALL_DIRECTIONS {
                if dir == curr.dir
                    && curr.repeated < MIN_RUN + LENGTH - 1
                    && map.in_range(curr.location, dir)
                {
                    let new_location = curr.location.delta(dir);
                    let mut next = State {
                        location: new_location,
                        dir,
                        repeated: curr.repeated + 1,
                        path: curr.path.clone(),
                    };
                    next.path.push(new_location);
                    let new_heat = current_heat + map[goal];
                    if next.location == goal && next.repeated >= MIN_RUN && new_heat < result {
                        result = new_heat;
                    }
                    to_do[current_heat + map[new_location]].push_back(next);
                } else if dir != curr.dir
                    && !opposite(dir, curr.dir)
                    && (0..SIDE_LENGTH as isize).contains(
                        &(curr.location.row() as isize + 4 * isize::from(delta(dir).d_row())),
                    )
                    && (0..SIDE_LENGTH as isize).contains(
                        &(curr.location.col() as isize + 4 * isize::from(delta(dir).d_col())),
                    )
                {
                    let dir_delta = delta(dir);
                    let new_location = Location::new(
                        (curr.location.row() as isize + 4 * isize::from(dir_delta.d_row()))
                            as usize,
                        (curr.location.col() as isize + 4 * isize::from(dir_delta.d_col()))
                            as usize,
                    );
                    let mut next_state = State {
                        location: new_location,
                        dir,
                        repeated: 4,
                        path: curr.path.clone(),
                    };
                    let mut new_heat = current_heat;
                    for ith in 1..=4 {
                        let new_location = Location::new(
                            (curr.location.row() as isize + ith * isize::from(dir_delta.d_row()))
                                as usize,
                            (curr.location.col() as isize + ith * isize::from(dir_delta.d_col()))
                                as usize,
                        );
                        next_state.path.push(new_location);
                        new_heat += map[new_location];
                    }

                    if next_state.location == goal && new_heat < result {
                        result = new_heat;
                    }

                    to_do[new_heat].push_back(next_state);
                }
            }
        }
    }

    result
}
