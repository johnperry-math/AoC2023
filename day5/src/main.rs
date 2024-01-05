// Advent of Code 2023
//
// John Perry
//
// Day 4: If You Give A Seed A Fertilizer
//
// part 1: help an elf figure out which of 20 seeds to plant
//         by following an almanac's mapping of seed to soil,
//         soil to fertilizer, etc.
//
// part 2: whoops! it's not 20 seeds, but 10 seeds and 10 intervals;
//         do it again with this understanding

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::{
    fs::File,
    io::{BufRead, BufReader, Lines},
};

// SECTION
// global types and variables

// mapping of an interval of given length from source to destination
struct Map {
    source: u64,
    destination: u64,
    length: u64,
}

#[derive(Default)]
struct AllMaps {
    seed_to_soil: Vec<Map>,
    soil_to_fert: Vec<Map>,
    fert_to_watr: Vec<Map>,
    watr_to_lite: Vec<Map>,
    lite_to_temp: Vec<Map>,
    temp_to_humd: Vec<Map>,
    humd_to_locn: Vec<Map>,
}

// a value traveling through all the maps
struct MapValue(u64);

impl MapValue {
    // applies the mapping of `maps` to the wrapped value
    fn follow_map(&self, maps: &[Map]) -> Self {
        let source = self.0;
        let new_value = maps.iter()
            .find(|map| (map.source..map.source + map.length).contains(&source))
            .map_or(source, |map| map.destination + source - map.source);
        Self(new_value)
    }    
}

struct InputData {
    seed_list: Vec<u64>,
    all_maps: AllMaps,
}

fn read_map(lines: &mut Lines<BufReader<File>>, map: &mut Vec<Map>) {
    let mut lines = lines.skip(1);
    loop {
        let line = lines.next().map_or(String::new(), std::result::Result::unwrap);
        if line.is_empty() {
            return;
        }
        let mut split = line.split_ascii_whitespace();
        let destination = split
            .next()
            .expect("no destination?!?")
            .parse()
            .expect("unable to parse destination!");
        let source = split
            .next()
            .expect("no source?!?")
            .parse()
            .expect("unable to parse source!");
        let length = split
            .next()
            .expect("no length?!?")
            .parse()
            .expect("unable to parse length!");
        map.push(Map {
            source,
            destination,
            length,
        });
    }
}

fn read_input() -> InputData {
    let mut all_maps = AllMaps::default();
    let input = std::fs::File::open("input.txt").expect("what, where's input.txt?!?");
    let reader = BufReader::new(input);
    let mut lines = reader.lines();
    let seed_list = lines
        .next()
        .expect("no lines in the code?!?")
        .expect("no line... why do i have to do this twice again?!?")
        .split_ascii_whitespace()
        .skip(1)
        .map(|text| {
            text.parse::<u64>()
                .expect("unable to convert {text} to u64...?!")
        })
        .collect();

    let _ = lines.next();

    read_map(&mut lines, &mut all_maps.seed_to_soil);
    read_map(&mut lines, &mut all_maps.soil_to_fert);
    read_map(&mut lines, &mut all_maps.fert_to_watr);
    read_map(&mut lines, &mut all_maps.watr_to_lite);
    read_map(&mut lines, &mut all_maps.lite_to_temp);
    read_map(&mut lines, &mut all_maps.temp_to_humd);
    read_map(&mut lines, &mut all_maps.humd_to_locn);

    InputData {
        seed_list,
        all_maps,
    }
}

fn find_location(all_maps: &AllMaps, seed: u64) -> u64 {
    MapValue(seed).follow_map(&all_maps.seed_to_soil)
    .follow_map(&all_maps.soil_to_fert)
    .follow_map(&all_maps.fert_to_watr)
    .follow_map(&all_maps.watr_to_lite)
    .follow_map(&all_maps.lite_to_temp)
    .follow_map(&all_maps.temp_to_humd)
    .follow_map(&all_maps.humd_to_locn)
    .0
}

fn part_1(seeds: &[u64], all_maps: &AllMaps) -> u64 {
    seeds.iter().fold(u64::MAX, |result, seed| result.min(find_location(all_maps, *seed)))
}

struct Interval {
    left: u64,
    right: u64,
}

fn split_over_map(maps: &Vec<Map>, intervals: Vec<Interval>) -> Vec<Interval> {
    let mut result = intervals;
    for map in maps {
        let mut temp = Vec::new();
        let left = map.source;
        let right = map.source + map.length;

        for interval in result {
            if interval.left < left {
                if interval.right < left {
                    temp.push(interval);
                } else if interval.right <= right {
                    temp.push(Interval { left: interval.left, right: left - 1 });
                    temp.push(Interval { left, right: interval.right });
                } else {
                    temp.push(Interval { left: interval.left, right: left - 1 });
                    temp.push(Interval { left, right });
                    temp.push(Interval { left: right + 1, right: interval.right });
                }
            } else if interval.left <= right {
                if interval.right <= right {
                    temp.push(interval);
                } else {
                    temp.push(Interval { left: interval.left, right });
                    temp.push(Interval { left: right + 1, right: interval.right });
                }
            } else {
                temp.push(interval);
            }
        }
        result = temp;
    }
    result
}

fn map_and_split(maps: &Vec<Map>, intervals: Vec<Interval>) -> Vec<Interval> {
    let mut result = split_over_map(maps, intervals);
    for interval in &mut result {
        for map in maps {
            if (map.source .. map.source + map.length).contains(&interval.left) {
                interval.left += map.destination - map.source;
                interval.right += map.destination - map.source;
                break;
            }
        }
    }
    result
}

fn part_2_efficient(seeds: &[u64], all_maps: &AllMaps) -> u64 {
    let true_seeds = seeds.iter().step_by(2);
    let lengths = seeds.iter().skip(1).step_by(2);
    let pairs = true_seeds.zip(lengths);
    let intervals = pairs.map(|(left, length)| Interval {left: *left, right: left + length}).collect();

    let intervals = map_and_split(&all_maps.seed_to_soil, intervals);
    let intervals = map_and_split(&all_maps.soil_to_fert, intervals);
    let intervals = map_and_split(&all_maps.fert_to_watr, intervals);
    let intervals = map_and_split(&all_maps.watr_to_lite, intervals);
    let intervals = map_and_split(&all_maps.lite_to_temp, intervals);
    let intervals = map_and_split(&all_maps.temp_to_humd, intervals);
    let intervals = map_and_split(&all_maps.humd_to_locn, intervals);

    intervals.iter().fold(u64::MAX, |result, interval| result.min(interval.left))

}

fn main() {
    let InputData {
        seed_list,
        all_maps,
    } = read_input();
    println!("the closest location that needs a seed is {}", part_1(&seed_list, &all_maps));
    println!("no, it's actually {}", part_2_efficient(&seed_list, &all_maps));
}
