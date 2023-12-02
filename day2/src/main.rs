// Advent of Code 2018
//
// John Perry
//
// Day 2: Cube Conundrum
//
// part 1: determine which cube games are valid
//
// part 2: determine the sum of power values of minimal bounds for all cube games

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use std::io::{BufRead, BufReader};

// Section
// global types and constants

#[derive(Default, Copy, Clone)]
struct Set {
    blue: u32,
    green: u32,
    red: u32,
}

const BOUNDS: Set = Set {
    blue: 14,
    green: 13,
    red: 12,
};

type Game = Vec<Set>;

// Section
// Parsing the input

#[derive(Parser)]
#[grammar = "game.pest"]
struct GamesParser;

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("unexpected token {0} when expecting {1}")]
    UnexpectedToken(String, String),
}

// putting this here to avoid making read_input() too hard to read
impl<'a> TryFrom<Pair<'a, Rule>> for Set {
    type Error = Error;

    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        let mut set = Self::default();
        let mut number: u32 = 0;

        value
            .into_inner()
            .try_for_each(|value| {
                value.into_inner().try_for_each(|value| {
                    match value.as_rule() {
                        Rule::number => {
                            number =
                                value.as_str().parse().unwrap_or_else(|_| {
                                    panic!(
                                        "how'd we get {} as a number?!?",
                                        value.as_str()
                                    )
                                });
                        }

                        Rule::blue => set.blue = number,

                        Rule::green => set.green = number,

                        Rule::red => set.red = number,

                        _ => {
                            return Err(Error::UnexpectedToken(
                                value.as_str().to_owned(),
                                "number or color".to_owned(),
                            ))
                        }
                    }
                    Ok(())
                })
            })
            // using unwrap because pest gives reasonably decent errors
            .unwrap();
        Ok(set)
    }
}

// SECTION
// I/O

fn read_input() -> Vec<Game> {
    let mut result: Vec<Game> = Vec::default();

    let input =
        std::fs::File::open("input.txt").expect("wait, where's input.txt?!?");
    let reader = BufReader::new(input);

    reader.lines().for_each(|line| {
        let line = line.expect("what?!? no line?");
        let game_parse = GamesParser::parse(Rule::game, &line)
            .unwrap_or_else(|e| panic!("{e}"));

        // at this point we have a game node
        game_parse.into_iter().for_each(|game| {
            result.push(
                game.into_inner()
                    // skip over "Game", number, etc.
                    .filter(|node| Rule::hand == node.as_rule())
                    // turn the hand into a vector of Set
                    .map(|hand| {
                        hand.into_inner()
                            .map(|node| {
                                // we better have a set now
                                Set::try_from(node).unwrap()
                            })
                            .collect::<Vec<Set>>()
                    })
                    // there's only one element, which is a vector of Set
                    // but we have to collect into a container
                    // then extract the one element
                    .collect::<Vec<Vec<Set>>>()
                    .first()
                    .unwrap()
                    .clone(),
            );
        });
    });

    result
}

// SECTION
// Parts 1 and 2

fn main() {
    let games = read_input();
    println!("sum of valid game indices is {}", part_1(&games));
    println!("sum of power of minimum sets is {}", part_2(&games));
}

// return the sum of indices of games whose sets
// are all bounded by BOUNDS
fn part_1(games: &[Game]) -> usize {
    games
        .iter()
        .enumerate()
        .filter(|(_, game)| {
            game.iter().all(|set| {
                set.blue <= BOUNDS.blue
                    && set.green <= BOUNDS.green
                    && set.red <= BOUNDS.red
            })
        })
        .map(|(index, _)| index + 1)
        .sum()
}

fn power(set: Set) -> usize {
    usize::try_from(set.blue * set.green * set.red)
        .expect("how did we get such a large product from {set:?}?!?")
}

// return the set whose values are the maximal values of each set in game
fn minimal(game: &[Set]) -> Set {
    game.iter().fold(Set::default(), |result, set| Set {
        blue: result.blue.max(set.blue),
        green: result.green.max(set.green),
        red: result.red.max(set.red),
    })
}

// return the sum of powers of the minimal set for each game
fn part_2(games: &[Game]) -> usize {
    games
        .iter()
        .fold(0, |result, game| result + power(minimal(game)))
}
