// Advent of Code 2023
//
// John Perry
//
// Day 4: Scratchcards
//
// part 1: determine the total points won on each scratchcard
//
// part 2: whoops! points are inappropriate; rather, you win copies of cards
//         and get to play them, too. how many cards do you have at the end?

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::io::{BufRead, BufReader};

// SECTION
// global types and constants

const MAX_CARD: usize = 100;                // largest value a card can have
const NUM_CARDS: usize = 201;               // number of cards
const NUM_ON_CARD: usize = 10;              // number of numbers on a card
const NUM_YOU_HAVE: usize = 25;             // number of numbers you have

#[derive(Copy, Clone)]
/// a card has numbers on it, plus numbers you have
/// (the former are printed on and the latter are revealed by scratching?)
struct CardSetup {
    on_card: [bool; MAX_CARD],
    you_have: [bool; MAX_CARD],
}

type AllGames = [CardSetup; NUM_CARDS];     // type of data

impl Default for CardSetup {
    fn default() -> Self {
        Self {
            on_card: [false; MAX_CARD],
            you_have: [false; MAX_CARD],
        }
    }
}

impl CardSetup {
    // number of values on the card that match what you have
    pub fn matches(&self) -> usize {
        self.on_card
            .iter()
            .enumerate()
            // identify the numbers on the card
            .filter(|(_, has)| **has)
            // drop the boolean
            .map(|(value, _)| value)
            // keep only the ones you have
            .filter(|value| self.you_have[*value])
            // count the remaining ones and return that value
            .count()
    }
}

fn main() {
    let games = read_input();
    println!("the cards are worth {}", part_1(&games));
    println!("you end up with {}", part_2(&games));
}

fn read_input() -> AllGames {
    let mut result = [CardSetup::default(); NUM_CARDS];
    let input = std::fs::File::open("input.txt").expect("what, where's input.txt?!?");
    let reader = BufReader::new(input);
    reader.lines().enumerate().for_each(|(card_num, line_in)| {
        let binding = line_in.expect("uh... we really should have a line here...");
        // split into words
        let line_in = binding.split_ascii_whitespace();
        // record the numbers on the card
        line_in
            .clone()
            // skip "Game" and "<number>:"
            .skip(2)
            // keep the next few, discard the rest
            .take(NUM_ON_CARD)
            // convert to a number
            .map(|text| {
                text.parse::<usize>()
                    .unwrap_or_else(|_| panic!("{text} is not a u8"))
            })
            // record it on the card
            .for_each(|value| result[card_num].on_card[value] = true);
        line_in
            // skip "Game", "<number>:", the numbers on the card, and "|"
            .skip(2 + NUM_ON_CARD + 1)
            // keep the ones you need, discard the rest (though there aren't any)
            .take(NUM_YOU_HAVE)
            // convert to a number
            .map(|text| {
                text.parse::<usize>()
                    .unwrap_or_else(|_| panic!("{text} is not a u8?!"))
            })
            // record it on the card
            .for_each(|value| result[card_num].you_have[value] = true);
    });
    result
}

fn part_1(cards: &AllGames) -> usize {
    #[allow(clippy::cast_possible_truncation)]
    cards
        .iter()
        .map(|setup| 2_usize.pow(setup.matches() as u32 - 1))
        .sum()
}

fn part_2(cards: &AllGames) -> usize {
    let mut copies = [1; NUM_CARDS];
    cards.iter().enumerate().for_each(|(card, setup)| {
        let card_copies = copies[card];
        copies
            .iter_mut()
            // ARGH got hung up a while because I used card instead of card + 1
            .skip(card + 1)
            .take(setup.matches())
            .for_each(|copy| {
                // need to use a copied value
                // as otherwise we'd borrow copies immutable,
                // violating borrow checker rules
                *copy += card_copies;
            });
    });
    copies.iter().sum()
}
