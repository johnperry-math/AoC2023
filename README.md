# Advent of Code 2023 in Ada and Rust! :new: :crab:

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 5 years of pain and suffering aren't enough. :grin:
This year features an attempt to implement the same algorithm
in both Ada and Rust.

* [Day 1](#day-1-trebuchet): Trebuchet?!

### Day 1: Trebuchet?!

The elves have decided you need to fix the lack of snow.
Their solution is to catapult you into the sky via a trebuchet.

This has the most entertaining paragraph I recall in Advent of Code:

> You try to ask why they can't just use a weather machine ("not powerful enough")
> and where they're even sending you ("the sky")
> and why your map looks mostly blank ("you sure ask a lot of questions")
> and hang on did you just say the sky ("of course, where do you think snow comes from")
> when you realize that the Elves
> are already loading you into a trebuchet ("please hold still, we need to strap you in").

The problem depends on **calibration values**,
which are the first and last "digits" to appear in a string.

1. Sum the calibration values, where "digit" means `1..9`.
1. Sum the calibration values, where "digit" now includes the spellings
   (`one`, `two`, ...)

#### Unusual tools

* Ada: contracts on the `Digit` function
  (sure would be nice if Ada had a `To_Digit` function the way Rust does)
* Rust: Aho-Corasick

#### Experience

This is the first time I use Aho-Corasick in Rust,
and as with many things Rust, there was quite the learning curve.

Since I'm using Aho-Corasick in Rust, the algorithms aren't quite the same.
The Ada uses a sequence of `if`-`then` statements
and converts each character or string to a digit.

Speedwise, the two are more or less the same.
The Rust is a little faster, but (a) I'm running it in release mode,
while I'm running Ada at whatever gnat's default optimization level is, and
(b) Ada is checking the contracts for the `Digit` function.
