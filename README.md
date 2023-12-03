# Advent of Code 2023 in Ada and Rust! :new: :crab: and even some :new: Modula-2!

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 5 years of pain and suffering aren't enough. :grin:
This year features an attempt to implement the same algorithm
in both Ada and Rust.
For fun, I thrown in some Modula-2 from time to time...
well, at least once.

* [Day 1](#day-1-trebuchet): Trebuchet?!

## Day 1: Trebuchet?!

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

### Unusual tools

* Ada: contracts on the `Digit` function
  (sure would be nice if Ada had a `To_Digit` function the way Rust does)
* Modula-2: just using Modula-2 these days was unusual enough
* Rust: Aho-Corasick

### Experience

#### Ada and Rust

This is the first time I use Aho-Corasick in Rust,
and as with many things Rust, there was quite the learning curve.

Since I'm using Aho-Corasick in Rust, the algorithms aren't quite the same.
The Ada uses a sequence of `if`-`then` statements
and converts each character or string to a digit.

Speedwise, the two are more or less the same.
The Rust is a little faster, but (a) I'm running it in release mode,
while I'm running Ada at whatever gnat's default optimization level is, and
(b) Ada is checking the contracts for the `Digit` function.

#### Modula-2

This is my first non-trivial Modula-2 program in about 3 decades.
While it was fun to work with it in principle,
I encountered several issues that Ada and/or Rust would have prevented.
Of course, it's possible that Modula-2 offers a convenient way to do this,
and I just don't know about it.

1. Standard Library?

   The default library for gm2 is based on PIM,
   Niklaus Wirth's "Programming in Modula-2" specification.
   The ISO standard is different in some areas,
   such as string comparison:
   PIM, like C's string comparison, returns an integer,
   while ISO defines and returns a proper type.
   Fortunately, gnu Modula-2 offers the option
   to compile against the ISO libraries.
1. Uninitialized variables?

   I spent _way_ too long debugging an error where I was returning an uninitialized variable.
   Rust requires you to initialize all variables,
   and while Ada doesn't (!!!) it does allow you to specify initial values.
1. Inconvenient initialization?

   Both Ada and Rust allow you to initialize a variable when you declare it.
   Not so with Modula-2; you must wait until the body (`BEGIN`)...
   which means you may forget to initialize it.
1. String Comparisons? Substrings?

   Ada and Rust allow easy string comparison and substrings.
   Modula-2 does not, or at least I couldn't find a way.
   I had to declare and fill in a temporary variable instead
   (`Candidate`).

Somewhat surprisingly, the compiled code is several times slower
than unoptimized Ada and Rust, even when optimized with `-Ofast` and `-flto`.

## Day 2: Cube Conundrum

You're on an island in the clouds, walking along with an elf
who plays games with cubes.

1. Sum the indices of the games that satisfy bounds on the colored cubes.
1. Sum the powers of the minimum bounds on each game to make it valid.

### Unusual Tools

* Rust: pest crate, for parsing the input

### Experience

The Rust took a lot longer to write because I had to figure out the pest crate,
but I think it makes the top-level code easier to read, though
I'm not so sure about `TryFrom<Pair> for Set`.
