# Advent of Code 2023 in Ada and Rust! :new: :crab: and even some :new: Modula-2!

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 5 years of pain and suffering aren't enough. :grin:
This year features an attempt to implement the same algorithm
in both Ada and Rust.
For fun, I thrown in some Modula-2 from time to time...
well, at least once.

* [Day 1](#day-1-trebuchet): Trebuchet?!
* [Day 2](#day-2-cube-conundrum): Cube Conundrum
* [Day 3](#day-3-gear-ratios): Gear Ratios

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
1. Incorrect handling of constants of variant records?

   When I tried to initialize a constant of a variant record type,
   the compiler absolutely refused to handle it, saying it was an unknown field.
   I suspect it was a compiler bug.
   I will try to look into this more and possibly report it.

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

## Day 3: Gear Ratios

You need to get your gondola going!

1. Sum the values of the parts in the schematic.
   Parts are the numbers that are adjacent to a non-period, non-decimal symbol.
1. Sum the gear "ratios".
   Gears are the numbers adjacent to `*` symbols
   that have _only_ two numbers adjacent to them.

### Unusual tools

1. In Ada I was able to define a useful `Constrants` range type
   which made life a little annoying at one point.
1. In Rust:
   * I decided to go with an array, to mimic the Ada,
     rather than to use vectors.
   * Unlike Ada, this part 2 uses a state machine (`Locations`)
     to move through the known states of a potential gear.

### Experience

* Ada

  Kind of surprised I got this one right pretty quickly.
* Modula-2

  Translating from Ada was pretty straightfoward.
  It's interesting to me that you can define a variable
  to be of a certain range type,
  but Modula-2 won't choke if it goes outside that range
  the way Ada will (unless this is a bug in gm2).
  I'm definitely appreciating some of Ada's safety features,
  since the lack of them in Modula-2 hammered me a few times.
  In particular:
  * Can't seem to define a constant type of a variant record.
    I'm not sure if that's a misunderstanding on my part,
    or a bug in gm2.
  * I'm pretty sure this is a bug:
    if you neglect the parentheses on a function procedure
    that takes no parameters, gm2 treats it as if you want the address(?).
    Hence, the following line of the module repeatedly gave the wrong answer:

        InOut.WriteInt(Part1, 0); (* needs to be Part1() *)
    
* Rust

  Man, this took a while.
  I **really** missed having access to custom arrays,
  and it's also a lot harder in Rust to check indices.
  In Ada, I can do this:

      for Row_Offset in -1 .. 1 when Row + Row_Offset in Constraints loop
         -- ...
         if CH.Is_Decimal_Digit
                (Schematic (Row + Row_Offset) (Col + Col_Offset))
  
  ...but in Rust I have to do something like this:

      (-1..2).filter(|offset| (0..MAX_IDX).contains(&(row + offset)))
         // ...
         self.schematic[(row + row_offset) as usize]
                        [(col + offset) as usize]
                  .is_ascii_digit()

   ...unless my Rust is even worse than I thought.
   Notice the conversions in Rust: in the first case,
   I'm using `i32` because otherwise it will refuse to compile
   the sum of a `usize` and the `-1` that begins the range;
   then, I have to convert the sum to a `usize`
   in order to index into the array.

   There's also the annoyance
   (which made me waste quite a bit of time debugging)
   where `(-1..1)` indicates the numbers -1 and 0, **not** 1.
   This is why I have `(-1..2)` above.

   All in all, I wasn't expecting the Rust to be harder to write
   and more annoying to look at and debug than the Ada.
  