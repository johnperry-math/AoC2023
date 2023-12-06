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
* [Day 4](#day-4-scratchcards): Scratchcards
* [Day 5](#day-5-if-you-give-a-seed-a-fertilizer): If You Give A Seed A Fertilizer
* [Day 6](#day-6-wait-for-it)

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
  
## Day 4: Scratchcards

An elf can help you, but (as usual) first he wants a favor.
He has a bunch of scratchcards with numbers written on them,
and he scratches off the values to see which numbers he has.

1. If he receives 2^(n - 1) points on a card with n matches,
   how many points does he win?
1. Oops! (funny how often we say that in the Advent of Code...)
   He _doesn't_ receive points; instead, he receives copies of cards.
   If card i has m matches, then he receives a new copy of cards
   i + 1, i + 2, ..., i + m. How many cards does he have at the end?

### Unusual tools

1. It still tickles me pink to use Ada 2022's `'Reduce`.

### Experience

* Pretty pleased with how quickly I worked this out in Ada.
  A silly typo held me up on Part 2:

      Copies (Ith + Offset) := (@ + 1) * Copies (Ith);

  That leads to a **lot** of cards, already by card 5 or 6! :astonished:
* The Rust was also pretty straightforward. I got hung up on three things:
  * I first tried to `.split` the input strings on a ` `, but
    somehow that led to empty strings in the input.
    I never looked into what I was doing wrong because `.split_ascii_whitespace`
    did the job.
  * 0-based indexing meant I was off-by-one
    when I used `.skip(card)` in part 2.
    Fixed by using `.slip(card + 1)`.
  * I botched the initial filter in `matches`,
    forgetting what types I was looking at.

## Day 5: If You Give A Seed A Fertilizer

Yet another elf who's willing to help you,
though of course he wants a favor first.
He seems oddly unashamed of how long he's overlooked sending water.

1. help an elf figure out which of 20 seeds to plant
   by following an almanac's mapping of seed to soil,
   soil to fertilizer, etc.
2. whoops! **(this is getting to be a habit...)**
   it's not 20 seeds, but 10 seeds and 10 intervals;
   do it again with this understanding

### Unusual tools

* I hadn't used Ada's `Containers.Generic_Array_Sort` in a while.
  In fact, I couldn't even find it when searching previous AoC's,
  though I did search for `Sorter` instead of `Sort`.
  Amazingly, I instantiated the generic correctly on the first try!
* Interval operations, in particular intersections and/or partitions.
* :warning: 64-bit integers (`Long_Long_Integer` in Ada) as for some reason
  gnat _still_ thinks the default should be 32-bit integers.
  (Who would ever need a value greater than 4 billion or so?)

### Experience

* Despite the huge numbers, I was able to solve both parts via brute force.
  Part 2 will take a while! while I waited,
  I thought about how to tackle it efficiently, and hey, hey!
  I came up with the approach implemented here:
  splitting intervals when they overlap a mapping
  without containing or being contained.
  
  Before I could implement it, the brute force approach terminated
  _with the correct answer!_ :astonished:
  
  I pressed on out of a desire to have a _good_ solution, and
  once I wrung out the bugs, the correct answer popped up.
  This is much, _much_ faster, something like 9 milliseconds,
  as opposed to 9 minutes, or however long it took
  the inefficient solution to do its thing.
* The Rust implementation is about half as many as the Ada implementation.
  I'm not sure why that is, but a non-trivial part of it is:
  * formatting
  * more detailed comments in the Ada
  * the Rust lacks the brute-force implementation of Part 2
  * it takes a few more lines to set up certain library structures in Ada;
    compare, for instance, instantiation of Ada's `Interval_Vectors` package
    as opposed to Rust's inline declaration of the `Vec<Interval>`.

## Day 6: Wait For It

You need some sand, and Desert Island seems like a good place to find some.
But how should you get there? Oddly, you can't just ask any of the elves
hanging around, pointing out that you need to save Christmas, nooooo.
You have to finagle your way onto a ferry. But you have no money, apparently.
Fortunately, you can win an all-expenses-paid ferry ride there, if only
you manage to win at the boat races.

In these boat races, you press a button on top of the boat for x milliseconds,
which increases the speed the boat travels, then you release it to travel
the remaining time allotted to the race. You need to make a good distance.

1. How many different options do you have to press it a whole number of seconds
   and beat the current distance record in each race?
2. whoops! (:roll_eyes:) those weren't the records in separate races;
   they were the records in one race, but printed out with, **and I quote**,
   "very bad kerning". :rofl: :rofl: :rofl: :rofl: Rinse, lather, repeat.

### Unusual Tools

1. I haven't used Ada's floating-point packages in so long,
   I forgot everything there was to using them.

### Experience

Hey, it's the quadratic formula! :heart:
It only took me an embarrassingly long time
to line up the data and the coefficients! :blush:

Satisfying the compiler in Part 2 was tedious, but not especially hard.

This was surprisingly easy for a Day 6.
I can see how it could easily go awry for someone who didn't check for
the edge case, or who didn't recall how the decimal system works.