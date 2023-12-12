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
* [Day 6](#day-6-wait-for-it): Wait For It
* [Day 7](#day-7-camel-cards): Camel Cards
* [Day 8](#day-8-haunted-wasteland): Haunted Wasteland
* [Day 9](#day-9-mirage-maintenance): Mirage Maintenance
* [Day 10](#day-10-pipe-maze): Pipe Maze
* [Day 11](#day-11-cosmic-expansion): Cosmic Expansion

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
   _[update:
   [Confirmed on gm2 mailing list](https://lists.nongnu.org/archive/html/gm2/2023-12/msg00003.html)
   by the lead compiler developer, though he did point me
   to the compiler-supplied `DynamicStrings` module.]_
   I had to declare and fill in a temporary variable instead
   (`Candidate`).
1. Incorrect handling of constants of variant records?

   When I tried to initialize a constant of a variant record type,
   the compiler absolutely refused to handle it, saying it was an unknown field.
   I suspect it was a compiler bug.
   I will try to look into this more and possibly report it.
   _[update:
   [Confirmed on gm2 mailing list](https://lists.nongnu.org/archive/html/gm2/2023-12/msg00012.html)
   that this is a bug.]_

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
   
   _[update: [Confirmed by lead compiler developer](https://lists.nongnu.org/archive/html/gm2/2023-12/msg00012.html)
   that this is a bug.]_
    
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
2. whoops! ( :roll_eyes: ) those weren't the records in separate races;
   they were the records in one race, but printed out with, **and I quote**,
   "very bad kerning". :rofl: :rofl: :rofl: :rofl: Rinse, lather, repeat.

### Unusual Tools

1. I haven't used Ada's floating-point packages in so long,
   I forgot everything there was to using them.

### Experience

Hey, :heart: it's the quadratic formula!
It only took me an embarrassingly long time
to line up the data and the coefficients! :blush:

This was surprisingly easy for a Day 6.
I can see how it could easily go awry for someone who didn't check for
the edge case, or who didn't recall how the decimal system works.

#### Ada

Satisfying the compiler in Part 2 was tedious, but not especially hard.

#### Rust

Translating the Ada to Rust was straightforward.

## Day 7: Camel Cards

You're playing cards with an elf on a camel, whiling away the time
as he transports you across Desert Island. It's a bit like poker.

1. Determine the point values of the many, many hands you're dealt.
   This requires ranking them.
1. The elf introduces new rules. Determine the new point values.

### Unusual tools

#### Ada
* Part 1 uses an enumeration's default ordering to break ties between hands.
* Part 2 uses an array over an enumeration to re-order individual cards,
   allowing us to reuse the same basic structure:
   ```
   New_Ranking : constant array (Face) of Natural :=
      [Jack => 0, One => 1, Two => 2, Three => 3, Four => 4, Five => 5,
      Six   => 6, Seven => 7, Eight => 8, Nine => 9, Ten => 10, Queen => 11,
      King  => 12, Ace => 13];
   ```

#### Rust

Man! was this a chore.

* Rust gives you nothing for free, which I don't necessarily mind,
  but having to derive

      Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord
  
  on the `Face` type seemed a bit much.
  This is one drawback to Rust's conflating ordinary enumerations
  with algebraic types.
* On the other hand (no pun intended), at least Rust _lets_ me derive `Hash`.
  Ada doesn't even offer that.
* I wasted time trying to `impl PartialOrd for Hand`,
  then getting confused and trying to `impl Ord for Hand`,
  then getting more confused because `Ord` wants a `clamp`
  and the code completion filled it in with something even _it_ didn't like,
  all the time wondering what I would do in Part when I had to change ordering,
  before I realized that I could simply `.sort_by()` and supply the desired ordering.
  Granted, that stupidity is on _me_; I knew there was a `.sort_by()` and simply forgot about it.
  Still, it was a chore, and implementing `Ord::clamp()` still scares me.
* In part 1 I was nailed by the fact that counting always starts from 0,
  and you have no way to change it,
  so you _must_ remember to add 1 to `ith` (my default indexing variable)
  when you multiply it to the bid.
* Rust does have a nice 

### Experience

* This was straightforward to do in Ada, thanks to arrays,
  sensible defaults for things like orderings,
  and the ability to index in a manner appropriate to the problem.
* It was not quite so straightforward in Rust,
  for the reasons mentioned above.

## Day 8: Haunted Wasteland

Today's introduction manages to be both melancholy and spooky.

    You're still riding a camel across Desert Island
    when you spot a sandstorm quickly approaching.
    When you turn to warn the Elf, she disappears before your eyes!
    To be fair, she had just finished warning you about ghosts a few minutes ago.

As we will see, the simultaneity of these emotions is appropriate to the puzzle.

Meanwhile, a sandstorm is approaching.
You need to find your way to the destination.
The ghost seems to have left you some maps.

1. How many steps does it take to get from `AAA` to `ZZZ`?
1. How many steps does it take to get from any node ending in `A`
   to any node ending in `Z`?

### Unusual tools

In Ada, nothing special.

### Experience

#### Ada
If I hadn't incremented `Step` in the wrong place,
both parts would have been fun and easy, but
in Part 2 I incremented `Step` after moving _each_ ghost,
rather than after moving _all_ ghosts.
That left me unable to confirm my suspicion that
they were moving in regular cycles, and we needed an `Lcm`
(which I duly copied from AoC 2019 Day 12).

Otherwise, it was fun and easy.

Out of curiosity, while it makes sense that
ghosts can move in many places simultaneously...
how am _I_ supposed to do so?
Tune in tomorrow to find out! (I guess)

#### Rust
* `read_input()`is nearly 80 lines of Rust.
* `Read_Input` is less than 30 lines of Ada.

**On the other hand!**
* The Rust is fewer lines overall, in part because...
* ...I used the `num` crate to compute `lcm`'s, where in Ada
  I simply copy-and-pasted my `lcm` code from a previous year's puzzle,
  as noted above.

**On the other other hand!**
Compare this Rust...

    n = if direction == Direction::Left {
       map[&n].left
    } else {
       map[&n].right
    };

...to this Ada.

    N := Map (N) (D);

**We report, you decide.**

## Day 9: Mirage Maintenance

We never found out "how".

After the sandstorm, you come upon an oasis.
Above you is a metal island, probably named Metal Island.
While awaiting the sunrise, you take some ecological readings
which produce sequences of values.

1. Predict the future of the oasis by extending the sequences
   one value to the right. Return the new values' sum.
1. Infer the history of the oasis by extending the sequences
   one value to the left. Return the new values' sum.

### Unusual Tools

* In Ada, I finally took Jeffrey Carter's advice and defaulted to using
  a custom Integer range: `type Value is range -2**32 .. 2**32 - 1;`

### Experience

Oddly fun and easy for a day 9. Initially I goofed on Part 2
by adding the wrong numbers, but that was because
I was modifying the former `Extend_Sequence` to be more general.

#### Rust

* The only mistake I made with the Rust was to forget
  that the data could be negative, so I stupidly used `u64` instead of `i64`.
* This is one of those occasions where the Rust is much shorter than the Ada,
  in no small part thanks to
  * **less boilerplate**
    The Ada code spends at least 10 lines defining
    `Value_IO`, `Sequence_Vectors`, `Sequence_Sequences`.
    Those basically don't exist in the Rust code.
    There are other examples of this.
  * **easier iteration**
    Rust's functional-style iteration played a huge role in
    `extend`, `part_1`, and `part_2`.
    I could arguably do something similar in Ada's
    `Part_1` and `Part_2` by employing `Reduce` with a custom reducer,
    but I don't believe it's possible to do so with `Extend` --
    or whether it's desirable even if it is.
* The Rust is also significantly faster than the Ada: 6ms vs. 36ms.
  I will try to come back to this at some point.

## Day 10: Pipe Maze

As you make your way around metal island, you spot a (metal) animal
who ducks into a pipe.
But you need his help!

1. Trace the closed-loop pipe. Determine how many steps it takes
   to get to its furthest point.
1. Determine the size of a potential nest within the closed loop.

### Unusual Tools

#### Ada
* Ada 2022's `declare` expressions, in `Can_Move`.
  Unfortunately, the language plugin for VSCode doesn't recognize these yet,
  so formatting was a hassle.
* Exception handling, at least during debugging.

### Experience

This is much more complex than the previous puzzles.
* Some sort of search in Part 2.
* _That_ needs to detect points outside the loop but accessing to the animal
  by squeezing between pipes.
* The pipe shapes are a little hard to work out..
* If you get something wrong, debugging can be a hassle.

Still, I made it harder than it needed to be.
* In my first attempt at Part 1, searching terminated by testing whether
  the two copies of the animal that follow the pipe in opposite directions
  are the same.

  **That was dumb.**
  I needed only to check whether their _locations_ were the same.
* In my first attempt at Part 2, I forgot the puzzle master's note that
  "[a]ny tile that isn't part of the main loop
   can count as being enclosed by the loop."

All that aside, it's a very good puzzle.
I just hope we get some easier ones the next couple of days. :grin:

### Visualization

Finally! our first visualization of the year.

![visualization.png](day10/visualization.png)

## Day 11: Cosmic Expansion

You come upon an observatory, where
a researcher offers to help you as soon as he finishes his research project.
He has an image of part of the universe,
and wants to sum the distances between the observed galaxies.

1. Do this after doubling each row or column of empty space.
1. The universe is older than that.
   Instead of replacing each row or column by two,
   replace by one million.

### Tools

Just a bit of cleverness.

### Experience

Fun and, after yesterday, gratifyingly easy.

#### That aside, what happened to the animal?

We spent yesterday trying to find its nest, and
today there's not so much as a word breathed about it.
I still miss the ghost from Days 7 and 8,
and now I lose the animal, too?

Completing the Advent of Code is sometimes like reading a Neil Gaiman story:
one day you meet intriguing, sympathetic characters,
and the next day they're forgotten, as if they never really matter.

:cry: