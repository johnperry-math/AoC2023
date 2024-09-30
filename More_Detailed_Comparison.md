# A Comparison of Ada and Rust, using solutions to the Advent of Code

[Jeremy Grosser at forum.ada-lang.io](https://forum.ada-lang.io/t/advent-of-code-2023/540), invited comparisons of 2023 Advent of Code puzzles in different languages that have an angle on "safety", so I decided to try it with Ada, Rust, and Modula-2. So far I've completed all the puzzles in Ada and Rust, and three in Modula-2. While I've detailed a few observations [here](README.md) and [there](https://forum.ada-lang.io/t/irenic-language-comparisons-and-questions-ada-and-rust-aoc-2023-day-13/638), they were all on a puzzle-by-puzzle basis, so now that I've completed them all in Ada and Rust, I'd like to step back and take a more general, "higher-level" view.

## Table of Contents

* [Caveats and Disclaimers](#caveats-and-disclaimers)
  * [Which version of the language?](#which-version-of-the-language)
  * [Why didn't you discuss Feature X? It's the language's "killer app"!](#why-didnt-you-discuss-feature-x-its-the-languages-killer-app)
  * [Perhaps not the best tour guide?](#perhaps-not-the-best-tour-guide)
* [Language overviews](#language-overviews)
  * [Ada](#ada)
  * [Rust](#rust)
  * [Similarities](#similarities)
  * [Differences](#differences)
* [Case study 1: File iteration and processing with error handling](#case-study-1-file-iteration-and-processing-with-error-handling)
* [Case study 2: Modularity and generics](#case-study-2-modularity-and-generics)
* [Case study 3: Enumerations](#case-study-3-enumerations)
* [Case study 4: Filtered and enumerated iteration](#case-study-4-filtered-and-enumerated-iteration)

Each Case study will include a few words about aspects I find advantageous in each language.

## Caveats and Disclaimers

### Which version of the language?

Ada and Rust have multiple specifications. The versions used for comparison here are

* Ada 2022
  * This includes an occasional reference to Ada SPARK 2014 on account of its more stringent rules.
* Rust 2021

### Why didn't you discuss Feature X? It's the language's "killer app"!

I can already hear a couple of people rage-typing that I've omitted their favorite language's most valuable feature. So why didn't I include it here? Possible answers include:

* I did, but it's not a section heading.
* It didn't seem important in my solutions.
* I'm kind of worn out after all the things I *did* discuss.
* I dunno. I just didn't think of it. Why, yes, I am a horrible human being.

You might convince me to include a bit on that feature! Contact me (use GitHub to submit PR, or create an Issue) Submit a request and I'll consider it. Just don't rage type it.

### Perhaps not the best tour guide?

* I speak both Ada and Rust, but I've met "native" speakers of those languages and my [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) / [Modula-2](https://en.wikipedia.org/wiki/Modula-2) accent is all too apparent. Code excerpts should accordingly be considered usable but possibly non-idiomatic and quite possibly suboptimal. I do learn from other people's solutions from time to time.
* My solutions may not be optimal, or even advisable. I don't generally look at other people's solutions, or even the discussion of solutions, unless I've solved mine or I'm so hopelessly stuck that I'm looking for the "magic bullet" insight that some puzzles require.

## Language overviews

First, some comments on the languages, how I came to be familiar with them, and the depth of my experience with them.

### Ada

Ada is a multi-paradigm, "kitchen-sink" language with an emphasis on safe, secure programming. It's often used in [situations where correctness, reliability, and large-scale development are the primary concern](https://www.adacore.com/industries). There is an open-source, GPL'd Ada compiler which stays on the language's cutting edge, but vendors have been around for decades.

Ada's arguably the best language that you've either:

* never heard of, or
* heard someone disparage it wrongly, claiming it's:
  * dead,
  * complicated, and/or
  * far too wordy.

I don't consider myself an Ada expert by any means, [but](https://github.com/johnperry-math/PL-0-in-Ada) [I](https://github.com/johnperry-math/GtkAdaQuoter) [have](https://github.com/johnperry-math/RosettaCode) [written](https://github.com/johnperry-math/AoC2020) [some](https://github.com/johnperry-math/AoC2020) [non](https://github.com/johnperry-math/AoC2022)-[trivial](https://github.com/johnperry-math/AoC2022) [programs](https://github.com/johnperry-math/AoC2021) [in](https://github.com/johnperry-math/AoC2018) [it](https://github.com/johnperry-math/AoC2023).

#### History

Ada's origins lie in the US Department of Defense, which famously wanted to replace the hundreds or thousands of custom-designed languages in most of its projects with one high-level programming language. A competitive bidding process with international competitors led to the eventual specification of what we now call Ada 83.

From 1987 through 1997, the Department of Defense "required" the use of Ada in all new software, but waivers were available and frequently used. I have met DoD employees from the 90s who heard of both Ada and the mandate, but have never seen Ada code. After 1997 DoD relented, so that C++ and Java came to eclipse Ada.

Subsequent revisions to the language in 1995, 2002, 2007, 20012, and 2022, each with an ISO standard. Ada users like to point out that Ada was the first programming language whose object-oriented had an ISO standard specification.

#### SPARK

A dialect of Ada called SPARK offers additional features and guarantees, though there are restrictions. I wouldn't ordinarily mention a dialect of a language, but Spark is a pretty big deal in the Ada world.

### Rust

Rust is more opinionated than Ada seems to be, in that some of Rust's more interesting "features" are what its designers *declined* to include in the language. Unlike most languages designed since the 1970s, Rust has turned its back on

* object-oriented programming and inheritance, at least via `class` mechanisms of inheritance;
* garbage collection (depending on how one defines the term);
* null pointers, AKA [the billion-dollar mistake](https://en.wikipedia.org/wiki/Null_pointer#History).

(Probably not a copmrehensive list.)

I won't say that Rust is the best language you *have* heard of, but it is the language that StackOverflow users [keep](https://survey.stackoverflow.co/2024/technology#admired-and-desired) voting, [year](https://survey.stackoverflow.co/2023/#technology-admired-and-desired) after [year](https://survey.stackoverflow.co/2022/#technology-most-popular-technologies), as their most admired programming language, and I use it at work, and I enjoy using it. Mostly. Beats C and C++, anyway, which I also use at work (and [wrote a bit](https://github.com/johnperry-math/DynGB) of in the past).

I've been working in Rust a lot the past three years, but most of that is owned by my employer, so I can only show off, though perhaps I shouldn't 😳, only [my solutions to the 2023 Advent of Code](https://github.com/johnperry-math/AoC2023). Despite that, I don't consider myself an expert; I'm still uncomfortable and inexperienced with tools such as `Box`, `Arc`, and lifetimes. That said, it says something **very positive** about Rust that I haven't needed to exercise myself in those areas much.

(It also says something very positive about our lead developers, but that's another matter.)

#### History

Rust began at Mozilla as a project of [Graydon Hoare](https://graydon2.dreamwidth.org/307291.html)'s. (It's quite the irony that [he is not related](https://news.ycombinator.com/item?id=12613295) to [Tony Hoare](https://en.wikipedia.org/wiki/Tony_Hoare).) It has a reputation for being

* difficult to learn,
* difficult to use, but
* paying off immensely.

However, Rust is not a minimalist language; some of its special features include:

* the famous (and feared) borrow checker, and
* a highly developed macro language for compile-time code generation.

Mozilla used Rust in several projects, such as rewriting [Firefox's CSS engine](https://searchfox.org/mozilla-central/source/servo) in Rust.

### Similarities

Both Ada and Rust are:

* systems programming languages,
* designed with the goal of safe programming,
* encouraging stack-centric programming idioms,
* developed and used in major projects at a major organization (US DoD for Ada, Mozilla for Rust),
* later released to a community organization (Ada Rapporteur Group, Rust Foundation).

### Differences

I'll enumerate only major differences here. More details will appear below. There's no way I'll touch on all of them.

#### General differences

* Ada and Rust have different notions of safety.
* Ada relies on a very detailed, carefully considered specification that is an ISO standard determined by a committee of academic and industry experts. The Ada Reference Manual is [freely available and downloadable online](http://www.ada-auth.org/standards/22rm/html/RM-TTL.html). Parts of it can be difficult to read, but I use it routinely as a reference and discussions of Ada frequently refer back to the ARM.
* Rust's specification seems to be [The Rust Reference](https://doc.rust-lang.org/reference/index.html) and/or the compiler.
  * The website explicitly states that The Rust Reference "is not a formal spec".

    > Finally, this book is not normative. It may include details that are specific to `rustc` itself, and should not be taken as a specification for the Rust language. We intend to produce such a book someday, and until then, the reference is the closest thing we have to one.
    >
  * The standard library's documentation is [separate](https://doc.rust-lang.org/std/index.html) from the language reference.

#### Feature differences

The reader should keep in mind that the following is meant to indicate whether something is built into the language and available immediately, *not* whether you can find it in a library, regardless of how robust the library is. That means [tokio](https://tokio.rs/) and [GNAT extensions to Ada](https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/gnat_language_extensions.html) don't count.

✔️ default

📝 default for debug mode; opt-in for release mode

📖 available with a library not in `Ada.` / `std::`

❌ unavailable


| topic                                                     | Ada  | SPARK | Rust | Notes                                                                                                                                                                                                                        |
| ----------------------------------------------------------- | ------ | ------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| array bounds checking                                     | ✔️ | ✔️  | ✔️ | all terminate gracefully with a useful error message on a bounds violation                                                                                                                                                   |
| automatic detection and collection of unused heap objects | ❌   | ❌    | ✔️ | I'd call this "built-in garbage collection", which it is, but people would misunderstand me.                                                                                                                                 |
| concurrent programming / tasking                          | ✔️ | ✔️  | 📖   | Rust's`async` requires a non-standard library                                                                                                                                                                                |
| containers                                                | ✔️ | ✔️  | ✔️ | Rust's containers are much nicer to use                                                                                                                                                                                      |
| design by contract (DBC)                                  | ✔️ | ✔️  | ❌   | one or two crates exist for Rust, but are not well-developed                                                                                                                                                                 |
| DBC compile-time verification                             | ❌   | ✔️  | ❌   | **not** easy                                                                                                                                                                                                                 |
| exception handling                                        | ✔️ | ✔️  | ❌   | for Rust, see "optional" and "result" values below                                                                                                                                                                           |
| functional chaining                                       | ❌   | ❌    | ✔️ | whether use of the chaining idiom is available for all types                                                                                                                                                                 |
| functional purity                                         | ❌   | ✔️  | ✔️ | whether the language specifies allows function to have side effects*only* via an opt-in mechanism                                                                                                                            |
| global variables                                          | ✔️ | ✔️  | ❌   | Rust considers this a safety issue;*all* variables must be local to a function. Ada allows global variables in a compilation unit, such as a package.                                                                        |
| indexing by any discrete type                             | ✔️ | ✔️  | ❌   | e.g.,`type Direction is (Up, Dn, Lt, Rt); type Destinations is array (Direction) of Location;`                                                                                                                               |
| integer overflow                                          | ✔️ | ✔️  | 📝   |                                                                                                                                                                                                                              |
| memory safety                                             | ❌   | ✔️  | ✔️ | i can't possibly go into the details of this here, but: in Ada it is possible to reference a`null` pointer at run-time; in both Spark and Rust this violates the languaeg specification (or you're engaged in `unsafe` Rust) |
| macros                                                    | ❌   | ❌    | ✔️ |                                                                                                                                                                                                                              |
| "optional" values                                         | ❌   | ❌    | ✔️ |                                                                                                                                                                                                                              |
| pattern matching                                          | ❌   | ❌    | ✔️ | e.g.,`if let Some(thing) = fnctn_with_optional_result(...)`                                                                                                                                                                  |
| "result" values                                           | ❌   | ❌    | ✔️ |                                                                                                                                                                                                                              |
| ranges and subranges as types                             | ✔️ | ✔️  | ❌   | e.g.,`type Digit is new Positive range 0..9`                                                                                                                                                                                 |
|                                                           |      |       |      |                                                                                                                                                                                                                              |
|                                                           |      |       |      |                                                                                                                                                                                                                              |

### The elephant in the room

I haven't mentioned "speed".

* Rust has a well-deserved reputation for fast execution times. It has a less well-deserved reputation for slow compilation times. Without going into too much detail: at work we have a Rust codebase with a C++ bridge. Building the C++ files always, *always* takes disproportionately more time than the Rust.
* With Ada the situation is less clear. It compiles very, very quickly, but:

  * It has a reputation for slow execution.
  * This is due primarily to the amount of run-time checks. Rust has some run-time checks, too; see "array bounds checking" above.
  * The compiler can detect at compile-time many circumstances where it can remove run-time checks.
  * When other languages include the same run-time checks Ada does, differences in speed typically disappear.

Almost all my solutions run quickly on my machine, even in "debug" mode, but one or two take quite a few seconds, and perhaps even a few minutes. I've seen Ada programs run faster than Rust programs and vice-versa. For what it's worth, here are a few samples plucked at random from the latter few days, when things tend to get more complicated. Times are an average of 3 runs, measured in seconds, except for day 23, which is just one run.


| day | Ada development | Ada release | Rust debug    | Rust release |
| ----- | ----------------- | ------------- | --------------- | -------------- |
| 17  | 8.65            | 7.28        | 15.34         | 4.45         |
| 20  | 0.38            | 0.33        | 0.44          | 0.36         |
| 21  | 7.02            | 4.471       | 16.28         | 1.18         |
| 22  | 0.32            | 0.28        | 2.14          | 0.21         |
| 23  | 377.            | 251.        | crashed (?!?) | 87.0         |
| 24  | 0.03            | 0.02        | 1.60          | 0.19         |
| 25  | 6.50            | 5.22        | 10.6          | 1.16         |

In general, Rust has both the slowest and the fastest runtimes. The results almost certainly have more to do with the following than anything inherent to the language.

#### Compiler and linker options

Cargo's release mode disables overflow checking (`overflow-checks = false`) but overflow checking is required for all builds of an Ada compiler. Overflow checking involves a real performance hit.

#### Design of library code

The one puzzle where Ada significantly outperformed Rust occurred on Day 24. The reason for this was that the Ada compiler recognized that

number of checks performed at runtime.

## Case Study 1: File iteration and processing, with error handling

Advent of Code requires file processing of an input file. The processing is sometimes done twice. The file doesn't require modification, merely reading and parsing. While the puzzles do provide information required to parse the input, you almost never know:

* the dimensions of the data,
* the number or full range of symbols, nor
* the ranges of integer values.

### Ada

Opening the input is relatively straightforward; each of my soutions has something along these lines.

```ada
with Ada.Text_IO;
--  ... snip ...
package IO renames Ada.Text_IO;
--  the above line is not necessary, but I prefer it to use statements
--  and to repeatedly typing out Ada.Text_IO.Something_Else
--  ... snip ...
declare --  this line not required at the beginning of a subprogram
   F : IO.File_Type;
begin
   IO.Open (F, IO.In_File, "input.txt");
   while not IO.End_Of_File (F) loop
      declare
         Line : String := IO.Get_Line (F);
         Position : Positive := Line'First;
      begin
         --  processing goes here
      end;
   end loop;
end;
A `String` is an `array` of `Character`, which itself is the traditional 8-bit byte, so the processing can loop through the line if needed:

```ada
         for Letter of String loop
            --  do something with Letter
         end loop;
```
...or one can extract substrings according to pre-determined ranges (this happens at times), patterns, types, etc.:

```ada
         First_Symbol := Line (3..5);
         --  if one has instantiated Integer_IO as Ada.Text_IO.Integer_IO with an appropriate numerical type:
         Integer_IO.Get (Line (Position..Line'Last), Integer_Value, Position);
         --  if one has instantiated Enum_IO as Ada.Text_IO.Enum_IO with an appropriate enumeration type:
         Enum_IO.Get (Line (Position..Line'Last), Enum_Value, Position);
         --  in each of the previous two cases, Position will be updated to the last index read,
         --  and 'Last is an attribute that indicates the last valid index of an array
```
### Rust

#### My approach

It's not so hard to open the file itself (nor as verbose as Ada):

```rust
let file = std::fs::File::open("input.txt").expect("where's my input?!?");
// i could .unwrap() above instead, but unwraps are less ideal; expects give more information
let lines = std::io::BufReader::new(file).lines();
for line in lines {
    // line is a `Result` type, so we have to extract the line
    let line = line.expect("how'd i get a line that's not a line?!?");
    // processing goes here
}
```
What happens next is more involved. Unlike Ada, Rust does not allow you to iterate through the characters of a `String`; the compiler objects:

```
`std::string::String` is not an iterator
the trait `std::iter::Iterator` is not implemented for `std::string::String`, which is required by `std::string::String: std::iter::IntoIterator`
```
Neither can you index your way into a `String`. You must iterate instead. I'll write more about iterators below, but Rust's iterators are **well-designed and readable**:

```rust
let mut chars = line.chars().skip(3);
// the above turns line into an iterator of characters, then skips the first 3
let first_symbol = [
    chars.next().unwrap(),
    chars.next().unwrap(),
    chars.next().unwrap()
];
// the above takes the "next" item from chars, 3 times;
// since this may fail, you receive an Option result, which explains the .unwrap

// or
let mut split = line.split(" @ ");
// the above turns line into an iterator of tokens separated by the string " @ "
let integer_value = split
    .next()
    .unwrap()
    .parse::<usize>()
    .expect("unable to parse to a usize!");
```
Enumerations take more work to parse in Rust. In part, that's because Rust's enumerations are somewhat powerful and complex that your run-of-the-mill enumeration. I don't seem to have needed enumeration I/O in this year's puzzles, but Rust will require you to use an external package, someting like `serde` ("serialization/deserialziation") or `aho_corasick` (sophisticated pattern-based parsing). We use `serde` at work **all the time** and it's not difficult to use at all. I experimented with Aho-Corasick on Day 1 and found it useful, but never used it again.

#### Another approach

I came of "software age" in the 80s and 90s, so I sometimes try to speak Rust in foreign idioms. While writing this I looked up another solution or two. [The following](https://github.com/theoludwig/advent_of_code_2023) resembles what I've often seen "native" Rustaceans produce:

```rust
fn main() {
    let input = include_str!("../input.txt");
    // ... snip ...
}

pub fn part_1(input: &str) -> usize {
    input
        .lines()
        .map(|line| {
            let characters_digits = line
                .chars()
                .filter(|&character| character.is_ascii_digit())
                .collect::<Vec<char>>();

            let first_digit = characters_digits.first().unwrap_or(&'0').to_owned();
            let last_digit = characters_digits.last().unwrap_or(&'0').to_owned();
            let number = format!("{}{}", first_digit, last_digit);
            let number: usize = number.parse().expect("Should parse as a `usize`.");
            number
        })
        .sum()
}
```
This seems more idiomatic than my approach:

* The author reads the input using the `include_str!` macro; that is, he basically pastes it into his program. I wouldn't do that, but it's a nice example of using a macro for quick, convenient, compile-time code generation.
* This code relies on an iterator's `.map` method. I'll write more about this below, but essentially:
  * the `input`
  * is passed line-by-line, thanks to the `.lines` method, which returns an iterator,
  * to the `.map` method, which
    * accepts the result of one iteration as its input, `line`, and then
    * then executes several statements in a closure, denoted by `{ ... }`, with the aim of transforming `line` into something else, in this case an integer, after which
  * the `.map`'s results are `.sum`med.
* The lack of a semicolon on the function's last line indicates that that last line's result is the function's result.

The several lines within the `.map` closure are imperative in style, rather than functional.

* The author used `.chars` to iterate through the line's characters.
* A `.filter` keeps only ascii digits, and a `.collect` turns them into a `Vec<char>`.
* Much as there was no semicolon after the `.sum()`, the lack of a semicolon after the last instance of `number` indicates that the closure's result passes to the next iterator method.
* All of this is done *for each input `line`*!

### Comments

The Ada is straightforward and intuitive, though my comments make it look wordy. At least on file handling, Ada's reputation for verbosity seems a little undeserved.

The Rust is fine; I can certainly read the general outline of the Rust without too much trouble, and I can appreciate the elegance in it.

#### Points to Ada

Even after three years of using Rust, I usually prefer imperative style (more on this below) and this seems like a good example. Rust's functional style takes getting used to. I've seen closures before and am not averse to them, at least when they're short, but the inline nesting of code through closures can start to look like a [Pyramid of Doom](https://en.wikipedia.org/wiki/Pyramid_of_doom_(programming)#Resolution).

The inability to iterate directly over the `String`'s 8-bit elements with a `for` loop, *even though that's what the string holds*, made me feel as if I had to work harder to accomplish some pretty simple things in Rust. The tools I used were more powerful, for sure! But it felt like more work.

#### Points to Rust

The Rust code above was unable to examine the `Line` or `Char` until it extracted it from the `Result` type via `.unwrap` or, preferably, `.expect`, forcing the programmer at least to *think about* the possibility of an error when opening the file. Rust code does routinely; its philosophy is that, when the program encounters an exceptional condition, the system either

* `.panic!`s (a bad thing!), or
* returns an `Option` or `Result`type, so the developer to handle them at an appropriate time.

It often happens that handling an error would be inappropriate in a certain place; in this case, Rust forces the programmer to indicate this using a `?` operator. Thus, I could have done this:

```rust
fn read_input(filename: String) -> Result<Expected_Data_Type, std::io::Error> {
   let file = std::fs::File::open("input.txt")?;
   // and so forth
}
```
**The potential for an error appears right there in the function signature.** By contrast, Ada lets me do this:

```ada
function Read_Input (Filename : String) return Expected_Data_Type is
   Input : IO.File_Type;
begin
   IO.Open (Input, IO.In_File, Filename);
   -- and so forth
end Read_Input;
```
I could forget to handle it. Worse, if this is library code, clients might never be aware the exception could occur! And I certainly *have* forgotten to handle it in cases where it occurs.

SPARK's handling of exceptions is more sophisticated, but I'm not familiar enough with it to comment at this time. (**Look into this.**)

## Case Study 2: Modularity and generics

Advent of Code puzzles frequently involve:

* reading and navigating a map, and
* very large numbers, where one must often compute a least common multiple.

I organized some reusable code for this into a separate file or two. I've seen quite a few others do things like this.

### Terminology

* Ada has historically called this unit of organization a `package`.
* Rust calls them `mod`ules, but these are so often distributed via the `cargo` tool, which calls its packages "crates", that in my experience the term "crate" is used more often.
* The Ada community has deveoped its own package manager, [alire](https://alire.ada.dev/), which also calls its packages "crates".

### General structure

For each language, the `common` package / module will offer sub-package / sub-modules that:

* describe a two-dimensional map structure, including appropriate and useful types and functions such as cell location (row & column);
* describe two-dimensional motion in a map, including appropriate types and constants, an enumeration for directions, **and** an `Opposite` function that handles that enumeration appropriately;
  and
* enable the reading of a generalized, two-dimensional map, making use of serialization and deserialization functions specified by the client.

The Ada package also offers functions to compute the greatest common divisor and the least common multiple of a generic type that has the operations required for this. Apparently I didn't find that necessary for Rust, whose standard library tends to offer many more features.

### Modularity: specification and implementation

#### Ada

Ada requires every package to separate the specification from the implementation. This was not uncommmon for languages designed from the late 60s through the mid-80s: C, C++, Modula-2, and Modula-3 also offer some version of this feature.

The specification appears as:

```ada
package Common is
   --  snip
end Common;
```
The implementation appears as:

```ada
package body Common is
   --  snip
end Common;
```
Almost no executable code may take place within the specification; Ada 2022 allows only the exception of functions that return an expression: for example,

```ada
function Opposite (Left, Right : Direction) return Boolean is
      --  returns True iff Left and Right are opposite cardinal directions

        (case Left is when North => Right = South, when South => Right = North,
           when East => Right = West, when West => Right = East);
```
Otherwise, specifications are limited to declaring types, variables, and subprograms in the package's public interface.

If you want a public `record` type, but don't want to expose its fields, you can delcare the type as `private`, then define the fields in the specification's `private` section.

Packages can have children, which you can think of as "sub-packages". They can appear in the same file or separate files. My `Common` package has four children, `Two_Dimensional_Motion`, `Two_Dimensional_Map`, `Two_Dimensional_IO`, and `Mathematics`.

To access a package's public interface, the client must first `with` the package:

```ada
with Ada.Text_IO;

with Common;
with Common.Two_Dimensional_Motion;
```
Subsequently, its contents are available if you prefix with the package name:

```ada
if not Common.Two_Dimensional_Motion.Opposite
   (Common.Two_Dimensional_Motion.North, Common.Two_Dimensional_Motion.South)
then
   Ada.Text_IO.Put_Line ("I broke the world!");
end if;
```
That can get a little hard to read, so there are two ways to make it easier. The first is to `use` a package:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Common;
with Common.Two_Dimensional_Motion; use Common.Two_Dimensional_Motion;
--  snip

if not Opposite (North, South) then
   Put_Line ("I broke the world!");
end if;
```
However, that can lead to compile-time errors when packages use the same names, so a second approach is to `rename` a package. It's also possible to `use` a type, and to combine the approaches.

```ada
--  snip (omitting our with's)

package Motion_2D renames Common.Two_Dimensional_Motion;
use all type Motion_2D.Direction;
--  snip

if not Motion_2D.Opposite (North, South) then
   IO.Put_Line ("I broke the world!");
end if;

```
I prefer this approach because it makes it clear to the reader where a function comes from, while keeping the verbosity low. Sure, I could use the IDE to click into the function and find its package, but that indirection interrupts my thinking.

That said, my impression is that this is pretty rare: most Ada programs I've read adopt the `use` approach.

#### Rust

Rust is like every language I've seen since the mid-80s: the programmer writes only the implementation file, and:

* the compiler produces a machine-readable specification for its purposes, while
* a separate documentation generator, a la `javadoc` produces human-readable documentation.

Rust modules don't declare a private section; everything is private by default. To expose something to a client, you use `pub`. For example:

```rust
pub enum Direction {
    North,
    South,
    East,
    West,
}
```
Just as Ada packages can contain children, Rust modules can contain sub-modules.

You do not have to `with` a Rust module that is declared in the same crate or, if you are using `cargo`, made available through `Cargo.toml`. Simply access them, using `::` to enter a scope:

```rust
if !common::two_dimensional_motion::opposite(
    common::two_dimensional_motion::Direction::North,
    common::two_dimensional_motion::Direction::South
) {
    println!("I broke the world!);
}
```
That's pretty hard to read, and Rust provides the same two ways of getting around it as Ada. One is to `use` a feature:

```rust
use common::two_dimensional_motion::{Direction::{North, South}, opposite};
// snip

if !opposite(North, South) {
    println!("I broke the world!");
}
```
The other is to rename. This requires using the `use` feature along with `as`. You can of course mix the two, as well.

```rust
use common::{two_dimension_motion as motion_2d};
use motion_2d::Direction::{North, South}

if !motion_2d::opposite(North, South) {
   println!("I broke the world!");
}
```
#### Unit testing

Perusees of `common.rs` will notice that it contains a submodule named `tests`:

```rust
#[cfg(test)]
mod tests {
    use crate::{two_dimensional_map::Location, two_dimensional_motion::Direction};

    #[test]
    fn location_deltas() {
        // etc.
```
The annotations indicate:

* `#[cfg(test)]`: Build the following unit only while the `test` feature is active (e.g., when running `cargo test`). This annotation can appear before any declaration, not only before modules.
* `#[test]`: Execute the next function while testing.

Several lines within `location_deltas` assert certain results that test the desired behavior; if someone (i.e., yours truly) should decide one day to "optimize" the tested behaviors, and this "optimization" in reality changes the behavior, then running `cargo test` will reveal this very quickly. Rust's `cargo`-based build system makes it quite easy to add tests like this.

To my knowledge, Ada has no equivalent capability. Unit testing is possible, and I have employed it in a project or two, but it requires a different approach; for exapmle, [AUnit](https://www.adacore.com/documentation/aunit-cookbook).

### Generics: specification and instantiatiation

When a puzzle has a map, the features vary from puzzle to puzzle. Representing them by a generic parameter seemed appropriate for a `Map` structure meant for as many puzzles as possible.

While I was at it, I made the map's dimensions into generic parameters.

#### Ada

With Ada you can declare as generic either packages or procedures, and nothing else. As a consequence, we the map type itself can't be directly generic; rather, the package containing it has to be generic.

```ada
generic
   Row_Length, Col_Length : Positive;
   type Object is private;

package Two_Dimensional_Map is

      subtype Row_Range is Natural range 1 .. Row_Length;
      subtype Col_Range is Natural range 1 .. Col_Length;

      type Map_Array is array (Row_Range, Col_Range) of Object;

--  etc.
```
To use a generic package, you have to instantiate it. Here's an example from day 21's solution:

```rust
type Object is (Plot, Rock, Start);
Side_Length : constant Positive := (if Doing_Example then 11 else 131);
package Map_Package is new Common.Two_Dimensional_Map
  (Row_Length => Side_Length, Col_Length => Side_Length, Object => Object);
--  in that last line, the first Object refers to the generic parameter;
--  the second Object refers to the type declared in the first line
```
After these declarations, `Map_Package.Row_Range` and `Map_Package.Col_Range` represent the ranges from 1 through 11 or 131, depending on whether the code is testing the example or solving the puzzle. (Ada's ranges are inclusive by default.) In addition, `Map_Package.Row_Length` and `Map_Package.Row_Column` are accessible values, and you can now declare a value of type `Map_Package.Map_Array`. As usual, you can `use Map_Package` if you don't want to prefix all those things with the package name.

### Rust

With Rust you follow the more common approach of declaring types as generic.

```rust
// declaration

pub mod two_dimensional_map { // not generic!

    pub struct Map<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> {
        locations: [[Object; COL_LENGTH]; ROW_LENGTH],
    }

// etc.
}

// instantiation

enum Object {
    #[default]
    Plot,
    Rock,
    Start,
}
const SIDE_LENGTH: usize = 131;
type Map = common::two_dimensional_map::Map<SIDE_LENGTH, SIDE_LENGTH, Object>;
```
Unlike Ada, `SIDE_LENGTH` does not become a proper field for the `Map` type. That doesn't mean you can't know what the map's dimensions are; if you don't already have `SIDE_LENGTH` handy then you can add a function to return the value; for example,

```rust
impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> Map<ROW_LENGTH, COL_LENGTH, Object> {
    pub const fn row_dimension(&self) -> usize {
        ROW_LENGTH
    }
}
```
## Case Study 3: Enumerations

Ada and Rust both have enumeration types, but while there are similarities, ultimately they mean different things.

The example I'll use for enumerations here is the notion of direction in a map: north, south, east, and west.

### Ada

Declaring an enumeration in Ada is brief:

```ada
type Direction is (North, South, East, West);
```
The enumeration's literals are available directly in its scope:

```ada
type Direction is (North, South, East, West);

Orientation: Direction := North;
```
Enumerations automatically receive the treatment of a linearly ordered, discrete type:

```ada
North < South < East < West
```
You can iterate through them, as I often do when performing Breadth-First Search exploration:

```ada
for Dir in Direction loop
```
You can even index arrays based on them:

```ada
type Destinations is array (Direction) of Location;
```
Combining these abilities leads to some very nice, understandable code. Consider the following example from Day 16:

```ada
Bg_Color :=
   (if (for some Dir in Direction => Energized (Row, Col) (Dir))
    then Energized_Color
    else Empty_Color);

```
"If the `Energized` array is `True` in some direction, then assign `Energized_color` to `Bg_Color`; otherwise, use `Empty_Color`." Can't quite put a finger on why, but ever since I've learned to write code like that, I've enjoyed it.

### Rust

Rust has enumerations, as well, so I can declare directions:

```rust
    pub enum Direction {
        North,
        South,
        East,
        West,
    }

```
Unlike Ada's enumerations, you *do not* get a linear ordering, an iteration, or array indexing for free. You don't get the ability to print them out for free. You don't get the ability to copy the value of one enumeration to another for free (i.e., `let enum2 = enum1`). You don't even get the ability to test for equality! You have to implement those features via traits, though Rust's macro system lets you get some of it *almost* for free. The following declaration gives me a `Direction` type that I can print in debug format, test for equality, and copy from one value to another.

```rust
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Direction {
        North,
        South,
        East,
        West,
    }
```
Using them for iteration takes a little more work. I implemented it by creating a constant array, over which it is possible to iterate, as I often do:

```
    // in the common crate
    pub const ALL_DIRECTIONS: [Direction; 4] = [
        Direction::North,
        Direction::South,
        Direction::East,
        Direction::West,
    ];

    // in various puzzle solutions
    for dir in ALL_DIRECTIONS {
        // ...
    }

```
There are ways around this, such as [this tool in the strum_macros](https://docs.rs/strum_macros/latest/strum_macros/derive.EnumIter.html) crate, but they might not work in every circumstance.

The reason for this complexity is that Rust's enumerations are *not just enumerations*; their variants can store data:

```rust

struct MyEnum {
   Variant1 { field1: usize, },
   Variant2 { field2: isize, },
}
// then, later,
let u = MyEnum::Variant1 { field1: 10 };
let v = MyEnum::Variant2 { field2 : -10 };
// etc.
```
This is very useful in practice, especially when combined with Rust's pattern-matching facilities.

Ada programmers will recognize that they can do this, too; the mechanism is simply different. In Ada, it's called a discriminated type:

```ada
type My_Enum_Variants is (Variant_1, Variant_2);
type My_Enum (Variant : My_Enum_Variants) is record
   case Variant is
      when Variant_1 => Field_1 : Natural;
      when Variant_2 => Field_2 : Integer;
      --  those types don't match but this writeup is long enough, doncha think?
   end case;
end record;
--  then, later,
declare
   U : My_Enum := (Variant => Variant_1, Field_1 => 10);
   V : My_Enum := (Variant => Variant_2, Field_2 => -10);
--  etc.
```
However, Ada lacks Rust's pattern-matching, so the Ada version of Rust's

```rust
if let My_Enum::Variant1 { field1 } = v { // this will fail, given the declarations above
```
...would be

```ada
if V.Variant = Variant_1 then --  this will fail, given teh declarations above
   declare
      Field_1 := V.Field_1;
   begin
```
That may not look too bad, but the patterns can be fairly involved sometimes. For instance, suppose you have two variables which may or may not contain useful data, and you need to do different things depending on their values:

```rust
// assuming first and second are of types Option<T> and Option<U>, respectively
match (first, second) {
    (Some(this), Some(that)) => // do something with this and that
    (Some(this), None)       => // do something with this
    (None, Some(that))       => // do something with that
    (None, None)             => // do nothing, or something else entirely; I dunno
}
```
Ada doesn't have a comparable `Option` type in its standard library. It isn't hard to write and use one, but the pattern-matching isn't there. (Neither are tuples, let alone anonymous tuples.) Accomplishing the above becomes much more involved:

```ada
--  here we use a hypothetical implementation of Option as a discriminated type
--  where Valid is a Boolean that, when true, has a Value
if First.Valid then
   declare
      This renames First.Value;
   begin
      if Second.Valid then
         declare
            That renames Second.Value;
         begin
            --  do something with This and That
         end;
      else
         --  do something with This
      end if;
   end;
elsif Second.Valid then
   declare
      That renames Second.Value;
   begin
      --  do something with That
   end;
else
   --  do nothing, or something else entirely; I dunno
end if;
```
Great: a pyramid of doom.

In short, there are tradeoffs: the ability of Rust enumerations to carry data, along with Rust's requirement that no value ever be uninitialized, means you can't iterate safely through all enumerations, so you can't iterate safely through any enumeration unless you use an external package *and* all the enum's variants' contents have default values. On the other hand, Rust's pattern matching can help made code briefer without sacrificing readability.

Those who peruse the Rust solutions will find three places where I used `if let Some(thing)` and none where I used `match` with non-trivial pattern matching, but that's more a reflection of the approach I used to the Advent of Code puzzles, especially since I was translating Ada solutions. At work we use non-trivial pattern matching quite a bit.

## Case Study 4: Filtered and enumerated iteration

One frequently needs to iterate through a container, while indexing, pruning, and manipulating values.
Part 1 of Day 14 offers a good example of how the two languages do this differently.
The Ada implementation uses an Ada 2022 feature on line 2:
```ada
for Row in System'Range (1) loop
   for Col in System'Range (2) when System (Row, Col) = Movable loop

      declare
         Travel_Row         : Natural := Row;
         Movable_In_The_Way : Natural := 0;
      begin

         while Travel_Row > 1
            and then System (Travel_Row - 1, Col) /= Immovable
         loop

            Travel_Row := @ - 1;
            if System (Travel_Row, Col) = Movable then
               Movable_In_The_Way := @ + 1;
            end if;

         end loop;

         Result :=
            @ +
            Load (Side_Length - (Travel_Row + Movable_In_The_Way) + 1);

      end;
   end loop;
end loop;
```
```rust
system.iter().enumerate().fold(0, |result, (ith, row)| {
   result
      + row
            .iter()
            .enumerate()
            .filter(|(_, object)| **object == Object::Movable)
            .map(|(jth, _)| {
               let (travel_row, movable_in_the_way) = (0..ith)
                  .rev()
                  .take_while(|kth| system[*kth][jth] != Object::Immovable)
                  .fold((ith, 0), |(row, num_movable), kth| {
                        (
                           row - 1,
                           if system[kth][jth] == Object::Movable {
                              num_movable + 1
                           } else {
                              num_movable
                           },
                        )
                  });
               SIDE_LENGTH - ((travel_row + 1) + movable_in_the_way) + 1
            })
            .sum::<usize>()
})
```
With the Rust, almost everything is doable through functions, many of which take closures as arguments:
* `.iter` starts an iteration; `.enumerate` indicates that you want the indices
   * the outermost `.iter` is on the rows, and returns a pair `(ith, row)`,
     where `ith` is the index and `row` the thing indexed, which in our case is an "inner" column vector
   * the innermost `.iter` is on the columns, and returns a pair `(jth, _)`,
     where `jth` is the index and `_` indicates that we will not use the thing being indexed
* `.filter` keeps only those elements that satisfy a condition
* `.map` remaps elements that have survived the previous steps (i.e., the `.filter`) --
  the interior of this features a closure, where:
  * we start a new iteration using `(0..ith)`
  * `.rev` reverses that iteration, so we move from `ith` to 0
  * `.take_while` preserves the iteration only while a condition is satisfied
  * `.fold` takes the survivors of the previous steps (i.e,. the `.take_while`) and "folds" them into one object usings its closure
  * finally, the `.fold`'s result of is used in the return value (the last line of the interior closure)
* finally, the outer iteration `.sum`s the values.

It took me a short while to learn how to read this, and once I did, I came to appreciate its relative readability, though I am still not very good at writing it.
The forms of iteration I'm using here borrows the values, which is why you'll see things like `*kth` and `**object`, which dereferences the things being borrowed.

The Ada takes a completely different approach:
* iteration over the map (rows and columns) occurs via the well-known `for` loops
* the second `for` loop is filtered using a `when` statement
* the `declare` introduces a new scope, much as a closure does for Rust
* an inner iteration occurs via the well-known `while` loop
  * we don't need to `.rev`erse the loop as in Rust
  * we _do_ need to "step" the loop by modifying `Travel_Row`, which Rust's iterators freed us from worrying about

Ada has a `reverse` keyword for loops; i.e., `for Jth in reverse 2..Row`.
While writing this comparison, I realized that it could be used on the loop above, obviating the need to manually step `Travel_Row`:
```ada
         for Jth in reverse 1..Row - 1 loop
            exit when System (Jth, Col) = Immovable;

            Travel_Row := Jth;
            if System (Jth, Col) = Movable then
               Movable_In_The_Way := @ + 1;
            end if;

         end loop;
```
However, `Jth` does not exist outside the loop's scope, so you still have to update `Travel_Row`, which effectively steps it anyway: you've gained nothing except perhaps some clarity of expression. (More on that in a moment.)
The `exit` keyword is pretty crucial where it is: I first implemented it rather carelessly as a `for ... when ... loop` and spent far too long trying to figure out why that didn't work.
In the end I reverted to the `while` loop, even though in the `for` loop may be clearer on account of its explicit declaration that the loop _steps_ (`for`) and is thus guaranteed to be finite.
