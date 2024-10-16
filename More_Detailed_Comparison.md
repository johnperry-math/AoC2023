# A Comparison of Ada and Rust, using solutions to the Advent of Code

I sometimes participate in the Advent of Code competition, and have always used Ada in the past.
In 2023, Jeremy Grosser at the ada-lang.io forums [invited](https://forum.ada-lang.io/t/advent-of-code-2023/540) discussion of solutions in other languages with "a focus on safety or reliability." That gave me the idea to try translating the Ada solutions I devised into Rust and Modula-2.

I've detailed a few observations [here](README.md) and [there](https://forum.ada-lang.io/t/irenic-language-comparisons-and-questions-ada-and-rust-aoc-2023-day-13/638), but they were all on a puzzle-by-puzzle basis.
Now that I've completed them all in Ada and Rust, I'd like to step back and take a more general, "higher-level" view.

To that end, I decided to highlight code snippets for four common tasks, datatypes, or techniques.
The choice of topic is obviously subjective, but I'll try to restrain my commentary to what's necessary to understand,
as well as some distinctive features I'm aware of in each language's approach.
Otherwise, I'll avoid talking about personal preference.

(Well after I began writing this, but a little before I finished, AdaCore published [a similarly-themed review](https://blog.adacore.com/should-i-choose-ada-spark-or-rust-over-c-c) that addresses quite a few different points.)

   ## Table of Contents

* [Caveats and Disclaimers](#caveats-and-disclaimers)
  * [Which version of the language?](#which-version-of-the-language)
  * [Why didn't you discuss Feature X? It's the language's "killer app"!](#why-didnt-you-discuss-feature-x-its-the-languages-killer-app)
  * [Are you the best tour guide?](#are-you-the-best-tour-guide)
* [Language overviews](#language-overviews)
  * [Ada](#ada)
  * [Rust](#rust)
  * [Similarities](#similarities)
  * [Differences](#differences)
  * [Feature comparison](#feature-comparison)
  * [Why should I care about Language X?](#why-should-i-care-about-language-x)
  * [Speed and reliability](#speed-and-reliability)
* [Case study 1: File iteration and processing with error handling](#case-study-1-file-iteration-and-processing-with-error-handling)
* [Case study 2: Modularity and generics](#case-study-2-modularity-and-generics)
* [Case study 3: Enumerations](#case-study-3-enumerations)
* [Case study 4: Filtered and enumerated iteration](#case-study-4-filtered-and-enumerated-iteration)

## Caveats and Disclaimers

### Which version of the language?

Ada and Rust have multiple specifications. The versions used for comparison here are

* Ada 2022
  * This includes an occasional reference to Ada SPARK 2014 on account of its more stringent rules,
    but otherwise I didn't use SPARK this year.
* Rust 2021
  * One Rustacean [points out](https://users.rust-lang.org/t/irenic-comparison-of-ada-and-rust-based-on-the-advent-of-code/119249/3?u=johnperry-math) that I "should probably put both the edition and version here", since "[l]ots of changes happen within a single edition." I've worked on this for a lot of months, which involved several versions, but at the time of this writeup I'm using 1.81.0. 

### Why didn't you discuss Feature X? It's the language's "killer app"!

* I did, but it's not a section heading.
* It didn't seem important in my solutions.
* I'm kind of worn out after all the things I *did* discuss.
* I didn't think of it. Why, yes, I am a horrible ignoramus.
  Go ahead & 😡    rage 😡 type your pull request and I'll consider it.

### Are you the best tour guide?

No, but I'm the one you're stuck with. 😁

I don't consider myself an Ada expert by any means, [but](https://github.com/johnperry-math/PL-0-in-Ada) [I](https://github.com/johnperry-math/GtkAdaQuoter) [have](https://github.com/johnperry-math/RosettaCode) [written](https://github.com/johnperry-math/AoC2020) [some](https://github.com/johnperry-math/AoC2020) [non](https://github.com/johnperry-math/AoC2022)-[trivial](https://github.com/johnperry-math/AoC2022) [programs](https://github.com/johnperry-math/AoC2021) [in](https://github.com/johnperry-math/AoC2018) [it](https://github.com/johnperry-math/AoC2023). (Each word a different repository! 😁)

I don't have any Rust code in public repositories, aside from the one you're reading right now, but I've used it on an almost daily basis at work for three years. I don't think my employer will allow me to share that, though, sorry.

That said:

* I speak both Ada and Rust, but not natively. My background in 1980s-era C/C++, [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)), and [Modula-2](https://en.wikipedia.org/wiki/Modula-2) will probably be apparent. Code excerpts should accordingly be considered usable but not necessarily idiomatic.
* My solutions may not be optimal, or even advisable.
  * I only look at other people's solutions, or even the discussion of solutions, after I've either solved mine or become so hopelessly stuck that I'm looking for the insight needed to solve the puzzle.
  * I do try to clean up my solutions, but not as much as I probably should.
  * I do weird things like solve Day 24 using [Groebner bases](https://en.wikipedia.org/wiki/Gr%C3%B6bner_basis).

## Language overviews

You only need to read this section if you've never heard of one or the other language.

### Why should I care about Language X?

* You find language comparisons interesting.
* I started to write more, but that commentary seemed too subjective and hard to phrase to my satisfaction, so I'll quit there. I'm open to suggestions, though!

### Ada

Ada's arguably the best language that you've either:

* never heard of, or
* heard someone disparage it wrongly as:
  * dead,
  * complicated, and/or
  * far too wordy.

(Qualifiers matter. I'm not necessarily saying Ada is the best language, full stop.)

Ada is a general-purpose, high-level language with an emphasis on safe, secure programming that emphasizes ease of **reading** code over ease of **writing** it. It has a well-established niche in [situations where correctness, reliability, and large-scale development are the primary concern](https://www.adacore.com/industries). Its initial, international development culminated in an 1983 ANSI standard and a 1987 ISO standard. Subsequent [revisions to the language](https://www.adaic.org/ada-resources/standards/) led to Ada 95, Ada 2005, Ada 2012, and Ada 2022. Ada revisions tend to be backwards compatible; every now and then I'll read someone write with pleasure that he's taken an old Ada 83 codebase and successfuly recompiled it in Ada 2012 (say) with little to no effort. A dialect of Ada called Spark offers additional features and guarantees, subject to a number of restrictions on what you can do. Spark is a pretty big deal in the Ada world.

An open-source, GPL'd Ada compiler stays on the language's cutting edge, but vendors have been around for decades.

### Rust

I won't say that Rust is the best language you *have* heard of, but it is the language that StackOverflow users keep voting, [year](https://survey.stackoverflow.co/2024/technology#admired-and-desired) after [year](https://survey.stackoverflow.co/2023/#technology-admired-and-desired) after [year](https://survey.stackoverflow.co/2022/#technology-most-popular-technologies), as their most admired programming language.

Rust is a low-level, general-purpose language with an emphasis on safety, in particular memory safety. Rust began at Mozilla, who has used Rust in several projects, such as rewriting [Firefox's CSS engine](https://searchfox.org/mozilla-central/source/servo) in Rust. Mozilla subsequently release Rust "into the wild", placing it under the purview of the Rust Foundation. Rust 1.0 was released in 2015; since then, several somewhat-incompatible "editions" were released in 2018 and 2021, with a new edition due in late 2024.

As far as I know, there is only one Rust compiler, based on the llvm toolchain. The Gnu Compiler Collection is working on its own version. Ferrous systems offers a version of the compiler [certified for safety- and mission-critical systems](https://ferrocene.dev/en/) that [they developed in partnership with AdaCore](https://ferrous-systems.com/blog/ferrocene-update/).

### Similarities

Both Ada and Rust are:

* general-purpose programming languages used for systems and embedded development,
* designed with the goal of safe programming,
* recommended by [NIST](https://www.nist.gov/itl/ssd/software-quality-group/safer-languages) as languages suitable for safe development,
* with compilers certified for use safety- and mission-critical systems,
* encouraging stack-centric programming idioms,
* developed and used in major projects at a major organization (US DoD for Ada, Mozilla for Rust),
* later released to a community organization (Ada Rapporteur Group, Rust Foundation).

### Differences

Only major differences here. There's no way I'll touch on all of them.

* Ada and Rust have somewhat different notions of safety. I won't comment further, because that's beyond scope and this is already long in the tooth.
* Ada relies on a very detailed, carefully considered specification that is an ISO standard determined by a committee of academic and industry experts. The Ada Reference Manual is [freely available and downloadable online](http://www.adaic.org/resources/add_content/standards/22rm/html/RM-TTL.html). Parts of it can be difficult to read, but I use it routinely as a reference and discussions of Ada frequently refer back to the ARM.
* Rust's specification seems to be [The Rust Reference](https://doc.rust-lang.org/reference/index.html) and/or the compiler.
  * The website explicitly states that The Rust Reference "is not a formal spec".

    > Finally, this book is not normative. It may include details that are specific to `rustc` itself, and should not be taken as a specification for the Rust language. We intend to produce such a book someday, and until then, the reference is the closest thing we have to one.
    >
  * The standard library's documentation is [separate](https://doc.rust-lang.org/std/index.html) from the language reference.
* Ada programs tend to define types of the problem to be solved. The compiler then adapts the low-level type to match what is requested. Rust programs tend to rely on low-level types.

  That may not be clear, so two examples may help:
  
  1. Ada programmers prefer to specify integer types in terms of the ranges of values they may take and/or the precision of floating-point types in terms of digits. I ended up doing this at least once, where on Day 23 I specified a floating-point type in terms of the number of digits it should reproduce accurately: `Digits 18`. The compiler automatically chose the most appropriate machine type for that.

  2. Ada arrays don't have to start from 0, nor even do they have to be indexed by integers. An example of this appears below.
  
  By contrast, the Rust programs I've seen tend to specify types in terms of low-level, machine types. Thus, I tried to address the same problem using an `f64`. In this particular case, there were repercussions, but usually that works fine as long as you know what the machine types can do. You can index Rust types with non-integers, but it takes quite a bit more work than Ada.

### Feature comparison

The following table indicates whether something is built into the language and available immediately. It also indicates whether you can find it in a library, regardless of how robust the library is. As examples of what I mean:
* "Built into the language and available immediately" excludes Rust's [tokio](https://tokio.rs/) and [GNAT extensions to Ada](https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/gnat_language_extensions.html).
* However, you _can_ get concurrent programming in Rust by pulling in the widely used tokio library. For cases like that, I use the 📖 icon.
* On the other hand, "nightly" or compiler-specific "extensions" to a language don't count, so they get an ❌, though I might remark on them in the notes if I'm aware of them.

**Legend**

✔️ default

📝 default for debug mode; opt-in for release mode

📖 available with a library not in `Ada.` / `std::`

❌ unavailable


| topic                                                     | Ada  | Spark | Rust | Notes                                                                                                                                                                                                                        |
| ----------------------------------------------------------- | ------ | ------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| array bounds checking                                     | ✔️ | ✔️  | ✔️ | all terminate, um, "gracefully", by which I mean a useful error message on a bounds violation, rather than a segmentation fault or some other monstrosity |
| array indexing by any discrete type                             | ✔️ | ✔️  | ❌   | e.g., `type Direction is (Up, Dn, Lt, Rt); type Destinations is array (Direction) of Location;`                                                                                                                               |
| automatic detection and collection of unused heap objects | ❌   | ✔️ | ✔️ | I'd call this "built-in garbage collection", but these days everyone thinks that means a tracing garbage collector running in the background, and neither Spark nor Rust uses those.                                                                                                                                |
| concurrent programming / tasking                          | ✔️ | ✔️  | 📖   | Rust's `async` requires a non-standard library, but [see this reply](https://users.rust-lang.org/t/irenic-comparison-of-ada-and-rust-based-on-the-advent-of-code/119249/2?u=johnperry-math)                                                                                                                                                                                |
| containers with generics                                               | ✔️ | ✔️  | ✔️ |  included if only because implementing Day 10 in Modula-2 reminded me how glad I am that we have generics these days                                                                                                                                                                                     |
| design by contract (DBC)                                  | ✔️ | ✔️  | 📖   | Rustaceans can try [the contracts crate](https://gitlab.com/karroffel/contracts) and [mirai](https://github.com/endorlabs/MIRAI)                                                                                                                                                                 |
| DBC compile-time verification                             | ❌   | ✔️  | ❌   | Rust has the [MIRAI](https://github.com/facebookexperimental/MIRAI) crate, but it's nowhere near as functional as Spark, so it doesn't get the 📖 icon                                                                                                  |
| error handling via exceptions and exception types | ✔️ | ✔️  | ❌   | for Rust, see "error handling via return types"  below                                                                                                                                                                           |
| error handling via return types | ❌   | ❌    | ✔️ | for Ada, see "error handling via exceptions and exception types"; see also [Case Study 1](#case-study-1-file-iteration-and-processing-with-error-handling)'s discussion for why the ability to roll your own without too much trouble doesn't rise to the same level |                                                                                                                        | functional chaining                                       | 📖   | 📖    | ✔️ | only recently learned about [this iterators crate for Ada](https://github.com/mosteo/iterators), so I haven't compared                                                                                                                                                                  |
| functional purity                                         | ❌   | ✔️  | ✔️ | whether you can specify a function has side effects *only* via an opt-in mechanism                                                                                                                            |
| labeled loops | ✔️ | ✔️ | ✔️ | labels have a number of useful properties, such as breaking out of nested loops |
| integer overflow checking                                         | ✔️ | ✔️  | 📝   |                                                                                                                                                                                                                              |
| memory safety                                             | ❌   | ✔️  | ✔️ | i can't possibly go into the details of this here, but: in Ada it is possible to reference a`null` pointer at run-time; in both Spark and Rust this violates the language specification (or you're engaged in `unsafe` Rust) |
| macro programming                                                    | ❌   | ❌    | ✔️ |  Ada has generics, but it's not clear to me (nor to some much more knowledgeable Ada users) that this suffices for the job                                                                                                                                                                                                                            |
| object-oriented progamming via inheritance | ✔️ | ✔️ | ❌ | rust has explicitly rejected the inheritance of _fields_, although `Trait`s allow one to inherit _methods_ |
| pattern matching                                          | ❌   | ❌    | ✔️ | e.g.,`if let Some(thing) = fnctn_with_optional_result(...)`                                                                                                                                                                  |
| ranges and subranges as types                             | ✔️ | ✔️  | ❌   | e.g.,`type Digit is new Positive range 0..9`                                                                                                                                                                                 |
### Speed and reliability

* Rust has a well-deserved reputation for fast execution times. It has a less well-deserved reputation for slow compilation times.
  Pruning unused dependencies and enabling incremental compilation both help improve Rust compile times.
  Also helpful is eliminating any dependencies with a C++ backend, as C++ seems particularly bad.
  (`#include` is such a terrible way to implement "modularity" that [even the C++ committee has finally come around](https://en.cppreference.com/w/cpp/language/modules).)
  We've observed at work that C++ projects always, *always* take disproportionately longer to compile than Rust.
* With Ada the situation is less clear. It compiles quickly enough (though `alire` boots up much slower than `cargo`), but it has a reputation for slow execution. This is due primarily to the amount of run-time checks. Rust also performs some run-time checks; see "array bounds checking" above. I've read that when other languages include the same run-time checks Ada does, differences in speed typically disappear.
* In both languages, the compiler can detect at compile-time many circumstances where it can remove run-time checks.

Almost all my solutions run quickly on my computer, even in "debug" mode, but one or two take quite a few seconds, and perhaps even a few minutes. I've seen Ada programs run faster than Rust programs and vice-versa. For what it's worth, here are a few samples plucked at random from the latter few days, when things tend to get more complicated. Times are an average of 3 runs, measured in seconds, except for day 23, which is just one run.

An listing of the compiler switches used for each column heading appears at the end of this section.

| day | Ada development | Ada release | Rust debug    | Rust release |
| ----- | ----------------- | ------------- | --------------- | -------------- |
| 17  | 8.65            | 7.28        | 15.34         | 4.45         |
| 20  | 0.38            | 0.33        | 0.44          | 0.36         |
| 21  | 7.02            | 4.471       | 16.28         | 1.18         |
| 22  | 0.32            | 0.28        | 2.14          | 0.21         |
| 23  | 377.            | 251.        | ❌, see below | 87.0 (but ❌, see below)        |
| 24  | 0.03            | 0.02        | 1.60          | 0.19         |
| 25  | 6.50            | 5.22        | 10.6          | 1.16         |

In general, Rust has both the slowest and the fastest runtimes. The results almost certainly have more to do with:
* Error checking code. Cargo's release mode disables overflow checking (`overflow-checks = false`) but overflow checking is required for all builds of an Ada compiler.
* ❌ As shown in the table, my solution to Day 23 provokes an overflow error in Rust's debug build (which checks for overflow). It doesn't do this in the default release build, but it does when I enable overflow checks in release builds. It still gives the correct answer, but I'm missing something unpleasant.
* Not shown here: the same situation arises in my solution to Day 10. For what it's worth, the Modula-2 solution also works fine in debug mode, but crashes at the `-O3` level. I haven't investigated this one yet, but it's not an issue with floats.

**Why did Ada significantly outperform Rust occurred on Day 24?**
The Ada compiler recognized that the type I specified (`digits 18`) fits into a native `TBYTE` on my architecture.
Rust doesn't allow you to specify types in that high-level manner, so I was stuck with `f64`, which is a `DWORD`.
(I infer `TBYTE` = ten bytes, `DWORD` = double word or 8 bytes, but I haven't verified that.)
When I used `f64`, the Rust would terminate with a floating-point overflow error every time.

In principle I could use `f128`, and if memory serves I managed to make that work, but that's still unstable,
and I'm trying to use only **safe**, **stable**, **standard** features of each language.
Thus I switched to the `rust_decimal` crate, and made Day 24 work that way.

So, Ada has an inherent advantage due to its higher-level approach: Rust is stuck using library code.
By specifying the type in terms of the problem, the Ada programmer delegates the choice of machine-native type to the compiler,
which knows better.

(How did I come up with `digits 18`? Some time ago I tried to see how far I could crank up the digits before the compiler declined, and 18 was gnat's limit. A professional Ada user pointed out to me that I could simply use [System.Max_Digits](http://www.adaic.org/resources/add_content/standards/22rm/html/RM-13-7.html#I5674).)

**Compiler switches for column headings**

Programs were build using Alire (`alr`) and Cargo (`cargo`).
* Alire switches are gnat switches, which are spread over several pages of the gnat reference. I've linked to them! But to determine which were active, I inspected the swtches listed in the generated file `config/day14_config.gpr`.
   * `alr build` activates   :
      * [Debug mode](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-Og): `-Og` (optimize) and `-g` (include symbols)
      * [Placement of functions & data](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-ffunction-sections): `-ffunction-sections`, `-fdata-sections`
      * [Warnings](https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gnat_ugn/Warning-Message-Control.html#index--gnatwa-_0028gcc_0029): `-gnatwa`, `-gnatw.X`
      * [All validity checks](https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gnat_ugn/Validity-Checking.html#index--gnatVa-_0028gcc_0029)`-gnatVa`
      * [Style checks](https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gnat_ugn/Style-Checking.html): `-gnaty3`, `-gnatya`, `-gnatyA`, `-gnatyB`, `-gnatyb`, `-gnatyc`, `-gnaty-d`, `-gnatye`, `-gnatyf`, `-gnatyh`, `-gnatyi`, `-gnatyI`, `-gnatyk`, `-gnatyl`, `-gnatym`, `-gnatyn`, `-gnatyO`, `-gnatyp`, `-gnatyr`, `-gnatyS`, `-gnatyt`, `-gnatyu`, `-gnatyx`
      * [UTF encoding](https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gnat_ugn/Character-Set-Control.html#index--gnatW-_0028gcc_0029-1): `-gnatW8`
   * `alr build --release` activates:
      * [All optimizations](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-Og): `-O3`
      * [Placement of functions & data](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-ffunction-sections): `-ffunction-sections`, `-fdata-sections`
      * [Inline when specified](https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gnat_ugn/Alphabetical-List-of-All-Switches.html#index--gnatn-_0028gcc_0029): `-gnatn`
      * [UTF encoding](https://gcc.gnu.org/onlinedocs/gcc-13.3.0/gnat_ugn/Character-Set-Control.

  It's worth pointing out that `-gnatVd` is the default. It enables all the checks [required by the reference manual](https://ada-lang.io/docs/arm/AA-13/AA-13.9#1391--data-validity). I disabled it for day 25, but did not see a significant impact on performance.

* Cargo switches are all explained [on this page](https://doc.rust-lang.org/cargo/reference/profiles.html#profile-settings).
   * `cargo build`, [which activates](https://doc.rust-lang.org/cargo/reference/profiles.html#dev):

         [profile.dev]
         opt-level = 0
         debug = true
         split-debuginfo = '...'  # Platform-specific.
         strip = "none"
         debug-assertions = true
         overflow-checks = true
         lto = false
         panic = 'unwind'
         incremental = true
         codegen-units = 256
         rpath = false
   * `cargo build --release`, [which activates](https://doc.rust-lang.org/cargo/reference/profiles.html#release):

         [profile.release]
         opt-level = 3
         debug = false
         split-debuginfo = '...'  # Platform-specific.
         strip = "none"
         debug-assertions = false
         overflow-checks = false
         lto = false
         panic = 'unwind'
         incremental = false
         codegen-units = 16
         rpath = false


## Case Study 1: File iteration and processing, with error handling

Advent of Code requires file processing of an input file. The file doesn't require modification, merely reading and parsing. While the puzzles do provide information required to parse the input, you are rarely given:

* the dimensions of the data,
* the number or full range of symbols, nor
* the ranges of integer values.

You can certainly look at the data once you download it and figure that out.

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
```
A `String` is an `array of Character`, the traditional 8-bit byte, so the processing can loop through the line if needed:

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
I'm not sure how programmers raised on Rust's iterators would see this, but to someone who learned to program in the 80s and 90s, the loops look natural. It may seem verbose, but some of that may be my attempt to add lots of explanatory comments.

### Rust

#### My approach

Opening the file:
```rust
let file = std::fs::File::open("input.txt").expect("where's my input?!?");
// i could .unwrap() above instead, but unwraps are less ideal; expects give more information
// of course, THIS message may be less than ideal...
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
Neither can you index your way into a `String`. You must iterate instead. Iterators are a big deal in Rust-speak, and I'll write a bit more about them below, but here are two examples of how I've used them:

```rust
    // APPROACH 1
    let mut chars = line.chars().skip(3);
    // the above turns line into an iterator of characters, then skips the first 3
    let first_symbol = [
       chars.next().unwrap(),
       chars.next().unwrap(),
       chars.next().unwrap()
    ];
    // the above takes the "next" item from chars, 3 times;
    // since this may fail, you receive an Option result, which explains the .unwrap
 
    // APPROACH 2
    let mut split = line.split(" @ ");
    // the above turns line into an iterator of tokens separated by the string " @ "
    let integer_value = split
       .next()
       .unwrap()
       .parse::<usize>()
       .expect("unable to parse to a usize!");
```

Unlike Ada's standard library, Rust's standard library doesn't offer an easy way to parse enumeration types. This is because enumeration types in Rust are not quite the same as in Ada. I'll write a bit more on that below. You can parse them using other libraries, but I'm trying to rely only on the standard library.

#### Another approach

I came of "software age" in the 80s and 90s, so I speak Rust with foreign idioms. While writing this I looked up another solution or two. [The following](https://github.com/theoludwig/advent_of_code_2023) resembles what I've often seen "native" Rustaceans produce:

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

* The author reads the input using the `include_str!` macro; that is, he basically pastes it into his program. I wouldn't do that, but it's a nice example of using a macro for quick, convenient, compile-time code generation. Ada doesn't offer macros.
* This code relies on an iterator's `.map` method. I'll write more about this below, but essentially:
  * the `input`
  * is passed line-by-line, thanks to the `.lines` method, which returns an iterator,
  * to the `.map` method, which
    * accepts the result of one iteration as its input, `line`, and then
    * executes several statements in a closure, denoted by `{ ... }`, with the aim of transforming `line` into something else, in this case an integer, after which
  * the `.map`'s results are `.sum`med.
* The lack of a semicolon on the function's last line indicates that that last line's result is the function's result.

The several lines within the `.map` closure are imperative in style, rather than functional.

* The author used `.chars` to iterate through the line's characters.
* A `.filter` keeps only ascii digits, and a `.collect` turns them into a `Vec<char>`.
* Much as there was no semicolon after the `.sum()`, the lack of a semicolon after the last instance of `number` indicates that the closure's result passes to the next iterator method.
* All of this is done *for each input `line`*!

### Error handling

The Rust code above was unable to examine the `Line` or `Char` until it extracted it from the `Result` type via `.unwrap` or, preferably, `.expect`. This makes it a bit more verbose _on this task_ than Ada, and a common criticism of Ada is that it's verbose, but here Rust is forcing the programmer at least to *think about* the possibility of an error when opening the file. In Rust, when the program encounters an exceptional condition, the system either

* `.panic!`s (bad!), or
* returns an `Option` or `Result`type, compelling the developer to handle them at an appropriate time.

If handling an error would be inappropriate, the programmer can propragate it up the stack using a `?` operator. In this case, the function's return type has to reflect this. Thus, I could have done this:

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
If an error occurs, I could forget to handle it. Worse, a client subprogram might never realize the exception could occur! And I certainly *have* forgotten to handle it in cases where it occurs.

It is worth mentioning some caveats:
* With Ada, you _could_ roll your own `Option` and `Result`. One Ada user [pointed out three ways to do it](https://forum.ada-lang.io/t/irenic-language-comparisons-ada-and-rust-aoc-overall/1275/15?u=cantanima):
  * An instance of `Ada.Containers.Indefinite_Holders`; [Ada2012]
  * Variant records; [Ada83] (I've used this approach myself at least once)
  * Arrays, unconstrained. [Ada83]

  However, there's no _standard_ way to do it in the language or standard library, so there's no culture of doing it, especially when it's possible to `raise` an `exception` type.
* Even Rust code can `panic!`, and often does during development. For example, indexing an invalid array or vector index with the `[]` operator will panic. That said, Rust's programming culture *has* generally adopted the idiom; `Option` and `Result` types are in such widespread use that I can't remember seeing a crate that lacks them. Lacking a traditional exception mechanism seems to have done a lot in this regard.
* Spark's handling of exceptions is more sophisticated than Ada's, but I'm not familiar enough with it to comment.

## Case Study 2: Modularity and generics

Advent of Code puzzles frequently involve:

* reading and navigating a map, and
* very large numbers, where one must often compute a least common multiple.

A lot of that code is reusable. Rather than copy and paste all the time, we can organize that into a dedicated module. Some Advent of Code participants do this.

### Terminology

* Ada has historically called this unit of organization a `package`.
* Rust calls them `mod`ules, which are collected into "crates". A project typically has a "workspace" of many crates.
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

Ada requires every package to separate the specification from the implementation. This was not uncommmon for languages designed from the late 60s through the mid-80s: C, C++, Modula-2, and Modula-3 offer or require this.

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
Almost no executable code may take place within the specification, though Ada 2012 allows functions that return an expression, such as

```ada
function Opposite (Left, Right : Direction) return Boolean is
      --  returns True iff Left and Right are opposite cardinal directions

        (case Left is when North => Right = South, when South => Right = North,
           when East => Right = West, when West => Right = East);
```
Otherwise, specifications are limited to declaring types, variables, and signatures of subprograms in the public interface.

If you want a public `record` type, but don't want to expose its fields, you can declare the type as `private`, then define the fields in the specification's `private` section.

Packages can have children, which you can think of as "sub-packages". They can appear in the same file or separate files. My `Common` package has four children, `Two_Dimensional_Motion`, `Two_Dimensional_Map`, `Two_Dimensional_IO`, and `Mathematics`.

To import a package, a client `with`s it:

```ada
with Ada.Text_IO;

with Common;
with Common.Two_Dimensional_Motion;
```
It then accesses the contents by prefixing with the package name:

```ada
if not Common.Two_Dimensional_Motion.Opposite
   (Common.Two_Dimensional_Motion.North, Common.Two_Dimensional_Motion.South)
then
   Ada.Text_IO.Put_Line ("I broke the world!");
end if;
```
That can get a little hard to read, so there are two ways to make it easier.
* The first is to `use` a package:

   ```ada
   with Ada.Text_IO; use Ada.Text_IO;
   with Common;
   with Common.Two_Dimensional_Motion; use Common.Two_Dimensional_Motion;
   --  snip

   if not Opposite (North, South) then
      Put_Line ("I broke the world!");
   end if;
   ```
* A second approach is to `rename` a package. It's also possible to `use` a type, and to combine the approaches.

   ```ada
   --  snip (omitting our with's)

   package Motion_2D renames Common.Two_Dimensional_Motion;
   use all type Motion_2D.Direction;
   --  snip

   if not Motion_2D.Opposite (North, South) then
      IO.Put_Line ("I broke the world!");
   end if;
   ```

My impression is that most Ada programmers adopt the `use` approach. I prefer to `rename` when possible.

#### Rust

Rust is like every language I've seen since the mid-80s (starting from Oberon): the programmer writes only the implementation file, and:

* the compiler produces a machine-readable specification for its purposes, while
* a separate documentation generator produces human-readable documentation, a la `javadoc`, `doxygen`, etc.

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

To my knowledge, Ada has no equivalent capability of declaring tests and running them with the usual build tools. Unit testing is possible, and I have employed it in a project or two, but it requires a different approach; for example, [AUnit](https://www.adacore.com/documentation/aunit-cookbook).

### Generics: specification and instantiatiation

When a puzzle has a map, the features vary from puzzle to puzzle. Representing them by a generic parameter seemed appropriate for a `Map` structure meant for as many puzzles as possible.

#### Ada

With Ada you can declare generic packages or generic procedures, but not generic types. That means the map type can be generic only if its containing package is generic. From what I understand, this means that the implementation _can_ be done via shared generics, though it can also be done via templates. [Some Ada compilers](https://stackoverflow.com/a/11968226/4526030) do indeed implement shared generics.

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
Here's an example instantiation from day 21's solution:

```ada
type Object is (Plot, Rock, Start);
Side_Length : constant Positive := (if Doing_Example then 11 else 131);
package Map_Package is new Common.Two_Dimensional_Map
  (Row_Length => Side_Length, Col_Length => Side_Length, Object => Object);
--  in that last line, the first Object refers to the generic parameter;
--  the second Object refers to the type declared in the first line
```
After these declarations, `Map_Package.Row_Range` and `Map_Package.Col_Range` represent the ranges from 1 through 11 or 131, depending on whether the code is testing the example or solving the puzzle. (Ada's ranges are inclusive by default.) In addition, `Map_Package.Row_Length` and `Map_Package.Row_Column` are accessible values, and you can now declare a value of type `Map_Package.Map_Array`. As usual, you can `use Map_Package` if you don't want to prefix all those things with the package name.

### Rust

Rust applies generics to types. Unless I'm mistaken (and I may well be) that means shared generics are out; all instantiation is via templates.
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
Unlike Ada, `SIDE_LENGTH` does not become a proper field for the `Map` type. That doesn't mean you can't know what the map's dimensions are; if the client may need to know `SIDE_LENGTH`, then you can add a function to return the value; for example,
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
The enumeration's literals are injected directly into the scope, with no qualifications required:

```ada
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
Unlike Ada's enumerations, you *do not* get a linear ordering, an iteration, or array indexing for free. You don't get the ability to print them out for free. You don't get the ability to copy the value of one enumeration to another for free (i.e., `let enum2 = enum1`). You don't even get the ability to test for equality! You have to implement those features via traits, though Rust's annotation system lets you get some of it *almost* for free. The following declaration gives me a `Direction` type that I can print in debug format, test for equality, order, and copy from one value to another.

```rust
    #[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
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

Ada programmers can do similar things with enumerations; they just have to go through `record`s known as "discriminated types". In a sense, Ada and Rust offer the same facility; they just reverse the tool usage:

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
if V.Variant = Variant_1 then --  this will fail, given the declarations above
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
😨 A pyramid of doom! 😨

To sum up, Rust enumerations' ability to carry data, along with safe Rust's requirement that no variabile be uninitialized, means you can't iterate safely through all enumerations, so you can't iterate safely through any enumeration unless you use an external package *and* all the enum's variants' contents' types have default values. On the other hand, Rust's pattern matching can help made code briefer without sacrificing readability.

Those who peruse the Rust solutions will find three places where I used `if let Some(thing)` and none where I used `match` with non-trivial pattern matching, but that's more a reflection of the approach I used to the Advent of Code puzzles, especially since I was translating Ada solutions. At work we use non-trivial pattern matching quite a bit.

## Case Study 4: Filtered and enumerated iteration

One frequently needs to iterate through a container while indexing, pruning, and manipulating values.
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
As with file processing above, this seems very straightforward and readable. My Rust equivalent looks like this:
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
Almost everything is doable through functions, many of which take closures as arguments:
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

The forms of iteration I'm using here borrow the values, which is why you'll see things like `*kth` and `**object`, which dereferences the things being borrowed.

Looking back at the Ada:
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
However, `Jth` does not exist outside the loop's scope, so you still have to update `Travel_Row`, which effectively steps it anyway.
In the end I reverted to the `while` loop, even though the `for` loop may be clearer on account of its explicit declaration that the loop _steps_ (`for`) and is thus guaranteed to be finite.
