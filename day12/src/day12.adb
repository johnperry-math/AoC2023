pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 12: Hot Springs
--
--  part 1: how many ways could the recorded spring conditions
--          match the recorded sequences of damaged springs?
--
--  part 2: repeat after quintupling each record's length

with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

use all type Ada.Containers.Count_Type;

procedure Day12 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Num => Natural);

   --  SECTION
   --  global types and variables

   type Result_Value is range 0 .. 2**64 - 1;
   --  needed for part 2

   Debugging : constant Boolean := False;
   --  needed (by me anyway) for part 2

   --  SUBSECTION
   --  spring records

   type Condition is (Operational, Damaged, Unknown);

   type Spring_Array is array (Positive range <>) of Condition;

   package Condition_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Condition);
   --  the record of spring conditions

   package Contiguous_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Positive);
   --  the record of contiguous damaged springs

   type Condition_Record is record
      Springs    : Condition_Vecs.Vector;
      Contiguous : Contiguous_Vecs.Vector;
   end record;

   package Condition_Record_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Condition_Record);

   Condition_Records : Condition_Record_Vecs.Vector;

   --  SECTION
   --  I/O

   Invalid_Character : exception;

   procedure Read_Input is
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      while not IO.End_Of_File (Input) loop

         declare
            S   : constant String := IO.Get_Line (Input);
            Pos : Positive        := Ada.Strings.Fixed.Index (S, " ", 1);
            --  position in the text while parsing

            New_Conditions : Condition_Vecs.Vector;
            New_Contiguous : Contiguous_Vecs.Vector;

            Value : Natural;
            --  for reading integers

         begin

            for Ith in S'First .. Pos - 1 loop

               case S (Ith) is
                  when '.' =>
                     New_Conditions.Append (Operational);
                  when '#' =>
                     New_Conditions.Append (Damaged);
                  when '?' =>
                     New_Conditions.Append (Unknown);
                  when others =>
                     raise Invalid_Character;
               end case;

            end loop;

            Pos := @ + 1;

            while Pos <= S'Last loop
               Nat_IO.Get (S (Pos .. S'Last), Value, Pos);
               New_Contiguous.Append (Value);
               Pos := @ + 2;
            end loop;

            Condition_Records.Append
              (Condition_Record'
                 (Springs => New_Conditions, Contiguous => New_Contiguous));

         end;
      end loop;

   end Read_Input;

   procedure Put_Spring (Spring : Condition) is
   --  useful for debugging
   begin
      IO.Put
        (case Spring is when Operational => '.', when Damaged => '#',
           when Unknown => '?');
   end Put_Spring;

   procedure Put_Springs (Springs : Condition_Vecs.Vector) is
   --  useful for debugging
   begin
      for Each of Springs loop
         Put_Spring (Each);
      end loop;
   end Put_Springs;

   procedure Put_Contiguous (C : Contiguous_Vecs.Vector) is
   --  useful for debugging
   begin
      for Each of C loop
         Nat_IO.Put (Each, 0);
         IO.Put (',');
      end loop;
   end Put_Contiguous;

   procedure Put_Condition (C : Condition_Record) is
   --  useful for debugging
   begin
      Put_Springs (C.Springs);
      IO.Put (' ');
      Put_Contiguous (C.Contiguous);
   end Put_Condition;

   --  SECTION
   --  the original approach: recursion with pruned branches

   function Consistent (C : Condition_Record) return Boolean is
      --  is C consistent?
      Contiguous       : Contiguous_Vecs.Vector;
      Streak           : Natural  := 0;
      Last_Checked_Pos : Positive := 1;
   begin

      --  count sequences of damaged springs
      for Ith in C.Springs.First_Index .. C.Springs.Last_Index loop

         if C.Springs (Ith) = Damaged then
            Streak := @ + 1;
         elsif Streak > 0 then
            Contiguous.Append (Streak);
            Streak := 0;
         end if;

         exit when C.Springs (Ith) = Unknown;

         Last_Checked_Pos := Ith;

      end loop;

      --  don't lose the last sequence!
      if Streak > 0 then
         Contiguous.Append (Streak);
      end if;

      if Contiguous.Length > C.Contiguous.Length then
         --  whoops; too many sequences: not consistent!
         return False;
      end if;

      if Last_Checked_Pos = C.Springs.Last_Index
        and then Contiguous.Length < C.Contiguous.Length
      then
         --  whoops; not enough springs in the last sequence!
         return False;
      end if;

      for Ith in Contiguous.First_Index .. Contiguous.Last_Index loop

         if Contiguous (Ith) > C.Contiguous (Ith) then
            --  whoops; too many springs in the ith sequence!
            return False;

         elsif Ith < Contiguous.Last_Index
           and then Contiguous (Ith) < C.Contiguous (Ith)
         then
            --  whoops; too few springs in the ith sequence!
            --  (but the last gets a brief reprieve,
            --  in case we haven't finished the line)
            return False;

         elsif Ith = Contiguous.Last_Index
           and then Last_Checked_Pos = C.Springs.Last_Index
           and then Contiguous (Ith) < C.Contiguous (Ith)
         then
            --  whoops; the last sequence is too few
            --  and we have finished the line, so it can't expand!
            return False;
         end if;

      end loop;

      return True;

   end Consistent;

   function Matches
     (C : Condition_Record; At_Position : Positive := 1) return Natural
   is
      --  recursive approach: build a sequence springs,
      --  branching at each unknown, checking consistency to prune bad branches
      --
      --  we start at the given position, enabling the recursion

      Result : Natural  := 0;
      Pos    : Positive := At_Position;

   begin

      --  skip what we know
      while Pos < C.Springs.Last_Index and then C.Springs (Pos) /= Unknown loop
         Pos := @ + 1;
      end loop;

      --  unknown! branch!
      if Pos <= C.Springs.Last_Index and then C.Springs (Pos) = Unknown then

         declare
            First, Second               : Condition_Record := C;
            First_Result, Second_Result : Natural          := 0;
         begin

            First.Springs (Pos)  := Operational;
            Second.Springs (Pos) := Damaged;

            if Consistent (First) then
               First_Result := Matches (First, Pos + 1);
            end if;
            if Consistent (Second) then
               Second_Result := Matches (Second, Pos + 1);
            end if;

            Result := First_Result + Second_Result;

         end;

      else
         if Consistent (C) then
            Result := 1;
         end if;

      end if;

      return Result;

   end Matches;

   function Part_1 return Natural is
      Result     : Natural := 0;
      This_Score : Natural;
   begin
      for Rec of Condition_Records loop
         This_Score := Matches (Rec);
         Result     := @ + This_Score;
         Nat_IO.Put (This_Score, 0);
         Put_Condition (Rec);
         IO.New_Line;
      end loop;
      return Result;
   end Part_1;

   --  SECTION
   --  the dynamic programming approach

   --  SUBSECTION
   --  tracking state / memoization

   type Dynamic_State is record
      Remainder : Contiguous_Vecs.Vector;
      First     : Positive;
   end record;
   --  we keep track of the number of remaining sequences of damaged springs
   --  and the first place in the record to check

   function State_Hash (Value : Dynamic_State) return Ada.Containers.Hash_Type
   is
      Result : Natural := 0;
   begin

      for Each of Value.Remainder loop
         Result := (@ * 10) mod 100_001 + Each;
      end loop;
      Result := (@ * 30) mod 100_001 + Value.First;

      return Ada.Containers.Hash_Type (Result);

   end State_Hash;

   package Dynamic_State_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Dynamic_State, Element_Type => Result_Value,
      Hash     => State_Hash, Equivalent_Keys => "=");

   --  SUBSECTION
   --  matching

   function First_Fits_In_Second
     (First : Spring_Array; Second : Condition_Vecs.Vector) return Boolean is
   --  return true iff each spring in First matches
   --  the corresponding spring in Second
   --
   --  note that indexing is done relative to First'First and First'Last,
   --  only one of many reasons why Ada is Awesome.
   --  leave the zero-based indexing insanity to the compiler!

     (First'Length <= Second.Length
      and then
      (for all Ith in First'First .. First'Last =>
         First (Ith) = Second (Ith) or else First (Ith) = Unknown
         or else Second (Ith) = Unknown));

   function Matching_Slides
     (Springs :        Condition_Vecs.Vector; State : Dynamic_State;
      Cache   : in out Dynamic_State_Maps.Map) return Result_Value
   is
   --  slide across Springs a number of damaged spring
   --  corresponding to the first entry in State.Remainder

   begin

      if Debugging then
         IO.Put ("Sliding across ");
         Put_Springs (Springs);
         IO.Put (" from" & State.First'Image);
         IO.Put (" with respect to (");
         Put_Contiguous (State.Remainder);
         IO.Put (")");
         IO.New_Line;
      end if;

      if Cache.Contains (State) then      --  is it already known?
         if Debugging then
            IO.Put_Line ("   Cache state is known");
         end if;
         return Cache (State);

      elsif State.Remainder.Length = 0
         --  have we run out of
         --  contiguous damaged springs?
         then
         --  we cannot have any remaining damaged springs
         if
           (for all Ith in State.First .. Springs.Last_Index =>
              Springs (Ith) = Unknown or else Springs (Ith) = Operational)
         then
            --  only one possibility: all operational
            Cache.Insert (State, 1);
            return 1;
         else
            --  at least one damaged, but that is inconsistent with remainder
            Cache.Insert (State, 0);
            return 0;
         end if;

      else
         --  brace yourselves
         if Debugging then
            IO.Put_Line ("   Starting a slide");
         end if;

         declare

            --  things we know

            My_Length : constant Natural := State.Remainder.First_Element;
            --  how long this sequence will be
            First     : constant Natural := State.First;
            --  first index we check
            Last      : constant Natural := Springs.Last_Index;
            --  last index we check

            --  things we must compute

            Remaining_Minimum_Length : Natural := 0;
            --  minimum length of springs necessary for remaining sequences
            --  of damaged springs
            My_Last                  : Natural;
            --  last index we can try this sequence
            Remaining_Lengths        : Contiguous_Vecs.Vector;
            --  sequences of damaged springs that must appear after this one

         begin

            --  determine length requierd for remaining sequences
            for Ith in
              State.Remainder.First_Index + 1 .. State.Remainder.Last_Index
            loop
               Remaining_Minimum_Length := @ + 1 + State.Remainder (Ith);
               Remaining_Lengths.Append (State.Remainder (Ith));
            end loop;

            My_Last := Last - Remaining_Minimum_Length;

            if Debugging then
               IO.Put_Line
                 ("   length:" & My_Length'Image & " last:" & My_Last'Image);
            end if;

            --  can we even fit the number of damaged springs we need?
            if First + My_Length - 1 > My_Last then
               Cache.Insert (State, 0);
               return 0;
            end if;

            --  we can!
            --  create a matching string, slide it across
            --  for each match, add the number of sub-matchers
            declare
               Matcher : Spring_Array (First .. My_Last) :=
                 [others => Unknown];
               Result  : Result_Value                    := 0;
            begin

               --  set up the first n as damaged
               for Ith in First .. First + My_Length - 1 loop
                  Matcher (Ith) := Damaged;
               end loop;

               if Debugging then
                  IO.Put ("   Iterating ");
                  for Each of Matcher loop
                     Put_Spring (Each);
                  end loop;
                  IO.New_Line;

                  IO.Put_Line
                    ("   seeking length" & My_Length'Image & " from" &
                     First'Image & " through" & My_Last'Image &
                     " stopping at" & Natural'Image (My_Last - My_Length + 1));
               end if;

               --  check each, then shift sequence right

               for Ith in First .. My_Last - My_Length + 1 loop

                  if Debugging then
                     IO.Put_Line ("   starting from" & Ith'Image);
                     --  check that it fits; if so, add to result
                     IO.Put ("   Checking that ");
                     for Each of Matcher loop
                        Put_Spring (Each);
                     end loop;
                     IO.Put (" fits in ");
                     Put_Springs (Springs);
                     IO.Put (" at position" & First'Image);
                     IO.New_Line;
                  end if;

                  if First_Fits_In_Second (Matcher, Springs) then

                     --  it fits; can we extend?

                     if Debugging then
                        IO.Put_Line ("   it does!");
                     end if;

                     if Ith + My_Length <= Springs.Last_Index
                       and then
                       (Springs (Ith + My_Length) = Operational
                        or else Springs (Ith + My_Length) = Unknown)
                     then
                        Result :=
                          @ +
                          Matching_Slides
                            (Springs,
                             ((Remaining_Lengths, Ith + My_Length + 1)),
                             Cache);

                     elsif Remaining_Lengths.Length = 0
                       and then
                       (Ith + My_Length > Springs.Last_Index
                        or else Springs (Ith + My_Length) = Operational)
                     then
                        Result := @ + 1;

                     end if;

                     if Debugging then
                        IO.Put_Line ("   result is now" & Result'Image);
                     end if;

                  end if;

                  --  shift!
                  if Ith + My_Length <= My_Last then
                     Matcher (Ith + My_Length) := Damaged;
                     Matcher (Ith)             := Operational;
                  end if;

               end loop;

               --  record in cache, then return
               Cache.Insert (State, Result);
               return Result;

            end;
         end;
      end if;
   end Matching_Slides;

   function Part_1_Dynamic return Result_Value is
      Result : Result_Value := 0;
   begin
      for Rec of Condition_Records loop
         declare
            Cache      : Dynamic_State_Maps.Map;
            State      : constant Dynamic_State :=
              (Remainder => Rec.Contiguous, First => 1);
            This_Score : constant Result_Value  :=
              Matching_Slides (Rec.Springs, State, Cache);
         begin

            Result := @ + This_Score;
            if Debugging then
               IO.Put ("running score: " & This_Score'Image & " for ");
               Put_Condition (Rec);
               IO.New_Line;
            end if;

         end;
      end loop;

      return Result;

   end Part_1_Dynamic;

   function Part_2 return Result_Value is
      Result      : Result_Value := 0;
      New_Records : Condition_Record_Vecs.Vector;
   begin
      --  quintuple the records
      for Rec of Condition_Records loop
         declare
            New_Rec : Condition_Record;
         begin
            for Ith in 1 .. 5 loop
               for S of Rec.Springs loop
                  New_Rec.Springs.Append (S);
               end loop;
               for C of Rec.Contiguous loop
                  New_Rec.Contiguous.Append (C);
               end loop;
               if Ith < 5 then
                  New_Rec.Springs.Append (Unknown);
               end if;
            end loop;
            New_Records.Append (New_Rec);
         end;
      end loop;

      --  now check them, same as in part 1
      for Ith in New_Records.First_Index .. New_Records.Last_Index loop
         declare
            Cache      : Dynamic_State_Maps.Map;
            State      : Dynamic_State :=
              (Remainder => New_Records (Ith).Contiguous, First => 1);
            This_Score : Result_Value  :=
              Matching_Slides (New_Records (Ith).Springs, State, Cache);
         begin
            Result := @ + This_Score;
         end;
         IO.Put_Line ("Finished" & Ith'Image);
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   --  IO.Put_Line ("The number of possible arrangements is" & Part_1'Image);
   IO.Put_Line ("Done dynamically:" & Part_1_Dynamic'Image);
   IO.Put_Line ("After unfolding, that number is" & Part_2'Image);
end Day12;
