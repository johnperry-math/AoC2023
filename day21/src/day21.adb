pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 21: Step Counter
--
--  part 1: an elf wants to know how many garden plots he would visit
--          after 64 steps
--
--  part 2: he actually meant 26501365 steps

with Ada.Text_IO;

with Common;

procedure Day21 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   use Common.Two_Dimensional_Motion;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global constants, types, and variables

   type Ludicrous_Size is range 0 .. 2**64 - 1;

   --  SUBSECTION
   --  map information

   type Object is (Plot, Rock, Start);

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object is
     (case Symbol is when '.' => Plot, when '#' => Rock, when 'S' => Start,
        when others => raise Invalid_Symbol with Symbol'Image);

   function Serialize (O : Object) return Character is
     (case O is when Plot => '.', when Rock => '#', when Start => 'S');

   Side_Length : constant Positive := (if Doing_Example then 11 else 131);

   package Map_Package is new Common.Two_Dimensional_Map
     (Row_Length => Side_Length, Col_Length => Side_Length, Object => Object);

   use Map_Package;

   package Map_IO is new Common.Two_Dimensional_Map_IO
     (Map_Package => Map_Package);

   subtype Side_Range is Row_Range;

   Start_Position : Location_Record;

   procedure Find_Start is
   begin
      for Row in Row_Range loop
         for Col in Col_Range loop
            if Map (Row, Col) = Start then
               Start_Position := (Row, Col);
               Map (Row, Col) := Plot;
            end if;
         end loop;
      end loop;
   end Find_Start;

   --  SECTION
   --  I/O

   procedure Put_Map_Visited (Visited : Location_Sets.Set) is
   --  useful for debugging
   begin

      for Row in Side_Range loop
         for Col in Side_Range loop

            if Visited.Contains (Location_Record'(Row, Col)) then
               IO.Put ('O');

            elsif Map (Row, Col) = Rock then
               IO.Put ('#');

            else
               IO.Put ('.');
            end if;

         end loop;
         IO.New_Line;
      end loop;

   end Put_Map_Visited;

   --  SECTION
   --  Parts 1 and 2

   --  SUBSECTION
   --  Part 1

   Part_1_Steps : constant Positive := (if Doing_Example then 6 else 64);

   function Part_1
     (Max_Steps : Positive; Start : Location_Record) return Natural
   is

      To_Do : array (0 .. Max_Steps) of Location_Vectors.Vector;
      Done  : Location_Sets.Set;

      Next : Location_Record;

   begin

      To_Do (0).Append (Start);

      for Step in To_Do'Range loop

         Done.Clear;

         if Step < To_Do'Last then

            for Curr of To_Do (Step) loop
               for D of Deltas loop

                  if Curr.Row + D.DRow in Side_Range
                    and then Curr.Col + D.DCol in Side_Range
                  then

                     Next :=
                       Location_Record'
                         (Row => Curr.Row + D.DRow, Col => Curr.Col + D.DCol);

                     if Map (Next.Row, Next.Col) = Plot
                       and then (not Done.Contains (Next))
                     then
                        To_Do (Step + 1).Append (Next);
                        Done.Insert (Next);
                     end if;

                  end if;

               end loop;
            end loop;

         else

            for Curr of To_Do (Step) loop
               if not Done.Contains (Curr) then
                  Done.Insert (Curr);
               end if;
            end loop;

         end if;
      end loop;

      --  debugging
      --
      --  Put_Map_Visited (Done);
      --  IO.New_Line (2);
      --  Put_Map_Visited (Final_Visits);
      --  IO.New_Line (2);

      return Natural (Done.Length);

   end Part_1;

   --  SECTION
   --  exploratory support for part 2

   function Test_Approach return Natural is
      --  this builds a large subset of the infinite map and runs the stepper
      --  i used this to confirm and correct several conjectures about behavior
      --  retained here for sentimental reasons, and you can try it; see below

      --  SUBSECTION
      --  the basics

      Multiple : constant Positive := 9;
      --  repetitions of the square in each direction
      --  (this gives me 9x9x131x131)
      --  if your puzzle is like mine, and it probably is,
      --  you want to use an odd number here

      Repetitions : constant Positive :=
        Multiple / 2 * Side_Length + Side_Length / 2;
      --  number of steps we want to run;
      --  this formula imitates the structure of the steps requested in part 2

      --  SUBSECTION
      --  larger map

      Expanded_Side_Length : constant Positive := Side_Length * Multiple;

      package Expanded_Map_Array is new Common.Two_Dimensional_Map
        (Row_Length => Expanded_Side_Length,
         Col_Length => Expanded_Side_Length, Object => Object);

      subtype Expanded_Side_Range is Expanded_Map_Array.Row_Range;
      Expanded_Map renames Expanded_Map_Array.Map;

      subtype Expanded_Location_Record is Expanded_Map_Array.Location_Record;

      --  SUBSECTION
      --  adjusting location tracking

      package Expanded_Location_Sets renames Expanded_Map_Array.Location_Sets;
      package Expanded_Location_Vectors renames
        Expanded_Map_Array.Location_Vectors;

      --  SUBSECTION
      --  BFS-related

      To_Do : array (0 .. Repetitions) of Expanded_Location_Vectors.Vector;
      Done  : Expanded_Location_Sets.Set;

      Next : Expanded_Location_Record;

      Final_Visits : Expanded_Location_Sets.Set;

      procedure Put_Map_Expanded (Visited : Expanded_Location_Sets.Set) is
         --  useful for debugging

         Number_Of_Os : Natural := 0;

      begin

         --  print map and count all visits

         for Row in Expanded_Side_Range loop
            for Col in Expanded_Side_Range loop

               if Visited.Contains (Expanded_Location_Record'(Row, Col)) then
                  IO.Put ('O');
                  Number_Of_Os := @ + 1;

               elsif Expanded_Map (Row, Col) = Rock then
                  IO.Put ('#');

               else
                  IO.Put ('.');
               end if;

               if Col mod Side_Length = 0 then
                  IO.Put ('|');
               end if;

            end loop;

            IO.New_Line;

            if Row mod Side_Length = 0 then
               for Col in Expanded_Side_Range loop
                  IO.Put ('-');
               end loop;
               IO.New_Line;
            end if;

         end loop;

         IO.Put_Line ("Counted" & Number_Of_Os'Image & " plots visited");

         --  now count visits for each particular copy of the original square

         for Row_Mul in 0 .. Multiple - 1 loop
            for Col_Mul in 0 .. Multiple - 1 loop

               Number_Of_Os := 0;

               for Row in Side_Range loop
                  for Col in Side_Range loop

                     if Visited.Contains
                         ((Row_Mul * Side_Length + Row,
                           Col_Mul * Side_Length + Col))
                     then
                        Number_Of_Os := @ + 1;
                     end if;

                  end loop;
               end loop;

               IO.Put_Line
                 ("Block" & Natural'Image (Row_Mul + 1) &
                  Natural'Image (Col_Mul + 1) & " has" & Number_Of_Os'Image &
                  " visited plots");

            end loop;
         end loop;

      end Put_Map_Expanded;

   begin

      IO.Put_Line
        ("Multiple is" & Multiple'Image & " and will run" &
         Natural'Image (Repetitions) & " steps in a map of dimension" &
         Natural'Image (Expanded_Map'Length (1)));

      --  first copy the map

      for Row_Mul in 0 .. Multiple - 1 loop
         for Col_Mul in 0 .. Multiple - 1 loop

            for Row in Side_Range loop
               for Col in Side_Range loop

                  Expanded_Map
                    (Row_Mul * Side_Range'Last + Row,
                     Col_Mul * Side_Range'Last + Col) :=
                    Map (Row, Col);

               end loop;
            end loop;

         end loop;
      end loop;

      --  BFS our way to great joy

      To_Do (0).Append
        (Expanded_Location_Record'
           (Row => Expanded_Side_Length / 2 + 1,
            Col => Expanded_Side_Length / 2 + 1));

      for Step in 0 .. Repetitions loop

         IO.Put_Line
           ("Step" & Step'Image & " considers" & To_Do (Step).Length'Image &
            " plots");

         Done.Clear;

         if Step < Repetitions then

            for Curr of To_Do (Step) loop
               for D of Deltas loop

                  if Curr.Row + D.DRow in Expanded_Side_Range
                    and then Curr.Col + D.DCol in Expanded_Side_Range
                  then

                     Next :=
                       Expanded_Location_Record'
                         (Row => Curr.Row + D.DRow, Col => Curr.Col + D.DCol);

                     if Expanded_Map (Next.Row, Next.Col) = Plot
                       and then (not Done.Contains (Next))
                     then
                        To_Do (Step + 1).Append (Next);
                        Done.Insert (Next);
                     end if;
                  end if;

               end loop;
            end loop;

         else
            for Curr of To_Do (Step) loop
               if not Done.Contains (Curr) then
                  Done.Insert (Curr);
                  Final_Visits.Insert (Curr);
               end if;
            end loop;
         end if;

      end loop;

      Put_Map_Expanded (Final_Visits);
      IO.New_Line (2);

      return Natural (Final_Visits.Length);

   end Test_Approach;

   --  SECTION
   --  part 2

   function Part_2 (Number_Of_Steps : Ludicrous_Size) return Ludicrous_Size is

      Result : Ludicrous_Size := 0;

      Blocks_Up : constant Ludicrous_Size :=
        (Number_Of_Steps - Ludicrous_Size (Side_Length / 2)) /
        Ludicrous_Size (Side_Length);
      --  number of blocks traveled from initial block in steps listed

      --  the following represent all necessary values to obtain the answer
      --  see diagram (IO.Put_Line's below) since 1 picture = 1_000 words

      From_Top_Ctr, From_Bot_Ctr, From_Right_Mid, From_Left_Mid : Natural;
      From_Bot_Right_1, From_Bot_Right_2, From_Bot_Left_1,
      From_Bot_Left_2                                           : Natural;
      From_Top_Left_1, From_Top_Left_2, From_Top_Right_1,
      From_Top_Right_2                                          : Natural;
      Full_1, Full_2                                            : Natural;

      --  the following two formulas are slightly simplified
      Corner_1_Steps : constant Natural := Side_Length + Side_Length / 2 - 1;
      --  number of steps taken in the ..._1 variables after entering
      Corner_2_Steps : constant Natural := Side_Length / 2 - 1;
      --  number of steps taken in the ..._2 variables after entering

   begin

      --  prologue

      IO.Put_Line
        ("The following calculations assume you have a" & Side_Length'Image &
         " x" & Side_Length'Image & " grid");
      IO.Put_Line
        ("with the starting position in the center (" &
         Positive'Image (Side_Length / 2) & "),");
      IO.Put_Line ("a clear path from there to the edges, as well as");
      IO.Put_Line ("a clear path along each edge.");
      IO.Put_Line
        ("the elf visit" & Blocks_Up'Image & " blocks in each direction");

      --  calculate the useful values

      IO.Put_Line ("Calculating progress; please wait a few seconds...");

      From_Top_Ctr   :=
        Part_1
          (Side_Length - 1,
           Location_Record'
             (Row => Side_Range'First, Col => Side_Length / 2 + 1));
      From_Bot_Ctr   :=
        Part_1
          (Side_Length - 1,
           Location_Record'
             (Row => Side_Range'Last, Col => Side_Length / 2 + 1));
      From_Right_Mid :=
        Part_1
          (Side_Length - 1,
           Location_Record'
             (Row => Side_Length / 2 + 1, Col => Side_Range'Last));
      From_Left_Mid  :=
        Part_1
          (Side_Length - 1,
           Location_Record'
             (Row => Side_Length / 2 + 1, Col => Side_Range'First));

      From_Bot_Right_1 :=
        Part_1
          (Corner_1_Steps,
           Location_Record'(Row => Side_Range'Last, Col => Side_Range'Last));
      From_Bot_Left_1  :=
        Part_1
          (Corner_1_Steps,
           Location_Record'(Row => Side_Range'Last, Col => Side_Range'First));
      From_Top_Right_1 :=
        Part_1
          (Corner_1_Steps,
           Location_Record'(Row => Side_Range'First, Col => Side_Range'Last));
      From_Top_Left_1  :=
        Part_1
          (Corner_1_Steps,
           Location_Record'(Row => Side_Range'First, Col => Side_Range'First));

      From_Bot_Right_2 :=
        Part_1
          (Corner_2_Steps,
           Location_Record'(Row => Side_Range'Last, Col => Side_Range'Last));
      From_Bot_Left_2  :=
        Part_1
          (Corner_2_Steps,
           Location_Record'(Row => Side_Range'Last, Col => Side_Range'First));
      From_Top_Right_2 :=
        Part_1
          (Corner_2_Steps,
           Location_Record'(Row => Side_Range'First, Col => Side_Range'Last));
      From_Top_Left_2  :=
        Part_1
          (Corner_2_Steps,
           Location_Record'(Row => Side_Range'First, Col => Side_Range'First));

      Full_1 :=
        Part_1
          (Side_Length + Side_Length / 2,
           Location_Record'
             (Row => Side_Range'Last, Col => Side_Range'Last / 2));
      Full_2 :=
        Part_1
          (Side_Length + Side_Length / 2 + 1,
           Location_Record'
             (Row => Side_Range'Last, Col => Side_Range'Last / 2));

      --  diagram

      IO.Put_Line
        ("Visitation pattern every" & Natural'Image (Side_Length * 2) &
         " steps after the" & Natural'Image (Side_Length / 2) & "th");
      IO.Put ("     ");
      Nat_IO.Put (From_Bot_Right_2, 5);
      Nat_IO.Put (From_Bot_Ctr, 5);
      Nat_IO.Put (From_Bot_Left_2, 5);
      IO.Put ("     ");
      IO.New_Line;

      Nat_IO.Put (From_Bot_Right_2, 5);
      Nat_IO.Put (From_Bot_Right_1, 5);
      Nat_IO.Put (Full_1, 5);
      Nat_IO.Put (From_Bot_Left_1, 5);
      Nat_IO.Put (From_Bot_Left_2, 5);
      IO.New_Line;

      Nat_IO.Put (From_Left_Mid, 5);
      Nat_IO.Put (Full_1, 5);
      Nat_IO.Put (Full_2, 5);
      Nat_IO.Put (Full_1, 5);
      Nat_IO.Put (From_Right_Mid, 5);
      IO.New_Line;

      Nat_IO.Put (From_Top_Right_2, 5);
      Nat_IO.Put (From_Top_Right_1, 5);
      Nat_IO.Put (Full_1, 5);
      Nat_IO.Put (From_Top_Left_1, 5);
      Nat_IO.Put (From_Top_Left_2, 5);
      IO.New_Line;

      IO.Put ("     ");
      Nat_IO.Put (From_Top_Right_2, 5);
      Nat_IO.Put (From_Top_Ctr, 5);
      Nat_IO.Put (From_Top_Left_2, 5);
      IO.Put ("     ");
      IO.New_Line;

      --  report

      Result := 4 * (Blocks_Up / 2)**2 * Ludicrous_Size (Full_1);
      Result :=
        @ +
        (4 * Blocks_Up / 2 * (Blocks_Up / 2 - 1) + 1) *
          Ludicrous_Size (Full_2);
      Result := @ + Ludicrous_Size (From_Bot_Ctr);
      Result := @ + Ludicrous_Size (From_Top_Ctr);
      Result := @ + Ludicrous_Size (From_Left_Mid);
      Result := @ + Ludicrous_Size (From_Right_Mid);
      Result := @ + Ludicrous_Size (From_Bot_Right_2) * Blocks_Up;
      Result := @ + Ludicrous_Size (From_Top_Right_2) * Blocks_Up;
      Result := @ + Ludicrous_Size (From_Top_Left_2) * Blocks_Up;
      Result := @ + Ludicrous_Size (From_Bot_Left_2) * Blocks_Up;
      Result := @ + Ludicrous_Size (From_Bot_Right_1) * (Blocks_Up - 1);
      Result := @ + Ludicrous_Size (From_Top_Right_1) * (Blocks_Up - 1);
      Result := @ + Ludicrous_Size (From_Top_Left_1) * (Blocks_Up - 1);
      Result := @ + Ludicrous_Size (From_Bot_Left_1) * (Blocks_Up - 1);

      return Result;

   end Part_2;

begin
   Map_IO.Read_Input;
   Find_Start;
   IO.Put_Line
     ("On the" & Part_1_Steps'Image & "th step, the elf can visit exactly" &
      Part_1 (Part_1_Steps, Start_Position)'Image & " plots");

   --  an exploratory computation to test conjectures
   --
   --  IO.Put_Line (Part_2 (589)'Image);

   IO.Put_Line (Part_2 (26_501_365)'Image);

   --  example of how to use Test_Approach
   --
   --  IO.Put_Line
   --    ("in the requested number of steps, our elf could reach" &
   --     Test_Approach'Image & " plots");
end Day21;
