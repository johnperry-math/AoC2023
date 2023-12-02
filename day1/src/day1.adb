--  Advent of Code 2018
--
--  John Perry
--
--  Day 1: Trebuchet?!
--
--  part 1: Find the first and last digits in each string
--
--  part 2: First the first and last numbers in each string
--          (either a digit or the digit's spelling)

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Characters.Handling;

procedure Day1 is

   package IO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   --  SECTION
   --  global types and variables

   package Calibration_Value_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Natural);

   Calibration_Values : Calibration_Value_Vecs.Vector;
   Correct_Values : Calibration_Value_Vecs.Vector;

   --  SECTION
   --  Utility functions

   function Digit (C : Character) return Natural is
   --  returns the digit representation of C

      (Natural'Value ([1 => C]) - Natural'Value ([1 => '0']))

   with
      Pre => Character'Pos (C)
         in Character'Pos ('1') .. Character'Pos ('9'),
      Post => Digit'Result in 1 .. 9;

   --  SECTION
   --  Part 1

   function Naive_Parse (S : String) return Natural is
   --  returns the two-digit number
   --  whose tens digit is the first digit to appear in S and
   --  whose ones digit is the last digit to appear in S

      Idx : Positive := 1;
      First_Digit, Last_Digit : Natural;

   begin

      --  find first digit
      while not CH.Is_Digit (S (Idx)) loop
         Idx := @ + 1;
      end loop;
      First_Digit := Digit (S (Idx));

      --  find last digit
      while Idx <= S'Last loop
         if CH.Is_Digit (S (Idx)) then
            Last_Digit := Digit (S (Idx));
         end if;
         Idx := @ + 1;
      end loop;

      return First_Digit * 10 + Last_Digit;

   end Naive_Parse;

   --  SECTION
   --  Part 2

   type Parsed_Digit (Valid : Boolean := False) is record
      Value : Natural;
   end record;

   function Parse_Word (S : String) return Parsed_Digit is
   --  returns a valid parsed digit if the string starting at S
   --  spells a digit from 1 to 9

      Idx : constant Positive := S'First;
      Result : Parsed_Digit;

   begin

      --  we proceed by the size of the spelling

      --  one, two, six?
      if Idx + 2 <= S'Last then
         if S (Idx .. Idx + 2) = "one" then
            Result := Parsed_Digit'(Valid => True, Value => 1);
         elsif S (Idx .. Idx + 2) = "two" then
            Result := Parsed_Digit'(Valid => True, Value => 2);
         elsif S (Idx .. Idx + 2) = "six" then
            Result := Parsed_Digit'(Valid => True, Value => 6);
         end if;
      end if;

      --  four, five, nine?
      if Idx + 3 <= S'Last then
         if S (Idx .. Idx + 3) = "four" then
            Result := Parsed_Digit'(Valid => True, Value => 4);
         elsif S (Idx .. Idx + 3) = "five" then
            Result := Parsed_Digit'(Valid => True, Value => 5);
         elsif S (Idx .. Idx + 3) = "nine" then
            Result := Parsed_Digit'(Valid => True, Value => 9);
         end if;
      end if;

      --  three, seven, eight?
      if Idx + 4 <= S'Last then
         if S (Idx .. Idx + 4) = "three" then
            Result := Parsed_Digit'(Valid => True, Value => 3);
         elsif S (Idx .. Idx + 4) = "seven" then
            Result := Parsed_Digit'(Valid => True, Value => 7);
         elsif S (Idx .. Idx + 4) = "eight" then
            Result := Parsed_Digit'(Valid => True, Value => 8);
         end if;
      end if;

      return Result;

   end Parse_Word;

   function Correct_Parse (S : String) return Natural is
   --  returns the two-digit number
   --  whose tens digit is the first digit, possibly spelled, to appear in S and
   --  whose ones digit is the last digit, possibly spelled, in S

      First_Digit : Natural := 0;
      Last_Digit : Positive;

   begin

      --  IO.Put_Line (S);

      for Idx in S'Range loop

         if CH.Is_Digit (S (Idx)) then

            if First_Digit = 0 then
               First_Digit := Digit (S (Idx));
               Last_Digit := First_Digit;
            else
               Last_Digit := Digit (S (Idx));
            end if;

         else

            declare
               Maybe_Parsed : constant Parsed_Digit
                  := Parse_Word (S (Idx .. S'Last));
            begin

               if Maybe_Parsed.Valid then
                  if First_Digit = 0 then
                     First_Digit := Maybe_Parsed.Value;
                     Last_Digit := First_Digit;
                  else
                     Last_Digit := Maybe_Parsed.Value;
                  end if;
               end if;
            end;

         end if;

      end loop;

      return First_Digit * 10 + Last_Digit;

   end Correct_Parse;

   --  SECTION
   --  I/O and Parts 1, 2

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      while not IO.End_Of_File (F) loop

         declare
            S : constant String := IO.Get_Line (F);
         begin

            Calibration_Values.Append (Naive_Parse (S));
            Correct_Values.Append (Correct_Parse (S));
            --  IO.Put_Line (Calibration_Values.Last_Element'Image);
            --  IO.Put_Line (Correct_Values.Last_Element'Image);

         end;

      end loop;

      IO.Put_Line ("Read" & Calibration_Values.Length'Image & " values");

   end Read_Input;

begin
   Read_Input;
   IO.Put_Line (
      "Sum of Calibration Values is"
      & Natural'Image (Calibration_Values'Reduce ("+", 0))
   );
   IO.Put_Line (
      "Sum of Corrected Calibration Values is"
      & Natural'Image (Correct_Values'Reduce ("+", 0))
   );
end Day1;
