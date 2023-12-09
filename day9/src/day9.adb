pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 9: Mirage Maintenance
--
--  part 1: extend a sequence one unit to the right
--
--  part 2: extend the same sequence one unit to the left

with Ada.Text_IO;
with Ada.Containers.Vectors;
use all type Ada.Containers.Count_Type;

procedure Day9 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  values in sequence and their I/O

   type Value is range -2**32 .. 2**32 - 1;

   package Value_IO is new IO.Integer_IO (Num => Value);

   --  SUBSECTION
   --  lists of values, and lists of lists of values

   package Sequence_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Value);

   function "=" (Left, Right : Sequence_Vectors.Vector) return Boolean is
     (Left.Length = Right.Length
      and then
      (for all Ith in Left.First_Index .. Left.Last_Index =>
         Left (Ith) = Right (Ith)));

   package Sequence_Sequences is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Sequence_Vectors.Vector);

   All_Sequences : Sequence_Sequences.Vector;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            S    : constant String := IO.Get_Line (Input);
            Pos  : Positive        := S'First;
            V    : Value;
            List : Sequence_Vectors.Vector;
         begin
            while Pos <= S'Last loop
               Value_IO.Get (S (Pos .. S'Last), V, Pos);
               List.Append (V);
               Pos := @ + 2;
            end loop;
            All_Sequences.Append (List);
         end;
      end loop;
   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   type Direction is (Left, Right);
   --  which direction to extend a sequence

   function Extend
     (Sequence : Sequence_Vectors.Vector; From : Direction) return Value
   is
      --  computes the value that would extend Sequence
      --  in the direction indicated; does not modify Sequence

      Differences     : Sequence_Vectors.Vector;
      --  differences between the elements of Sequence
      Nonzero         : Boolean        := False;
      --  whether a nonzero apepars in Differences
      Build_From      : constant Value :=
        (if From = Left then Sequence.First_Element
         else Sequence.Last_Element);
      --  last element in the direction requested
      Last_Difference : Value;
      --  last difference in the direction requested

   begin

      for Ith in 2 .. Positive (Sequence.Length) loop
         Differences.Append (Value'(Sequence (Ith) - Sequence (Ith - 1)));
         if Differences.Last_Element /= 0 then
            Nonzero := True;
         end if;
      end loop;

      Last_Difference := (if Nonzero then Extend (Differences, From) else 0);

      return
        Build_From +
        (if From = Left then -Last_Difference else Last_Difference);

   end Extend;

   function Part_1 return Value is
      --  extend to the right

      Result : Value := 0;

   begin

      for Sequence of All_Sequences loop
         Result := @ + Extend (Sequence, Right);
      end loop;

      return Result;

   end Part_1;

   function Part_2 return Value is
      --  extend to the left

      Result : Value := 0;

   begin

      for Sequence of All_Sequences loop
         Result := @ + Extend (Sequence, Left);
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("The sum of extrapolated values on right is " & Part_1'Image);
   IO.Put_Line ("The sum of extrapolated values on left is " & Part_2'Image);
end Day9;
