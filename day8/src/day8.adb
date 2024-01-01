pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 8: Haunted Wasteland
--
--  part 1: how long does it take to get from AAA to ZZZ?
--
--  part 2: how long does it take to get from every node ending with A
--          to a node ending with Z?

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with Common;

procedure Day8 is

   package IO renames Ada.Text_IO;
   package Math is new Common.Mathematics
     (Base_Type => Long_Integer, Zero => 0);

   --  SECTION
   --  global types & variables

   subtype Node is String (1 .. 3);

   type Direction is (Left, Right);
   --  which direction to turn at a given node

   type Connection is array (Direction) of Node;

   package Directions_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Direction);
   Directions : Directions_Vectors.Vector;
   --  the map's left/right instructions

   function Node_Hash (N : Node) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type
        (Character'Pos (N (1)) * 26**2 + Character'Pos (N (2)) * 26 +
         Character'Pos (N (3))));
   package Node_To_Connection_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Node, Element_Type => Connection, Equivalent_Keys => "=",
      Hash     => Node_Hash);
   Map : Node_To_Connection_Maps.Map;
   --  the map itself

   --  i technically don't need to define Ghost_Starts until part 2, but
   --  it's easier and more efficient to set up the start locations
   --  when reading the input; otherwise i'd have to search through Map
   package Ghost_Start_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Node);
   Ghost_Starts : Ghost_Start_Vectors.Vector;
   --  whence start the ghosts

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         S : constant String := IO.Get_Line (Input);
      begin
         for C of S loop
            Directions.Append (if C = 'L' then Left else Right);
         end loop;
      end;
      IO.Skip_Line (Input, 1);
      while not IO.End_Of_File (Input) loop
         declare
            S : constant String := IO.Get_Line (Input);
            Source, Left_Node, Right_Node : Node;
         begin
            Source     := S (1 .. 3);
            Left_Node  := S (8 .. 10);
            Right_Node := S (13 .. 15);
            Map.Insert
              (Source, Connection'(Left => Left_Node, Right => Right_Node));
            if Source (3) = 'A' then
               Ghost_Starts.Append (Source);
            end if;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   function Part_1 (Start : Node) return Natural is
      D    : Direction;
      Step : Natural := 0;
      N    : Node    := Start;
   begin
      while N /= "ZZZ" loop
         D    := Directions (Step mod Natural (Directions.Length));
         N    := Map (N) (D);
         Step := @ + 1;
      end loop;
      return Step;
   end Part_1;

   --  SUBSECTION
   --  Part 2

   function Part_2 return Long_Integer is

      D    : Direction;
      Step : Natural := 0;

      Locations    : array (1 .. Natural (Ghost_Starts.Length)) of Node;
      Path_Lengths : array (1 .. Natural (Ghost_Starts.Length)) of Natural :=
        [others => 0];

      Result : Long_Integer := 1;

   begin

      --  set the ghosts in their locations
      for Ith in 1 .. Ghost_Starts.Last_Index loop
         Locations (Ith) := Ghost_Starts (Ith);
      end loop;

      --  loop until they're all in location
      while (for some L of Locations => L (3) /= 'Z') loop

         --  to save time, we stop moving them once they reach a terminus
         --  by checking the Path_Lengths variable
         for Ith in Locations'Range when Path_Lengths (Ith) = 0 loop

            D := Directions (Step mod Natural (Directions.Length));
            Locations (Ith) := Map (Locations (Ith)) (D);

            if Locations (Ith) (3) = 'Z' then
               --  I'll leave this in for entertainment purposes
               IO.Put_Line
                 (Ghost_Starts (Ith) & " reaches " & Locations (Ith) &
                  " after" & Step'Image & " steps");
               Path_Lengths (Ith) := Step + 1;
            end if;

         end loop;

         Step := @ + 1;

      end loop;

      --  the cycles are regular, in part because the ghosts' paths cycle
      for Ith in Locations'Range loop
         Result := Math.Lcm (Result, Long_Integer (Path_Lengths (Ith)));
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line
     ("It takes" & Part_1 ("AAA")'Image & " steps to reach ZZZ from AAA");
   IO.Put_Line ("It takes" & Part_2'Image & " steps when traversing all xxA");
end Day8;
