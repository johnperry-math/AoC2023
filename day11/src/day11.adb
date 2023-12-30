pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 11: Cosmic Expansion
--
--  part 1: sum the distances between pairs of galaxies
--          when each empty row/col of space expands by 1
--
--  part 2: repeat, but expand by 1 million

with Ada.Text_IO;
with Ada.Containers.Vectors;

with Common;

procedure Day11 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   type Distance is range 0 .. 2**64 - 1;

   --  SUBSECTION
   --  things and locations

   type Object is (Galaxy, Space);

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object is
     (case Symbol is when '#' => Galaxy, when '.' => Space,
        when others => raise Invalid_Symbol);

   function Serialize (O : Object) return Character is
     (case O is when Galaxy => '#', when Space => '.');

   Initial_Dimension : constant Positive := 140;
   --  dimension of the initial universe

   package Map_Package is new Common.Two_Dimensional_Map
     (Row_Length => Initial_Dimension, Col_Length => Initial_Dimension,
      Object     => Object);

   use Map_Package;

   package Map_Package_IO is new Common.Two_Dimensional_Map_IO
     (Map_Package => Map_Package);

   package Galaxy_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Location_Record);

   Galaxies : Galaxy_Vecs.Vector;

   --  SECTION
   --  Parts 1 and 2

   procedure Find_Galaxies is
   begin
      for Row in Map_Package.Row_Range loop
         for Col in Map_Package.Col_Range loop
            if Map_Package.Map (Row, Col) = Galaxy then
               Galaxies.Append (Location_Record'(Row, Col));
            end if;
         end loop;
      end loop;
   end Find_Galaxies;

   function Sum_Distances_Expanded_By (Expansion : Distance) return Distance is
      --  returns the sum of Manhattan distances
      --  of the indicated galaxies in the universe,
      --  taking the indicated expansion into account

      Result : Distance := 0;

      U renames Map;

      --  empty rows, columns of the universe
      Empty_Col : array (U'Range (2)) of Boolean := [others => False];
      Empty_Row : array (U'Range (1)) of Boolean := [others => False];

   begin

      --  identify empty rows, columns
      for Row in U'Range (1) loop
         if (for all Col in U'Range (2) => U (Row, Col) = Space) then
            Empty_Row (Row) := True;
         end if;
      end loop;
      for Col in U'Range (2) loop
         if (for all Row in U'Range (1) => U (Row, Col) = Space) then
            Empty_Col (Col) := True;
         end if;
      end loop;

      --  now find the distances, adjusting for expansion
      for First in Galaxies.First_Index .. Galaxies.Last_Index loop
         for Second in First + 1 .. Galaxies.Last_Index loop

            declare

               L1 : constant Location_Record := Galaxies (First);
               L2 : constant Location_Record := Galaxies (Second);

               --  rows and cols involved from first to second galaxy
               Min_Row : constant Positive := Positive'Min (L1.Row, L2.Row);
               Min_Col : constant Positive := Positive'Min (L1.Col, L2.Col);
               Max_Row : constant Positive := Positive'Max (L1.Row, L2.Row);
               Max_Col : constant Positive := Positive'Max (L1.Col, L2.Col);

               --  observed Manhattan distance
               Pair_Distance : Distance :=
                 Distance ((Max_Row - Min_Row) + (Max_Col - Min_Col));

            begin

               --  for each empty row/col, add distance for expansion
               --  when I first did this, I didn't subtract 1. oops...
               for Row in Min_Row .. Max_Row loop
                  if Empty_Row (Row) then
                     Pair_Distance := @ + Expansion - 1;
                  end if;
               end loop;
               for Col in Min_Col .. Max_Col loop
                  if Empty_Col (Col) then
                     Pair_Distance := @ + Expansion - 1;
                  end if;
               end loop;

               Result := @ + Pair_Distance;

            end;
         end loop;
      end loop;

      return Result;

   end Sum_Distances_Expanded_By;

begin
   Map_Package_IO.Read_Input ("input.txt");
   Find_Galaxies;
   IO.Put_Line
     ("the sum of distances between galaxies is" &
      Sum_Distances_Expanded_By (2)'Image);
   IO.Put_Line
     ("the sum of distances between galaxies is" &
      Sum_Distances_Expanded_By (1_000_000)'Image);
end Day11;
