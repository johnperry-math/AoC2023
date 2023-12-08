pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 3: Gear Ratios
--
--  part 1: determine which entries in the map are part numbers;
--          sum their values
--
--  part 2: determine which locations in the map are gears;
--          sum their gear "ratios"

with Ada.Text_IO;
with Ada.Characters.Handling;

procedure Day3 is

   package IO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   --  SECTION
   --  global types and variables

   subtype Constraints is Positive range 1 .. 140;
   subtype Schematic_Row is String (Constraints);
   Schematic : array (Constraints) of Schematic_Row;

   type Location is record
      Row, Col : Constraints;
   end record;

   --  SECTION
   --  useful subprograms

   function Digit (C : Character) return Natural is
   --  returns the digit representation of C

     (Natural'Value ([1 => C]) - Natural'Value ([1 => '0']));

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      for Row in Schematic'Range loop
         Schematic (Row) := IO.Get_Line (Input);
      end loop;
   end Read_Input;

   --  SECTION
   --  Part 1

   UsedLocations : array (Constraints, Constraints) of Boolean :=
     [others => [others => False]];

   function Expand_Unused (Row, Col : Constraints) return Natural is
      --  returns the value of the number located at the given Row and Col
      --  SO LONG AS IT'S UNUSED; otherwise, returns 0

      Result      : Natural     := 0;
      Left, Right : Constraints := Col;

   begin

      --  expand number left
      while Left > Constraints'First
        and then CH.Is_Decimal_Digit (Schematic (Row) (Left - 1))
      loop
         Left := @ - 1;
      end loop;

      --  expand number right
      while Right < Constraints'Last
        and then CH.Is_Decimal_Digit (Schematic (Row) (Right + 1))
      loop
         Right := @ + 1;
      end loop;

      --  now compute it... if it hasn't been used yet
      if not UsedLocations (Row, Left) then
         for Idx in Left .. Right loop
            Result := Result * 10 + Digit (Schematic (Row) (Idx));
         end loop;
         UsedLocations (Row, Left) := True;
      end if;

      return Result;

   end Expand_Unused;

   function Unused_Neighbors (Row, Col : Constraints) return Natural is
      --  returns the sum of the unused neigbors of the symbol
      --  at the given Row and Col

      Result : Natural := 0;

   begin

      for Row_Offset in -1 .. 1 when Row + Row_Offset in Constraints loop
         for Col_Offset in -1 .. 1 when Col + Col_Offset in Constraints loop

            if CH.Is_Decimal_Digit
                (Schematic (Row + Row_Offset) (Col + Col_Offset))
            then
               Result :=
                 @ + Expand_Unused (Row + Row_Offset, Col + Col_Offset);
            end if;

         end loop;
      end loop;

      return Result;

   end Unused_Neighbors;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin

      for Row in Schematic'Range loop

         declare
            Line renames Schematic (Row);
         begin

            for Col in Line'Range loop
               declare
                  C renames Line (Col);
               begin
                  if not (CH.Is_Decimal_Digit (C) or else C = '.') then
                     Result := @ + Unused_Neighbors (Row, Col);
                  end if;
               end;
            end loop;

         end;

      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   type Two_Locations (Valid : Boolean) is record
      case Valid is
         when True =>
            First, Second : Location;
         when False =>
            null;
      end case;
   end record;

   function Expand (Row, Col : Constraints) return Location is
      --  returns the number located at the given Row and Col

      Left : Constraints := Col;

   begin

      while Left - 1 in Constraints
        and then CH.Is_Decimal_Digit (Schematic (Row) (Left - 1))
      loop
         Left := @ - 1;
      end loop;

      return Location'(Row => Row, Col => Left);

   end Expand;

   Too_Many : exception;
   --  raised when Adjacent_To_Two_Parts counts too many gear ratios;
   --  this should never happen

   function Two_Adjacencies (Row, Col : Constraints) return Two_Locations
      --  if only two numbers are adjacent to the location
      --  at the given Row, Col, then this returns Two_Locations
      --  with Valid set to True and the First and Second locations
      --  set to the positions of the two numbers
      --
      --  otherwise, returns Two_Locations with Valid set to False
      is

      Adjacencies                                   : Natural := 0;
      First_Location, Second_Location, New_Location : Location;

   begin

      for Row_Offset in -1 .. 1 when Row + Row_Offset in Constraints loop
         for Col_Offset in -1 .. 1 when Col + Col_Offset in Constraints loop

            if CH.Is_Decimal_Digit
                (Schematic (Row + Row_Offset) (Col + Col_Offset))
            then

               New_Location := Expand (Row + Row_Offset, Col + Col_Offset);

               case Adjacencies is

                  when 0 =>
                     Adjacencies    := 1;
                     First_Location := New_Location;

                  when 1 =>
                     if New_Location /= First_Location then
                        Adjacencies     := 2;
                        Second_Location := New_Location;
                     end if;

                  when 2 =>
                     if New_Location /= First_Location
                       and then New_Location /= Second_Location
                     then
                        return Two_Locations'(Valid => False);
                     end if;

                  when others =>
                     raise Too_Many with "How did we get here?!?";

               end case;

            end if;

         end loop;
      end loop;

      if Adjacencies = 2 then
         return
           Two_Locations'
             (Valid  => True, First => First_Location,
              Second => Second_Location);
      else
         return Two_Locations'(Valid => False);
      end if;

   end Two_Adjacencies;

   function Value_At (L : Location) return Natural is
      --  returns the value of the number stored at L
      Left  : Constraints := L.Col;
      Value : Natural     := 0;
   begin

      while CH.Is_Decimal_Digit (Schematic (L.Row) (Left)) loop
         Value := @ * 10 + Digit (Schematic (L.Row) (Left));
         if Left < Constraints'Last then
            Left := @ + 1;
         else
            exit;
         end if;
      end loop;

      return Value;

   end Value_At;

   function Gear_Ratio (Where : Two_Locations) return Natural is
      --  returns the gear "ratio" of the numbers at the given locations
      --  weirdly, a "ratio" here is a "product" in reality,
      --  but what would I know

      First  : constant Natural := Value_At (Where.First);
      Second : constant Natural := Value_At (Where.Second);

   begin
      return First * Second;
   end Gear_Ratio;

   function Part_2 return Natural is
      Result : Natural := 0;
   begin
      --  i wish ada did a better job of encouraging one to avoid this nesting
      for Row in Schematic'Range loop
         declare
            Line renames Schematic (Row);
         begin
            for Col in Line'Range loop
               declare
                  C renames Line (Col);
               begin
                  if C = '*' then
                     declare
                        Adjacencies : constant Two_Locations :=
                          Two_Adjacencies (Row, Col);
                     begin
                        if Adjacencies.Valid then
                           Result := @ + Gear_Ratio (Adjacencies);
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("Sum of part numbers is" & Part_1'Image);
   IO.Put_Line ("Sum of gear ratios is" & Part_2'Image);
end Day3;
