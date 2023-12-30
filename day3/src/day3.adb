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

with Common;

procedure Day3 is

   package IO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   use Common.Two_Dimensional_Motion;

   --  SECTION
   --  global types and variables

   Constraint : constant Positive := 140;

   function Deserialize (Symbol : Character) return Character is (Symbol);

   function Serialize (O : Character) return Character is (O);

   package Map_Package is new Common.Two_Dimensional_Map
     (Row_Length => Constraint, Col_Length => Constraint, Object => Character);

   use Map_Package;

   package Map_Package_IO is new Common.Two_Dimensional_Map_IO
     (Map_Package => Map_Package);

   --  SECTION
   --  useful subprograms

   function Digit (C : Character) return Natural is
   --  returns the digit representation of C

     (Natural'Value ([1 => C]) - Natural'Value ([1 => '0']));

   --  SECTION
   --  Part 1

   UsedLocations : array (Row_Range, Col_Range) of Boolean :=
     [others => [others => False]];

   function Expand_Unused (Row : Row_Range; Col : Col_Range) return Natural is
      --  returns the value of the number located at the given Row and Col
      --  SO LONG AS IT'S UNUSED; otherwise, returns 0

      Result      : Natural   := 0;
      Left, Right : Col_Range := Col;

   begin

      --  expand number left
      while Left > Col_Range'First
        and then CH.Is_Decimal_Digit (Map (Row, Left - 1))
      loop
         Left := @ - 1;
      end loop;

      --  expand number right
      while Right < Col_Range'Last
        and then CH.Is_Decimal_Digit (Map (Row, Right + 1))
      loop
         Right := @ + 1;
      end loop;

      --  now compute it... if it hasn't been used yet
      if not UsedLocations (Row, Left) then
         for Idx in Left .. Right loop
            Result := Result * 10 + Digit (Map (Row, Idx));
         end loop;
         UsedLocations (Row, Left) := True;
      end if;

      return Result;

   end Expand_Unused;

   function Unused_Neighbors (Row : Row_Range; Col : Col_Range) return Natural
   is
      --  returns the sum of the unused neigbors of the symbol
      --  at the given Row and Col

      Result : Natural := 0;

   begin

      for Row_Offset in Nudge when Row + Row_Offset in Row_Range loop
         for Col_Offset in Nudge when Col + Col_Offset in Row_Range loop

            if CH.Is_Decimal_Digit (Map (Row + Row_Offset, Col + Col_Offset))
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

      for Row in Row_Range loop
         for Col in Col_Range loop
            declare
               C renames Map (Row, Col);
            begin
               if not (CH.Is_Decimal_Digit (C) or else C = '.') then
                  Result := @ + Unused_Neighbors (Row, Col);
               end if;
            end;
         end loop;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   type Two_Locations (Valid : Boolean) is record
      case Valid is
         when True =>
            First, Second : Location_Record;
         when False =>
            null;
      end case;
   end record;

   function Expand (Row : Row_Range; Col : Col_Range) return Location_Record is
      --  returns the number located at the given Row and Col

      Left : Col_Range := Col;

   begin

      while Left - 1 in Col_Range
        and then CH.Is_Decimal_Digit (Map (Row, Left - 1))
      loop
         Left := @ - 1;
      end loop;

      return Location_Record'(Row => Row, Col => Left);

   end Expand;

   Too_Many : exception;
   --  raised when Adjacent_To_Two_Parts counts too many gear ratios;
   --  this should never happen

   function Two_Adjacencies
     (Row : Row_Range; Col : Col_Range) return Two_Locations
      --  if only two numbers are adjacent to the location
      --  at the given Row, Col, then this returns Two_Locations
      --  with Valid set to True and the First and Second locations
      --  set to the positions of the two numbers
      --
      --  otherwise, returns Two_Locations with Valid set to False

   is

      Adjacencies                                   : Natural := 0;
      First_Location, Second_Location, New_Location : Location_Record;

   begin

      for Row_Offset in Nudge when Row + Row_Offset in Row_Range loop
         for Col_Offset in Nudge when Col + Col_Offset in Col_Range loop

            if CH.Is_Decimal_Digit (Map (Row + Row_Offset, Col + Col_Offset))
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

   function Value_At (L : Location_Record) return Natural is
      --  returns the value of the number stored at L
      Left  : Row_Range := L.Col;
      Value : Natural   := 0;
   begin

      while CH.Is_Decimal_Digit (Map (L.Row, Left)) loop
         Value := @ * 10 + Digit (Map (L.Row, Left));
         if Left < Row_Range'Last then
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
      for Row in Row_Range loop
         for Col in Col_Range loop
            declare
               C renames Map (Row, Col);
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
      end loop;
      return Result;
   end Part_2;

begin
   Map_Package_IO.Read_Input;
   IO.Put_Line ("Sum of part numbers is" & Part_1'Image);
   IO.Put_Line ("Sum of gear ratios is" & Part_2'Image);
end Day3;
