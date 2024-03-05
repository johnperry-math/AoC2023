pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 18: Lavaduct Lagoon
--
--  part 1: determine the size of the trench
--
--  part 2: whoops, misread directions. lather, rinse, repeat

with Ada.Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

with Common;

procedure Day18 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Num => Natural);

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;
   Filename      : constant String  :=
     (if Doing_Example then "example.txt" else "input.txt");

   --  SUBSECTION
   --  map (for part 1) and locations (for part 2)

   use Common.Two_Dimensional_Motion;

   type Map_Array is array (Integer range <>, Integer range <>) of Natural;

   type Location_Record is record
      Row, Col : Integer;
   end record;

   package Location_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Location_Record);

   type Initial_Data (First_Row, Last_Row, First_Col, Last_Col : Integer) is
   record
      Map       : Map_Array (First_Row .. Last_Row, First_Col .. Last_Col);
      Locations : Location_Vectors.Vector;
   end record;

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  read input for part 1

   function Read_Input return Initial_Data is
      --  initial map will have 255**3 in empty spots and a color elsewhere
      --  (man, was THAT a red herring...)

      Invalid_Input : exception;

      Input : IO.File_Type;

      Distance : Natural;

      Row, Col                               : Integer := 0;
      Top_Row, Bott_Row, Left_Col, Right_Col : Integer := 0;
      --  map dimensions

      Locations : Location_Vectors.Vector;

   begin

      IO.Open (Input, IO.In_File, Filename);

      while not IO.End_Of_File (Input) loop
         declare
            Line  : constant String := IO.Get_Line (Input);
            Dummy : Positive;
         begin

            Nat_IO.Get (Line (3 .. Line'Last), Distance, Dummy);
            case Line (1) is
               when 'U' =>
                  Row := @ - Distance;
               when 'D' =>
                  Row := @ + Distance;
               when 'L' =>
                  Col := @ - Distance;
               when 'R' =>
                  Col := @ + Distance;
               when others =>
                  raise Invalid_Input;
            end case;

         end;

         Locations.Append (Location_Record'(Row, Col));

         Top_Row   := Integer'Min (Top_Row, Row);
         Bott_Row  := Integer'Max (Bott_Row, Row);
         Left_Col  := Integer'Min (Left_Col, Col);
         Right_Col := Integer'Max (Right_Col, Col);

      end loop;

      IO.Close (Input);

      IO.Put_Line
        ("the map is" & Natural'Image (Bott_Row - Top_Row) & " x" &
         Natural'Image (Right_Col - Left_Col) & " units wide");
      IO.Put_Line
        ("and ranges over" & Top_Row'Image & " .." & Bott_Row'Image &
         Left_Col'Image & " .." & Right_Col'Image);

      --  create a map for original approach to Part 1
      declare
         Map :
           Map_Array
             (Top_Row - 1 .. Bott_Row + 1, Left_Col - 1 .. Right_Col + 1) :=
           [others => [others => 255**3]];
      begin

         Row := 0;
         Col := 0;

         IO.Open (Input, IO.In_File, Filename);

         while not IO.End_Of_File (Input) loop
            declare
               Line  : constant String := IO.Get_Line (Input);
               Pos   : Positive;
               Color : Natural;
            begin

               Nat_IO.Get (Line (3 .. Line'Last), Distance, Pos);
               Pos := @ + 2;
               Nat_IO.Get
                 ("16#" & Line (Pos + 3 .. Line'Last - 1) & "#", Color, Pos);

               for Each in 1 .. Distance loop
                  case Line (1) is
                     when 'U' =>
                        Row := @ - 1;
                     when 'D' =>
                        Row := @ + 1;
                     when 'L' =>
                        Col := @ - 1;
                     when 'R' =>
                        Col := @ + 1;
                     when others =>
                        raise Invalid_Input;
                  end case;
                  Map (Row, Col) := Color;
               end loop;

            end;
         end loop;

         IO.Close (Input);

         return
           Initial_Data'
             (First_Row => Top_Row - 1, Last_Row => Bott_Row + 1,
              First_Col => Left_Col - 1, Last_Col => Right_Col + 1, Map => Map,
              Locations => Locations);

      end;
   end Read_Input;

   --  SUBSECTION
   --  reread input for part 2

   function Reread_Input return Location_Vectors.Vector is

      Invalid_Input : exception;

      Input    : IO.File_Type;
      Distance : Natural;

      Row, Col : Integer;   --  irritating that this didn't warn me

      Locations : Location_Vectors.Vector;

   begin

      IO.Open (Input, IO.In_File, Filename);

      while not IO.End_Of_File (Input) loop
         declare
            Line  : constant String := IO.Get_Line (Input);
            Pos   : Positive;
            Color : Natural;
         begin

            --  read incorrect distance to obtain Pos of correct dist
            Nat_IO.Get (Line (3 .. Line'Last), Distance, Pos);

            --  determine correct distance
            Nat_IO.Get
              ("16#" & Line (Pos + 4 .. Line'Last - 1) & "#", Color, Pos);
            Distance := (Color - (Color mod 16)) / 16;

            case Color mod 16 is
               when 3 =>         -- U
                  Row := @ - Distance;
               when 1 =>         -- D
                  Row := @ + Distance;
               when 2 =>         -- L
                  Col := @ - Distance;
               when 0 =>         -- R
                  Col := @ + Distance;
               when others =>
                  raise Invalid_Input;
            end case;

         end;

         Locations.Append (Location_Record'(Row, Col));

      end loop;

      IO.Close (Input);
      return Locations;

   end Reread_Input;

   --  SUBSECTION
   --  Output

   pragma Warnings (Off, "is not referenced");
   procedure Put_Location (Location : Location_Record) is
   begin
      IO.Put ("(" & Location.Row'Image & "," & Location.Col'Image & ")");
   end Put_Location;

   procedure Put_Map (Map : Map_Array) is
   begin
      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop
            IO.Put (if Map (Row, Col) = 0 then '.' else '#');
         end loop;
         IO.New_Line;
      end loop;
   end Put_Map;
   pragma Warnings (On, "is not referenced");

   --  SECTION
   --  Part 1

   Map_And_Locations : constant Initial_Data := Read_Input;
   --  ;-)

   procedure Flood_Fill (Map : in out Map_Array) is
      --  floods Map from the outside with 0

      package Location_Interface is new Ada.Containers
        .Synchronized_Queue_Interfaces
        (Element_Type => Location_Record);

      package Location_Queues is new Ada.Containers
        .Unbounded_Synchronized_Queues
        (Queue_Interfaces => Location_Interface);

      To_Do      : Location_Queues.Queue;
      Curr, Next : Location_Record;

   begin

      --  prime BFS
      To_Do.Enqueue ((Row => Map'First (1), Col => Map'First (2)));

      while Natural (To_Do.Current_Use) > 0 loop

         To_Do.Dequeue (Curr);

         for Dir in Direction when Curr.Row + Deltas (Dir).DRow in
           Map'Range (1)
         and then Curr.Col + Deltas (Dir).DCol in Map'Range (2)
         loop

            Next :=
              Location_Record'
                (Row => Curr.Row + Deltas (Dir).DRow,
                 Col => Curr.Col + Deltas (Dir).DCol);
            if Map (Next.Row, Next.Col) = 255**3 then
               Map (Next.Row, Next.Col) := 0;
               To_Do.Enqueue (Next);
            end if;

         end loop;

      end loop;

   end Flood_Fill;

   function Part_1 (Map : Map_Array) return Natural is
      --  quite easy with flood fill
      Result        : Natural   := 0;
      Alternate_Map : Map_Array := Map;
   begin

      Flood_Fill (Alternate_Map);

      for Row in Alternate_Map'Range (1) loop
         for Col in Alternate_Map'Range (2) loop

            if Alternate_Map (Row, Col) /= 0 then
               Result := @ + 1;
            end if;

         end loop;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2, done the way everyone and his sister did it on Reddit

   type Product_Range is range -2**64 .. 2**64 - 1;

   function Shoelace_Formula
     (Locations : Location_Vectors.Vector) return Product_Range
   is

      Result : Product_Range := 0;

   begin

      for Ith in Locations.First_Index .. Locations.Last_Index - 1 loop
         Result :=
           @ +
           (Product_Range (Locations (Ith).Row) *
            Product_Range (Locations (Ith + 1).Col) -
            Product_Range (Locations (Ith).Col) *
              Product_Range (Locations (Ith + 1).Row));
      end loop;

      Result :=
        @ +
        (Product_Range (Locations.Last_Element.Row) *
         Product_Range (Locations.First_Element.Col) -
         Product_Range (Locations.Last_Element.Col) *
           Product_Range (Locations.First_Element.Row));

      return abs (Result) / 2;

   end Shoelace_Formula;

   function Picks_Formula
     (Locations : Location_Vectors.Vector) return Product_Range
   is
      --  this is NOT QUITE Pick's Theorem,
      --  on account of the border NOT QUITE capturing all the points
      Perimeter : Product_Range := 0;
   begin

      for Ith in Locations.First_Index .. Locations.Last_Index - 1 loop
         Perimeter :=
           @ +
           Product_Range
             (abs (Locations (Ith).Row - Locations (Ith + 1).Row) +
              abs (Locations (Ith).Col - Locations (Ith + 1).Col));
      end loop;

      Perimeter :=
        @ +
        Product_Range
          (abs (Locations.Last_Element.Row - Locations.First_Element.Row) +
           abs (Locations.Last_Element.Col - Locations.First_Element.Col));

      return Shoelace_Formula (Locations) + Perimeter / 2 + 1;

   end Picks_Formula;

   function Part_2 (Locations : Location_Vectors.Vector) return Product_Range
   is
   begin
      return Picks_Formula (Locations);
   end Part_2;

   --  SECTION
   --  Part 2 done "right" ;-)
   --
   --  consider the following diagram (gee, where did this come from)
   --  +--------+    +---+
   --  |        |    |   |
   --  |        |    |   |
   --  |        |    |   |
   --  |        +----+   |
   --  |                 |
   --  |                 |
   --  |                 |
   --  |   +-----+       |
   --  |   |     |       |
   --  |   |     |   +---+
   --  |   |     |   |
   --  +---+     +---+
   --  the idea is to find a piece that "sticks out", then "munch" it, like so:
   --  2--------3    +---+
   --  |XXXXXXXX|    |   |
   --  |XXXXXXXX|    |   |   <- numbers indicate vertex labels in Menu_Item
   --  |XXXXXXXX|    |   |
   --  1--------4----+   |
   --  |                 |
   --  |                 |
   --  |                 |
   --  |   +-----+       |
   --  |   |     |       |
   --  |   |     |   +---+
   --  |   |     |   |
   --  +---+     +---+
   --  the area of the entire "meal", is the sum of all the munches
   --
   --  we have to be a little careful, since we don't want to add the notches,
   --  and we also have to watch out for situations where we try to munch this:
   --  +-----------------+
   --  |XXXXXXXXXXXXXXXXX|
   --  |XXXXXXXXXXXXXXXXX|
   --  |XXXXXXXXXXXXXXXXX|
   --  |XXX+-----+XXXXXXX|
   --  |XXX|XXXXX|XXXXXXX|
   --  |   |  |  |   +---+
   --  |   |oops!|   |
   --  +---+     +---+

   type Takeout_Box is record
      Min_Row, Max_Row, Min_Col, Max_Col : Integer;
   end record;
   --  we rarely eat in; we prefer to take out ;-)

   function Contains
     (B : Takeout_Box; Location : Location_Record) return Boolean is
   --  true iff B contains L (inclusive of edges!)

     (Location.Row in B.Min_Row .. B.Max_Row
      and then Location.Col in B.Min_Col .. B.Max_Col);

   type Menu_Item (Valid : Boolean) is record
      case Valid is
         when True =>
            First, Second, Third, Fourth : Positive;
            Cutoff                       : Takeout_Box;
            Area                         : Long_Long_Integer;
         when False =>
            null;
      end case;
   end record;

   function Inquire_About
     (Ith : Positive; Buffet : Location_Vectors.Vector) return Menu_Item
   is
      --  returns a valid menu item when Ith works as the 2nd location
      --  (see Menu_Item and diagram above)
      --  avoids notches by checking to make sure
      --  the turns are in the correct direction wrt digging instructions

      Hth : constant Positive :=
        (if Ith = 1 then Buffet.Last_Index else Ith - 1);
      --  1st location
      Jth : constant Positive :=
        (if Ith = Buffet.Last_Index then 1 else Ith + 1);
      --  3rd location
      Kth : constant Positive :=
        (if Jth = Buffet.Last_Index then 1 else Jth + 1);
      --  4th location

      Cutoff : Takeout_Box;

   begin

      if Buffet (Ith).Row = Buffet (Jth).Row then

         Cutoff.Min_Col := Integer'Min (Buffet (Ith).Col, Buffet (Jth).Col);
         Cutoff.Max_Col := Integer'Max (Buffet (Ith).Col, Buffet (Jth).Col);

         --  NotchGuard (TM) ;-)
         if Buffet (Hth).Row < Buffet (Ith).Row
           and then Buffet (Kth).Row < Buffet (Ith).Row
           and then Buffet (Ith).Col > Buffet (Jth).Col
         then
            Cutoff.Max_Row := Buffet (Ith).Row;
            Cutoff.Min_Row := Integer'Max (Buffet (Hth).Row, Buffet (Kth).Row);

         elsif Buffet (Hth).Row > Buffet (Ith).Row
           and then Buffet (Kth).Row > Buffet (Ith).Row
           and then Buffet (Ith).Col < Buffet (Jth).Col
         then
            Cutoff.Min_Row := Buffet (Ith).Row;
            Cutoff.Max_Row := Integer'Min (Buffet (Hth).Row, Buffet (Kth).Row);

         else
            return Menu_Item'(Valid => False);

         end if;

         if    --  >New< NotchGuard Plus (TM) ;-)
           (for some Lth in Buffet.First_Index .. Buffet.Last_Index =>
              Lth /= Hth and then Lth /= Ith and then Lth /= Jth
              and then Lth /= Kth and then Contains (Cutoff, Buffet (Lth)))
         then
            return Menu_Item'(Valid => False);

         else
            return
              Menu_Item'
                (Valid  => True, First => Hth, Second => Ith, Third => Jth,
                 Fourth => Kth, Cutoff => Cutoff,
                 Area   =>
                   (Long_Long_Integer
                      (abs (Cutoff.Max_Col - Cutoff.Min_Col) + 1) *
                    Long_Long_Integer
                      (abs (Cutoff.Max_Row - Cutoff.Min_Row))));
         end if;

      else     --  Buffet (Ith).Col = Buffet (Jth).Col

         Cutoff.Min_Row := Integer'Min (Buffet (Ith).Row, Buffet (Jth).Row);
         Cutoff.Max_Row := Integer'Max (Buffet (Ith).Row, Buffet (Jth).Row);

         if Buffet (Hth).Col < Buffet (Ith).Col
           and then Buffet (Kth).Col < Buffet (Ith).Col
           and then Buffet (Ith).Row < Buffet (Jth).Row
         then
            Cutoff.Max_Col := Buffet (Ith).Col;
            Cutoff.Min_Col := Integer'Max (Buffet (Hth).Col, Buffet (Kth).Col);

         elsif Buffet (Hth).Col > Buffet (Ith).Col
           and then Buffet (Kth).Col > Buffet (Ith).Col
           and then Buffet (Ith).Row > Buffet (Jth).Row
         then
            Cutoff.Min_Col := Buffet (Ith).Col;
            Cutoff.Max_Col := Integer'Min (Buffet (Hth).Col, Buffet (Kth).Col);

         else
            return Menu_Item'(Valid => False);
         end if;

         if
           (for some Lth in Buffet.First_Index .. Buffet.Last_Index =>
              Lth /= Hth and then Lth /= Ith and then Lth /= Jth
              and then Lth /= Kth and then Contains (Cutoff, Buffet (Lth)))
         then
            return Menu_Item'(Valid => False);

         else
            return
              Menu_Item'
                (Valid  => True, First => Hth, Second => Ith, Third => Jth,
                 Fourth => Kth, Cutoff => Cutoff,
                 Area   =>

                   (Long_Long_Integer (abs (Cutoff.Max_Col - Cutoff.Min_Col)) *
                    Long_Long_Integer
                      (abs (Cutoff.Max_Row - Cutoff.Min_Row) + 1)));
         end if;

      end if;
   end Inquire_About;

   procedure Munch (Item : Menu_Item; Buffet : in out Location_Vectors.Vector)
   is
   --  removes item from buffet and updates locations;
   --  DOES NOT prune redundant locations; call Clean_Place_Plate for that
   begin

      if Buffet (Item.First).Row > Buffet (Item.Second).Row then
         Buffet (Item.Second).Row := Item.Cutoff.Max_Row;
         Buffet (Item.Third).Row  := Item.Cutoff.Max_Row;

      elsif Buffet (Item.First).Row < Buffet (Item.Second).Row then
         Buffet (Item.Second).Row := Item.Cutoff.Min_Row;
         Buffet (Item.Third).Row  := Item.Cutoff.Min_Row;

      elsif Buffet (Item.First).Col > Buffet (Item.Second).Col then
         Buffet (Item.Second).Col := Item.Cutoff.Max_Col;
         Buffet (Item.Third).Col  := Item.Cutoff.Max_Col;

      else     --  Buffet (Item.First).Col < Buffet (Item.Second).Col
         Buffet (Item.Second).Col := Item.Cutoff.Min_Col;
         Buffet (Item.Third).Col  := Item.Cutoff.Min_Col;
      end if;

   end Munch;

   procedure Clean_Place (Buffet : in out Location_Vectors.Vector) is
      --  clean up your place after munching, dear ;-)

      Jth      : Positive := Buffet.First_Index;
      Ith, Kth : Positive;
      Changed  : Boolean;

   begin

      loop
         --  repeat until nothing more has been cleaned
         Changed := False;

         while Jth <= Buffet.Last_Index loop

            Ith := (if Jth = 1 then Buffet.Last_Index else Jth - 1);
            Kth := (if Jth = Buffet.Last_Index then 1 else Jth + 1);

            if Buffet (Ith).Row = Buffet (Jth).Row
              and then Buffet (Jth).Row = Buffet (Kth).Row
            then
               Buffet.Delete (Jth);
               Changed := True;
               exit;

            elsif Buffet (Ith).Col = Buffet (Jth).Col
              and then Buffet (Jth).Col = Buffet (Kth).Col

            then
               Buffet.Delete (Jth);
               Changed := True;
               exit;
            end if;

            Jth := @ + 1;
         end loop;

         if not Changed then
            exit;
         end if;

      end loop;

   end Clean_Place;

   function Map_Muncher
     (Locations : Location_Vectors.Vector) return Long_Long_Integer
   is
      --  a "better" way to do part 2
      --  (better only because it was my original idea)

      Result : Long_Long_Integer := 0;

      Buffet : Location_Vectors.Vector := Locations;

   begin

      --  repeat as long as there's sufficient "food" to eat ;-)
      while Natural (Buffet.Length) > 4 loop

         --  look for a place to munch
         for Ith in Buffet.First_Index .. Buffet.Last_Index loop
            declare
               Item : constant Menu_Item := Inquire_About (Ith, Buffet);
            begin

               if Item.Valid then
                  Munch (Item, Buffet);
                  Result := @ + Item.Area;
                  exit;
               end if;

            end;
         end loop;

         if Natural (Buffet.Length) > 4 then
            Clean_Place (Buffet);
         end if;

      end loop;

      --  only 4 remain... munch a "free-to-compute" rectangle
      Result :=
        Result +
        Long_Long_Integer

          ((if Buffet (1).Row = Buffet (4).Row then
              (abs (Buffet (4).Col - Buffet (1).Col) + 1) *
              (abs (Buffet (2).Row - Buffet (1).Row) + 1)
            else (abs (Buffet (4).Row - Buffet (1).Row) + 1) *
              (abs (Buffet (2).Col - Buffet (1).Col) + 1)));

      return Result;

   end Map_Muncher;

begin

   --  feel free to uncomment these; answers should be the same
   IO.Put_Line
     ("The lagoon can hold" & Part_1 (Map_And_Locations.Map)'Image &
      " cubic meters of lava");
   IO.Put_Line
     ("After revision, it can hold" & Part_2 (Reread_Input)'Image &
      " cubic meters");

   IO.Put_Line
     ("By munching:" & Map_Muncher (Map_And_Locations.Locations)'Image);
   IO.Put_Line ("Munching for real:" & Map_Muncher (Reread_Input)'Image);

end Day18;
