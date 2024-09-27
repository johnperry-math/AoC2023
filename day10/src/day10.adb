pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 10: Pipe Maze
--
--  part 1: determine the locations of the closed loop
--
--  part 2: count the locations within the closed loop

with Ada.Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Common;

procedure Day10 is

   package IO renames Ada.Text_IO;

   use Common.Two_Dimensional_Motion;

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;
   --  this puzzle gave me a REALLY hard time; the examples were REALLY helpful

   --  SUBSECTION
   --  pipes and maps

   type Pipe is
     (Vertical,   --  |
      Horizontal, --  -
      SE_Or_WN,   --  L
      SW_Or_EN,   --  J
      WS_Or_NE,   --  F
      ES_Or_NW,   --  7
      Ground,     --  .
      Start       --  S
   );

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Pipe is
     (case Symbol is when '|' => Vertical, when '-' => Horizontal,
        when 'L' => SE_Or_WN, when 'J' => SW_Or_EN, when '7' => ES_Or_NW,
        when 'F' => WS_Or_NE, when '.' => Ground, when 'S' => Start,
        when others => raise Invalid_Symbol with Symbol'Image);

   function Serialize (P : Pipe) return Character is
     (case P is when Horizontal => '-', when Vertical => '|',
        when SE_Or_WN => 'L', when SW_Or_EN => 'J', when WS_Or_NE => 'F',
        when ES_Or_NW => '7', when Ground => '.', when Start => 'S');

   Side_Length : constant Positive := (if Doing_Example then 20 else 140);

   package Map_Package is new Common.Two_Dimensional_Map
     (Row_Length => Side_Length, Col_Length => Side_Length, Object => Pipe);

   use Map_Package;

   package Map_Package_IO is new Common.Two_Dimensional_Map_IO
     (Doing_Example => Doing_Example, Map_Package => Map_Package);

   subtype Side_Range is Row_Range;

   --  SUBSECTION
   --  locations

   Start_Location : Location_Record;

   procedure Find_Start is
   begin
      for Row in Row_Range loop
         for Col in Col_Range loop
            if Map (Row, Col) = Start then
               Start_Location := (Row, Col);
               return;
            end if;
         end loop;
      end loop;
   end Find_Start;

   Moved_Locations : Location_Sets.Set;

   --  SUBSECTION
   --  the traversed map

   Traversed_Map : array (Side_Range, Side_Range) of Character :=
     [others => [others => ' ']];
   --  where the pipe lies

   --  SECTION
   --  I/O

   pragma Warnings (Off, "is not referenced");
   procedure Put_Traversed_Map is
   --  useful for debugging

   begin

      for Row in Side_Range loop
         for Col in Side_Range loop
            IO.Put (Traversed_Map (Row, Col));
         end loop;
         IO.New_Line;
      end loop;

   end Put_Traversed_Map;

   procedure Put_Map is
   --  useful for debugging

   begin

      for Row in Side_Range loop
         for Col in Side_Range loop

            IO.Put
              ((if Moved_Locations.Contains ((Row, Col)) then 'X'
                else (Serialize (Map (Row, Col)))));

         end loop;
         IO.New_Line;
      end loop;

   end Put_Map;

   pragma Warnings (On, "is not referenced");

   --  SECTION
   --  Parts 1 and 2

   type Animal is record
      Curr, Prev : Location_Record;
   end record;
   --  where the animal was before, and where it is now
   --  we keep track of the "before" to prevent moving where it is now

   --  SUBSECTION
   --  Part 1

   Left_Loop, Cycle_Loop : exception;

   function Can_Move (Here, From : Location_Record) return Boolean is
     (declare Current : constant Pipe := Map (From.Row, From.Col); begin (
      case Map (Here.Row, Here.Col) is
        when Vertical =>      -- |
          (From.Col = Here.Col and then From.Row - Here.Row = 1
           and then Current /= Horizontal
           and then Current /= ES_Or_NW
           and then Current /= WS_Or_NE)
          or else
          (From.Col = Here.Col and then From.Row - Here.Row = -1
           and then Current /= Horizontal
           and then Current /= SW_Or_EN
           and then Current /= SE_Or_WN),
        when Horizontal =>    -- -
          (From.Row = Here.Row and then From.Col - Here.Col = 1
           and then Current /= SE_Or_WN
           and then Current /= WS_Or_NE
           and then Current /= Vertical)
          or else
          (From.Row = Here.Row and then From.Col - Here.Col = -1
           and then Current /= SW_Or_EN
           and then Current /= ES_Or_NW
           and then Current /= Vertical),
        when WS_Or_NE =>   --  F
          (From.Row - Here.Row = 1 and then From.Col = Here.Col
           and then Current /= Horizontal
           and then Current /= ES_Or_NW
           and then Current /= WS_Or_NE)
          or else
          (From.Row = Here.Row and then From.Col - Here.Col = 1
           and then Current /= SE_Or_WN
           and then Current /= WS_Or_NE
           and then Current /= Vertical),
        when ES_Or_NW =>   -- 7
          (From.Row - Here.Row = 1 and then From.Col = Here.Col
           and then Current /= ES_Or_NW
           and then Current /= Horizontal
           and then Current /= WS_Or_NE)
           or else (From.Row = Here.Row and then From.Col - Here.Col = -1
           and then Current /= ES_Or_NW
           and then Current /= SW_Or_EN
           and then Current /= Vertical),
        when SE_Or_WN =>   -- L
          (From.Row - Here.Row = -1 and then From.Col = Here.Col
           and then Current /= Horizontal
           and then Current /= SE_Or_WN
           and then Current /= SW_Or_EN)
          or else (From.Row = Here.Row and then From.Col - Here.Col = 1
           and then Current /= WS_Or_NE
           and then Current /= SE_Or_WN
           and then Current /= Vertical),
        when SW_Or_EN =>   -- J
          (From.Row = Here.Row and then From.Col - Here.Col = -1
           and then Current /= ES_Or_NW
           and then Current /= Vertical
           and then Current /= SW_Or_EN)
          or else (From.Row - Here.Row = -1 and then From.Col = Here.Col
           and then Current /= Horizontal
           and then Current /= SE_Or_WN
           and then Current /= SW_Or_EN),
        when Ground => raise Left_Loop with Here'Image & " " & From'Image,
        when Start => raise Cycle_Loop with From'Image));

   Did_Not_Move : exception;

   procedure Record_Motion (Me : Animal) is
      --  records animal's current location to Traversed_Map

      Row renames Me.Curr.Row;
      Col renames Me.Curr.Col;

   begin

      if Row = Me.Prev.Row then
         if Col < Me.Prev.Col then
            Traversed_Map (Row, Col) := '<';
         else
            Traversed_Map (Row, Col) := '>';
         end if;

      elsif Row < Me.Prev.Row then
         Traversed_Map (Row, Col) := '^';

      else
         Traversed_Map (Row, Col) := 'v';

      end if;

   end Record_Motion;

   procedure Move
     (Me : in out Animal; But_Not_Here : Location_Record := Start_Location)
   is
      --  moves the animal to a valid location

      Option : Location_Record;
      --  where it might move

   begin

      for DRow in Nudge when Me.Curr.Row + DRow in Side_Range loop
         for DCol in Nudge when Me.Curr.Col + DCol in Side_Range loop

            if abs (DRow) /= abs (DCol) then

               Option :=
                 (Row => Me.Curr.Row + DRow, Col => Me.Curr.Col + DCol);

               if Option /= Me.Prev and then Option /= But_Not_Here
                 and then Map (Option.Row, Option.Col) /= Ground
                 and then Can_Move (Option, Me.Curr)
               then
                  Me.Prev := Me.Curr;
                  Me.Curr := Option;
                  return;
               end if;

            end if;

         end loop;
      end loop;

      raise Did_Not_Move with Me.Curr'Image;

   end Move;

   function Part_1 return Natural is
      First, Second : Animal  :=
        (Curr => Start_Location, Prev => Start_Location);
      Step          : Natural := 1;
   begin

      --  get animal's two first moves
      Traversed_Map (Start_Location.Row, Start_Location.Col) := 'S';
      Move (First);
      Record_Motion (First);
      Move (Second, First.Curr);
      Record_Motion (Second);

      loop
         Move (First);
         Record_Motion (First);
         exit when First.Curr = Second.Curr;
         Move (Second);
         Record_Motion (Second);
         exit when First.Curr = Second.Curr;
         Step := @ + 1;
      end loop;

      return Step + 1;
      --  we exited before incrementing step

   end Part_1;

   --  SUBSECTION
   --  Part 2

   --  the strategy here is to rescale the maze to twice its size;
   --  that way, "between" the pipes becomes trivial to "see"
   --  there are now more interior points,
   --  but the ones corresponding to original interior points
   --  are "easy" to identify

   Doubled_Side_Length : constant Natural := 2 * Side_Length;
   subtype Doubled_Side_Range is Integer range 0 .. Doubled_Side_Length;

   type Doubled_Map_Type is
     array (Doubled_Side_Range, Doubled_Side_Range) of Boolean;
   --  True implies Outside the loop

   type Doubled_Location is record
      Row, Col : Doubled_Side_Range;
   end record;

   procedure Flood_Fill (Is_Filled : in out Doubled_Map_Type) is
      --  flood the outside, leaving the inside

      package Queue_Interfaces is new Ada.Containers
        .Synchronized_Queue_Interfaces
        (Element_Type => Doubled_Location);
      package Location_Queues is new Ada.Containers
        .Unbounded_Synchronized_Queues
        (Queue_Interfaces => Queue_Interfaces);

      ToDo          : Location_Queues.Queue;
      Current, Next : Doubled_Location;

   begin

      --  breadth-first search! prime it
      --  for the input, (0,0) is outside the loop,
      --  but not on the last example, so I prime with both points
      --
      --  a more rigorous approach would be to walk along the edge
      --  until we find ground

      ToDo.Enqueue ((0, 0));
      Is_Filled (0, 0) := True;
      ToDo.Enqueue ((Doubled_Side_Range'Last, 0));
      Is_Filled (Doubled_Side_Range'Last, 0) := True;

      --  now BFS it, using Is_Filled itself to prune search paths
      while Natural (ToDo.Current_Use) > 0 loop

         ToDo.Dequeue (Current);

         for DRow in -1 .. 1 when Current.Row + DRow in Doubled_Side_Range loop
            for DCol in -1 .. 1 when Current.Col + DCol in Doubled_Side_Range
            loop

               Next := (Row => Current.Row + DRow, Col => Current.Col + DCol);
               if not Is_Filled (Next.Row, Next.Col) then
                  Is_Filled (Next.Row, Next.Col) := True;
                  ToDo.Enqueue (Next);
               end if;

            end loop;
         end loop;

      end loop;
   end Flood_Fill;

   function Size_Of_Interior (Is_Filled : Doubled_Map_Type) return Natural is
      --  assumes that only the interior of the loop is not filled

      Result : Natural := 0;

   begin

      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop

            if not Is_Filled (2 * Row - 1, 2 * Col - 1) then
               Result := @ + 1;
            end if;

         end loop;
      end loop;

      return Result;

   end Size_Of_Interior;

   function Double_Map return Doubled_Map_Type is
      --  doubles the original Map

      Result : Doubled_Map_Type := [others => [others => False]];

   begin

      for Row in Map'Range (1) loop
         for Col in Map'Range (2) loop

            if Traversed_Map (Row, Col) /= ' ' then

               case Map (Row, Col) is
                  when Vertical =>
                     Result (2 * Row - 1, 2 * Col - 1) := True;
                     Result (2 * Row - 0, 2 * Col - 1) := True;

                  when Horizontal =>
                     Result (2 * Row - 1, 2 * Col - 1) := True;
                     Result (2 * Row - 1, 2 * Col - 0) := True;

                  when SE_Or_WN =>
                     Result (2 * Row - 2, 2 * Col - 1) := True;
                     Result (2 * Row - 1, 2 * Col - 1) := True;
                     Result (2 * Row - 1, 2 * Col - 0) := True;

                  when SW_Or_EN =>
                     Result (2 * Row - 1, 2 * Col - 2) := True;
                     Result (2 * Row - 1, 2 * Col - 1) := True;
                     Result (2 * Row - 2, 2 * Col - 1) := True;

                  when WS_Or_NE =>
                     Result (2 * Row - 0, 2 * Col - 1) := True;
                     Result (2 * Row - 1, 2 * Col - 1) := True;
                     Result (2 * Row - 1, 2 * Col - 0) := True;

                  when ES_Or_NW =>
                     Result (2 * Row - 1, 2 * Col - 2) := True;
                     Result (2 * Row - 1, 2 * Col - 1) := True;
                     Result (2 * Row - 0, 2 * Col - 1) := True;

                  when Ground =>
                     null;

                  when Start =>
                     --  this is a pain

                     Result (2 * Row - 1, 2 * Col - 1) := True;

                     if Row - 1 in Side_Range then
                        case Map (Row - 1, Col) is
                           when Vertical | WS_Or_NE | ES_Or_NW =>
                              Result (2 * Row - 2, 2 * Col - 1) := True;
                           when others =>
                              null;
                        end case;
                     end if;

                     if Row + 1 in Side_Range then
                        case Map (Row + 1, Col) is
                           when Vertical | SE_Or_WN | SW_Or_EN =>
                              Result (2 * Row - 0, 2 * Col - 1) := True;
                           when others =>
                              null;
                        end case;
                     end if;

                     if Col - 1 in Side_Range then
                        case Map (Row, Col - 1) is
                           when Horizontal | SE_Or_WN | WS_Or_NE =>
                              Result (2 * Row - 1, 2 * Col - 2) := True;
                           when others =>
                              null;
                        end case;
                     end if;

                     if Col + 1 in Side_Range then
                        case Map (Row, Col + 1) is
                           when Horizontal | SW_Or_EN | ES_Or_NW =>
                              Result (2 * Row - 1, 2 * Col - 0) := True;
                           when others =>
                              null;
                        end case;
                     end if;

               end case;

            end if;

         end loop;
      end loop;

      return Result;

   end Double_Map;

   procedure Write_Visualization (Map : Doubled_Map_Type) is
      --  who doesn't like a pretty picture?

      Output : IO.File_Type;

      Inside  : constant Natural := 96;
      Outside : constant Natural := 255;

   begin

      IO.Create (Output, IO.Out_File, "visualization.pgm");

      --  header
      IO.Put_Line (Output, "P2");
      IO.Put_Line (Output, Positive'Image (Map'Length (2) * 2 - 2));
      IO.Put_Line (Output, Positive'Image (Map'Length (1) * 2 - 2));
      IO.Put_Line (Output, "255"); -- max color
      IO.New_Line (Output);

      --  data
      for Row in 0 .. Map'Last (1) - 1 loop
         --  double each row for visibility
         for Each in 1 .. 2 loop
            for Col in 0 .. Map'Last (2) - 1 loop
               --  double each column for visibility
               IO.Put
                 (Output,
                  (if Map (Row, Col) then Outside'Image else Inside'Image));
               IO.Put
                 (Output,
                  (if Map (Row, Col) then Outside'Image else Inside'Image));

            end loop;
            IO.New_Line (Output);
         end loop;
      end loop;

      IO.Close (Output);

   end Write_Visualization;

   procedure Put_Doubled_Map (Doubled_Map : Doubled_Map_Type) is
   begin
      for Row in Doubled_Map'Range (1) loop
         for Col in Doubled_Map'Range (2) loop
            IO.Put ((if Doubled_Map (Row, Col) then '#' else ' '));
         end loop;
         IO.New_Line;
      end loop;
   end Put_Doubled_Map;

   function Part_2 return Natural is
      Doubled_Map : Doubled_Map_Type := Double_Map;
   begin
      --  Put_Doubled_Map (Doubled_Map);
      --  IO.New_Line;
      Flood_Fill (Doubled_Map);
      --  Put_Doubled_Map (Doubled_Map);
      --  IO.New_Line;
      Write_Visualization (Doubled_Map);
      return Size_Of_Interior (Doubled_Map);
   end Part_2;

begin
   Map_Package_IO.Read_Input;
   Find_Start;
   IO.Put_Line
     ("From entrance to farthest point takes" & Part_1'Image & " steps");
   --  Put_Traversed_Map;
   IO.Put_Line ("Nest area contains" & Part_2'Image & " spaces");
end Day10;
