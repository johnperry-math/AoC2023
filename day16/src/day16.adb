pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 16: The Floor Will Be Lava
--
--  part 1: count the number of tiles energized by a beam of light
--
--  part 2: find the maximum number of tiles that a beam might energize

with Ada.Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day16 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   Visualize : constant Boolean := True;

   type Object is (Empty, Vertical, Horizontal, Foremirror, Backmirror);

   --  SUBSECTION
   --  direction and change of motion

   type Direction is (North, South, East, West);
   subtype Difference is Integer range -1 .. 1;
   type Diff_2Dim is record
      DRow, DCol : Difference;
   end record;

   Deltas : constant array (Direction) of Diff_2Dim :=
     [North => (-1, 0), South => (1, 0), East => (0, 1), West => (0, -1)];

   --  SUBSECTION
   --  the facility and energizing

   Side_Length : constant Positive := 110;
   subtype Side_Range is Positive range 1 .. Side_Length;

   type Facility_Type is array (Side_Range, Side_Range) of Object;
   Facility : Facility_Type;

   type Energized_Direction is array (Direction) of Boolean;
   type Energized_Type is
     array (Side_Range, Side_Range) of Energized_Direction;
   Energized : Energized_Type := [others => [others => [others => False]]];

   --  SUBSECTION
   --  photons (what the puzzle calls "beams") and the tracking thereof

   type Photon is record
      Row, Col  : Side_Range;
      Dir       : Direction;     --  direction of travel
      Iteration : Natural;       --  purely for visualization
   end record;

   package Photon_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Photon);
   package Photon_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Photon_Interface);
   Photons : Photon_Queues.Queue;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
      Invalid_Input : exception;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      for Row in Side_Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin

            for Col in Side_Range loop
               Facility (Row, Col) :=
                 (case Line (Col) is when '.' => Empty, when '|' => Vertical,
                    when '-' => Horizontal, when '/' => Foremirror,
                    when '\' => Backmirror,
                    when others => raise Invalid_Input with Line (Col .. Col));
            end loop;

         end;
      end loop;

      IO.Close (Input);

   end Read_Input;

   procedure Put_Photon (P : Photon) is
   --  useful for debugging
   begin
      IO.Put ("Photon traveling ");
      IO.Put
        ((case P.Dir is when North => "north", when South => "south",
            when East => "east", when West => "west"));
      IO.Put (" at" & P.Row'Image & "," & P.Col'Image);
   end Put_Photon;

   procedure Write_Visualization (P : Photon; Frame : Natural) is
      Output : IO.File_Type;

      type Color_Range is new Natural range 0 .. 255;
      type Color is record
         Red, Green, Blue : Color_Range;
      end record;

      Bg_Color : Color;

      Empty_Color : constant Color := (Red => 192, Green => 192, Blue => 192);
      Energized_Color : constant Color :=
        (Red => 224, Green => 224, Blue => 128);
      Object_Color    : constant Color := (Red => 64, Green => 64, Blue => 64);

      Row_Suffix, Col_Suffix : String (1 .. 3) := [others => '0'];

      Naive_Row_Suffix : constant String := P.Row'Image;
      Naive_Col_Suffix : constant String := P.Col'Image;
      Dir_Suffix       : constant String := Direction'Image (P.Dir);

      Frame_Suffix       : String (1 .. 4) := [others => '0'];
      Naive_Frame_Suffix : String          := Frame'Image;

   begin

      Row_Suffix (3) := Naive_Row_Suffix (Naive_Row_Suffix'Last);
      if P.Row >= 10 then
         Row_Suffix (2) := Naive_Row_Suffix (Naive_Row_Suffix'Last - 1);
      end if;
      if P.Row >= 100 then
         Row_Suffix (1) := Naive_Row_Suffix (2);
      end if;
      Col_Suffix (3) := Naive_Col_Suffix (Naive_Col_Suffix'Last);
      if P.Col >= 10 then
         Col_Suffix (2) := Naive_Col_Suffix (Naive_Col_Suffix'Last - 1);
      end if;
      if P.Col >= 100 then
         Col_Suffix (1) := Naive_Col_Suffix (2);
      end if;

      Frame_Suffix (4) := Naive_Frame_Suffix (Naive_Frame_Suffix'Last);
      if Frame >= 10 then
         Frame_Suffix (3) := Naive_Frame_Suffix (Naive_Frame_Suffix'Last - 1);
      end if;
      if Frame >= 100 then
         Frame_Suffix (2) := Naive_Frame_Suffix (Naive_Frame_Suffix'Last - 2);
      end if;
      if Frame >= 1_000 then
         Frame_Suffix (1) := Naive_Frame_Suffix (2);
      end if;

      IO.Create
        (Output,
         Name =>
           "frames/Photon_" & Row_Suffix & "_" & Col_Suffix & "_" &
           Dir_Suffix & "_" & Frame_Suffix & ".ppm");
      IO.Put (Output, "P3");
      IO.Put (Output, Positive'Image (Facility'Length (2) * 3));
      IO.Put (Output, Positive'Image (Facility'Length (1) * 3));
      IO.Put (Output, Color_Range'Image (Color_Range'Last)); -- max color
      IO.New_Line (Output);

      for Row in Facility'Range (1) loop
         for Each_Row in 1 .. 3 loop
            for Col in Facility'Range (2) loop
               Bg_Color :=
                 (if (for some Dir in Direction => Energized (Row, Col) (Dir))
                  then Energized_Color
                  else Empty_Color);
               case Facility (Row, Col) is
                  when Empty =>
                     for Each_Col in 1 .. 3 loop
                        IO.Put (Output, Bg_Color.Red'Image);
                        IO.Put (Output, Bg_Color.Green'Image);
                        IO.Put (Output, Bg_Color.Blue'Image);
                        IO.New_Line (Output);
                     end loop;
                  when Vertical =>
                     IO.Put (Output, Bg_Color.Red'Image);
                     IO.Put (Output, Bg_Color.Green'Image);
                     IO.Put (Output, Bg_Color.Blue'Image);
                     IO.New_Line (Output);
                     IO.Put (Output, Object_Color.Red'Image);
                     IO.Put (Output, Object_Color.Green'Image);
                     IO.Put (Output, Object_Color.Blue'Image);
                     IO.New_Line (Output);
                     IO.Put (Output, Bg_Color.Red'Image);
                     IO.Put (Output, Bg_Color.Green'Image);
                     IO.Put (Output, Bg_Color.Blue'Image);
                     IO.New_Line (Output);
                  when Horizontal =>
                     for Each_Col in 1 .. 3 loop
                        if Each_Row = 1 or else Each_Row = 3 then
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                        else
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                        end if;
                     end loop;
                  when Foremirror =>
                     case Each_Row is
                        when 1 =>
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                        when 2 =>
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                        when 3 =>
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                     end case;
                  when Backmirror =>
                     case Each_Row is
                        when 1 =>
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                        when 2 =>
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                        when 3 =>
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Bg_Color.Red'Image);
                           IO.Put (Output, Bg_Color.Green'Image);
                           IO.Put (Output, Bg_Color.Blue'Image);
                           IO.New_Line (Output);
                           IO.Put (Output, Object_Color.Red'Image);
                           IO.Put (Output, Object_Color.Green'Image);
                           IO.Put (Output, Object_Color.Blue'Image);
                           IO.New_Line (Output);
                     end case;
               end case;
            end loop;
         end loop;
      end loop;

      IO.Close (Output);

   end Write_Visualization;

   --  SECTION
   --  Part 1 : counting energized tiles

   function Moved (P : in out Photon) return Boolean is
      --  attempts to move P according to its direction and position;
      --  returns True iff successful; i.e., P is still moving

      Dxy          : constant Diff_2Dim := Deltas (P.Dir);
      Still_Moving : Boolean            := False;

   begin

      if P.Row + Dxy.DRow in Side_Range and then P.Col + Dxy.DCol in Side_Range
      then
         Still_Moving := True;
         P.Row        := @ + Dxy.DRow;
         P.Col        := @ + Dxy.DCol;
         P.Iteration  := @ + 1;
      end if;

      return Still_Moving and then not Energized (P.Row, P.Col) (P.Dir);

   end Moved;

   function Moved_Foremirror (P : in out Photon) return Boolean is
   --  use this when P's position is on a Foremirror /
   --  returns True iff still moving
   begin

      case P.Dir is
         when North =>
            P.Dir := East;
         when South =>
            P.Dir := West;
         when East =>
            P.Dir := North;
         when West =>
            P.Dir := South;
      end case;

      return Moved (P);

   end Moved_Foremirror;

   function Moved_Backmirror (P : in out Photon) return Boolean is
   --  use this when P's position is on a Backmirror \
   --  returns True iff still moving
   begin

      case P.Dir is
         when North =>
            P.Dir := West;
         when South =>
            P.Dir := East;
         when East =>
            P.Dir := South;
         when West =>
            P.Dir := North;
      end case;

      return Moved (P);

   end Moved_Backmirror;

   procedure Split_Horizontal (P : Photon; Q : in out Photon_Queues.Queue) is
   --  use this when P's position is on a horizontal splitter -
   --  automatically enqueues the two, one, or zero photons
   begin
      if P.Col - 1 in Side_Range
        and then not Energized (P.Row, P.Col - 1) (West)
      then
         Q.Enqueue ((P with delta Col => P.Col - 1, Dir => West));
      end if;
      if P.Col + 1 in Side_Range
        and then not Energized (P.Row, P.Col + 1) (East)
      then
         Q.Enqueue ((P with delta Col => P.Col + 1, Dir => East));
      end if;
   end Split_Horizontal;

   procedure Split_Vertical (P : Photon; Q : in out Photon_Queues.Queue) is
   --  use this when P's position is on a vertical splitter |
   --  automatically enqueues the two, one, or zero photons
   begin
      if P.Row - 1 in Side_Range
        and then not Energized (P.Row - 1, P.Col) (North)
      then
         Q.Enqueue ((P with delta Row => P.Row - 1, Dir => North));
      end if;
      if P.Row + 1 in Side_Range
        and then not Energized (P.Row + 1, P.Col) (South)
      then
         Q.Enqueue ((P with delta Row => P.Row + 1, Dir => South));
      end if;
   end Split_Vertical;

   function Part_1
     (Initial   : Photon  :=
        (Row       => Side_Range'First, Col => Side_Range'First, Dir => East,
         Iteration => 0);
      Visualize : Boolean := False) return Natural
   is

      P                 : Photon;
      Result, Max_Beams : Natural := 0;
      Current_Iteration : Natural := 0;

   begin

      --  prime BFS
      Photons.Enqueue (Initial);
      Energized := [others => [others => [others => False]]];

      while Natural (Photons.Current_Use) > 0 loop

         Max_Beams := Natural'Max (Max_Beams, Natural (Photons.Current_Use));

         Photons.Dequeue (P);
         if Visualize and then P.Iteration >= Current_Iteration then
            Write_Visualization (Initial, Current_Iteration);
            Current_Iteration := P.Iteration;
         end if;

         if not Energized (P.Row, P.Col) (P.Dir) then

            Energized (P.Row, P.Col) (P.Dir) := True;

            case Facility (P.Row, P.Col) is
               when Empty =>
                  if Moved (P) then
                     Photons.Enqueue (P);
                  end if;

               when Foremirror =>
                  if Moved_Foremirror (P) then
                     Photons.Enqueue (P);
                  end if;

               when Backmirror =>
                  if Moved_Backmirror (P) then
                     Photons.Enqueue (P);
                  end if;

               when Horizontal =>
                  case P.Dir is
                     when East | West =>
                        if Moved (P) then
                           Photons.Enqueue (P);
                        end if;
                     when North | South =>
                        Split_Horizontal (P, Photons);
                  end case;

               when Vertical =>
                  case P.Dir is
                     when North | South =>
                        if Moved (P) then
                           Photons.Enqueue (P);
                        end if;
                     when East | West =>
                        Split_Vertical (P, Photons);
                  end case;

            end case;
         end if;
      end loop;

      for Row in Side_Range loop
         for Col in Side_Range loop
            if (for some Dir in Direction => Energized (Row, Col) (Dir)) then
               Result := @ + 1;
            end if;
         end loop;
      end loop;

      Put_Photon (Initial);
      IO.Put_Line (" generated" & Max_Beams'Image & " beams");

      return Result;

   end Part_1;

   function Part_2 return Natural is
      Result : Natural := 0;
   begin

      --  try each possible beam down the left & right
      for Row in Side_Range loop
         Result :=
           Natural'Max
             (Result,
              Part_1
                (Photon'
                   (Row       => Row, Col => Side_Range'First, Dir => East,
                    Iteration => 0)));
         Result :=
           Natural'Max
             (Result,
              Part_1
                (Photon'
                   (Row       => Row, Col => Side_Range'Last, Dir => West,
                    Iteration => 0)));
      end loop;

      --  try each possible beam across the top & bottom
      for Col in Side_Range loop
         Result :=
           Natural'Max
             (Result,
              Part_1
                (Photon'
                   (Row       => Side_Range'First, Col => Col, Dir => South,
                    Iteration => 0)));
         Result :=
           Natural'Max
             (Result,
              Part_1
                (Photon'
                   (Row       => Side_Range'Last, Col => Col, Dir => North,
                    Iteration => 0)));
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line
     ("The facility energizes" & Part_1 (Visualize => Visualize)'Image &
      " tiles");
   IO.Put_Line ("Best configuration energizes" & Part_2'Image & " tiles");
end Day16;
