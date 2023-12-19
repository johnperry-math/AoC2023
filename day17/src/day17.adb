pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 17: Clumsy Crucible
--
--  part 1: find the least heat loss possible
--
--  part 2: repeat, using an ultra crucible

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

procedure Day17 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;
   Visualize     : constant Boolean := True;

   Side_Length : constant Positive := (if Doing_Example then 13 else 141);
   subtype Side_Range is Positive range 1 .. Side_Length;
   Map : array (Side_Range, Side_Range) of Positive;

   --  SUBSECTION
   --  movement

   type Direction is (North, South, East, West);

   function Opposite (Left, Right : Direction) return Boolean is
     (case Left is when North => Right = South, when South => Right = North,
        when East => Right = West, when West => Right = East);

   subtype Nudge is Integer range -1 .. 1;

   type Drc is record
      DRow, DCol : Nudge;
   end record;

   Deltas : constant array (Direction) of Drc :=
     [North => (-1, 0), South => (1, 0), East => (0, 1), West => (0, -1)];

   --  SUBSECTION
   --  location and state

   type Location is record
      Row, Col : Side_Range;
   end record;

   function "<" (Left, Right : Location) return Boolean is
     (Left.Row < Right.Row
      or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   package Location_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Location);

   type State is record
      Where    : Location;
      Dir      : Direction;
      --  current direction of travel
      Repeated : Natural;
      --  number of steps in given direction the crucible has repeated
      Path     : Location_Vectors.Vector;
   end record;

   package State_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => State);

   package State_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => State_Interfaces);

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
      Dummy : Positive;
   begin

      IO.Open
        (Input, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));

      for Row in Side_Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Side_Range loop
               Map (Row, Col) :=
                 Character'Pos (Line (Col)) - Character'Pos ('0');
            end loop;
         end;
      end loop;

      IO.Close (Input);

   end Read_Input;

   procedure Put_Location (L : Location) is
   begin
      IO.Put ("(");
      IO.Put (L.Row'Image);
      IO.Put (",");
      IO.Put (L.Col'Image);
      IO.Put (" )");
   end Put_Location;

   procedure Put_Path (Path : Location_Vectors.Vector) is
   begin
      IO.Put_Line ("Traversed");
      for L of Path loop
         IO.Put ("   ");
         Put_Location (L);
         IO.Put (Map (L.Row, L.Col)'Image);
         IO.New_Line;
      end loop;
   end Put_Path;

   procedure Write_Visualization (P : Location_Vectors.Vector; Part : String)
   is
      --  who doesn't like a pretty picture?

      Output : IO.File_Type;

   begin
      IO.Create (Output, IO.Out_File, "visualization_" & Part & ".ppm");

      --  header
      IO.Put_Line (Output, "P3");
      IO.Put_Line (Output, Positive'Image (Map'Length (2) * 4));
      IO.Put_Line (Output, Positive'Image (Map'Length (1) * 4));
      IO.Put_Line (Output, "255"); -- max color
      IO.New_Line (Output);

      --  data
      for Row in Map'Range (1) loop
         for Each_Row in 1 .. 4 loop
            for Col in Map'Range (2) loop
               for Each_Col in 1 .. 4 loop
                  for Rgb in 1 .. 3 loop
                     if P.Contains (Location'(Row, Col)) and then Rgb < 3 then
                        IO.Put
                          (Output,
                           Natural'Image (255 - 10 * (10 - Map (Row, Col))));
                     else
                        IO.Put (Output, Natural'Image (Map (Row, Col) * 12));
                     end if;
                  end loop;
               end loop;
            end loop;
            IO.New_Line (Output);
         end loop;
         IO.New_Line (Output);
      end loop;
      IO.Close (Output);
   end Write_Visualization;

   --  SECTION
   --  Part 1

   function Part_1 return Natural is

      Result : Natural := 0;

      type Neighbor_Cost is array (Direction, 1 .. 3) of Natural;

      package Location_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Location, Element_Type => Neighbor_Cost);

      Visited : Location_Maps.Map;
      To_Do   : array (0 .. 5_000) of State_Queues.Queue;

   begin

      --  prime BFS
      To_Do (0).Enqueue
        (State'
           (Where => Location'(Side_Range'First, Side_Range'First),
            Dir   => East, Repeated => 1, Path => <>));

      loop

         --  if we've exhausted this heat, move forward in queue
         while Result <= To_Do'Last
           and then Natural (To_Do (Result).Current_Use) = 0
         loop
            Result := @ + 1;
            IO.Put_Line
              ("Heat loss:" & Result'Image & " has" &
               To_Do (Result).Current_Use'Image & " elements");
         end loop;

         declare
            Curr, Next   : State;
            New_Location : Location;
         begin

            To_Do (Result).Dequeue (Curr);
            if not Visited.Contains (Curr.Where) then
               Visited.Insert
                 (Curr.Where, [others => [others => Natural'Last]]);
            end if;

            if Visited (Curr.Where) (Curr.Dir, Curr.Repeated) > Result then

               Curr.Path.Append (Curr.Where);
               Visited (Curr.Where) (Curr.Dir, Curr.Repeated) := Result;

               for Dir in Direction loop

                  if Curr.Where.Row + Deltas (Dir).DRow in Side_Range
                    and then Curr.Where.Col + Deltas (Dir).DCol in Side_Range
                    and then (Curr.Dir /= Dir or else Curr.Repeated < 3)
                  then

                     New_Location :=
                       Location'
                         (Row => Curr.Where.Row + Deltas (Dir).DRow,
                          Col => Curr.Where.Col + Deltas (Dir).DCol);

                     --  check for termination
                     if New_Location.Row = Side_Range'Last
                       and then New_Location.Col = Side_Range'Last
                     then
                        --  Put_Path (Curr.Path);
                        if Visualize then
                           Write_Visualization (Curr.Path, "1");
                        end if;
                        return Result + Map (Side_Range'Last, Side_Range'Last);
                     end if;

                     --  not done; enqueue if not visited, then move on
                     if not Curr.Path.Contains (New_Location) then
                        Next.Where    := New_Location;
                        Next.Dir      := Dir;
                        Next.Repeated :=
                          (if Dir = Curr.Dir then Curr.Repeated + 1 else 1);
                        Next.Path     := Curr.Path;
                        To_Do
                          (Result + Map (New_Location.Row, New_Location.Col))
                          .Enqueue
                          (Next);
                     end if;

                  end if;

               end loop;
            end if;
         end;
      end loop;

   end Part_1;

   --  SECTION
   --  Part 2

   function Part_2 return Natural is

      Result       : Natural := Natural'Last;
      Current_Heat : Natural := 0;

      type Neighbor_Cost is array (Direction, 4 .. 10) of Natural;
      --  must travel at least 4, can travel at most 10

      package Location_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Location, Element_Type => Neighbor_Cost);

      Visited : Location_Maps.Map;
      To_Do   : array (0 .. 5_000) of State_Queues.Queue;

      Winning_Path : Location_Vectors.Vector := Location_Vectors.Empty_Vector;

   begin

      --  prime BFS...
      To_Do (0).Enqueue
        ((Location'(1, 1), Dir => East, Repeated => 10,
          Path                 => [1 => Location'(1, 1)]));
      To_Do (0).Enqueue
        ((Location'(1, 1), Dir => South, Repeated => 10,
          Path                 => [1 => Location'(1, 1)]));

      while Current_Heat < Result loop

         --  if we've exhausted this heat, move forward in queue
         while Current_Heat <= To_Do'Last
           and then Natural (To_Do (Current_Heat).Current_Use) = 0
         loop
            Current_Heat := @ + 1;
            IO.Put_Line
              ("Heat loss:" & Current_Heat'Image & " has" &
               To_Do (Current_Heat).Current_Use'Image & " elements");
         end loop;

         declare
            Curr, Next   : State;
            New_Location : Location;
            New_Heat     : Natural;
         begin

            To_Do (Current_Heat).Dequeue (Curr);
            if not Visited.Contains (Curr.Where) then
               Visited.Insert
                 (Curr.Where, [others => [others => Natural'Last]]);
            end if;

            if Visited (Curr.Where) (Curr.Dir, Curr.Repeated) > Current_Heat
            then

               Visited (Curr.Where) (Curr.Dir, Curr.Repeated) := Current_Heat;

               for Dir in Direction loop

                  if    --  move in same direction?
                  Dir = Curr.Dir
                    and then Curr.Where.Row + Deltas (Dir).DRow in Side_Range
                    and then Curr.Where.Col + Deltas (Dir).DCol in Side_Range
                    and then Curr.Repeated < 10
                  then

                     New_Location :=
                       Location'
                         (Row => Curr.Where.Row + Deltas (Dir).DRow,
                          Col => Curr.Where.Col + Deltas (Dir).DCol);

                     --  not done; enqueue if not visited, then move on
                     Next.Where    := New_Location;
                     Next.Dir      := Dir;
                     Next.Repeated := Curr.Repeated + 1;
                     Next.Path     := Curr.Path;
                     Next.Path.Append (New_Location);

                     --  check for termination
                     if Next.Where.Row = Side_Range'Last
                       and then Next.Where.Col = Side_Range'Last
                       and then Next.Repeated >= 4
                       and then
                         Current_Heat +
                           Map (Side_Range'Last, Side_Range'Last) <
                         Result
                     then
                        --  DON'T return yet!
                        Result       :=
                          Current_Heat +
                          Map (Side_Range'Last, Side_Range'Last);
                        Winning_Path := Next.Path;
                     end if;

                     To_Do
                       (Current_Heat +
                        Map (New_Location.Row, New_Location.Col))
                       .Enqueue
                       (Next);

                  elsif    --  move in different direction?
                  Dir /= Curr.Dir and then not Opposite (Dir, Curr.Dir)
                    and then Curr.Where.Row + 4 * Deltas (Dir).DRow in
                      Side_Range
                    and then Curr.Where.Col + 4 * Deltas (Dir).DCol in
                      Side_Range
                  then

                     New_Location :=
                       Location'
                         (Row => Curr.Where.Row + 4 * Deltas (Dir).DRow,
                          Col => Curr.Where.Col + 4 * Deltas (Dir).DCol);

                     --  not done; enqueue if not visited, then move on
                     Next.Where    := New_Location;
                     Next.Dir      := Dir;
                     Next.Repeated := 4;
                     Next.Path     := Curr.Path;
                     New_Heat      := Current_Heat;
                     for S in 1 .. 4 loop
                        Next.Path.Append
                          (Location'
                             (Curr.Where.Row + S * Deltas (Dir).DRow,
                              Curr.Where.Col + S * Deltas (Dir).DCol));
                        New_Heat :=
                          @ +
                          Map
                            (Curr.Where.Row + S * Deltas (Dir).DRow,
                             Curr.Where.Col + S * Deltas (Dir).DCol);
                     end loop;

                     --  check for termination
                     if Next.Where.Row = Side_Range'Last
                       and then Next.Where.Col = Side_Range'Last
                       and then New_Heat < Result
                        --  just repeated 4, so no need to check

                     then
                        --  DON'T return yet!
                        Result       := New_Heat;
                        Winning_Path := Next.Path;
                     end if;

                     --  IO.Put_Line
                     --    ("   New heat will be" & Natural'Image (New_Heat));
                     To_Do (New_Heat).Enqueue (Next);

                  end if;
                  --  end loop;

               end loop;
            end if;
         end;
      end loop;

      if Visualize then
         Write_Visualization (Winning_Path, "2");
      end if;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("Minimum heat loss to traverse the map is" & Part_1'Image);
   IO.Put_Line ("With ultra crucible," & Part_2'Image);
end Day17;
