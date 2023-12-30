pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 23: A long walk
--
--  part 1: find the _longest_ path through a maze,
--          when you can't walk against slopes
--
--  part 2: repeat, except now you can walk up slopes

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

use all type Ada.Containers.Count_Type;

with Common;

procedure Day23 is

   package IO renames Ada.Text_IO;

   use Common.Two_Dimensional_Motion;

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;

   --  SUBSECTION
   --  the map itself

   Side_Length : constant Positive := (if Doing_Example then 23 else 141);

   type Object is
     (Forest, Path, Slope_East, Slope_West, Slope_North, Slope_South);

   function Opposite (Dir : Direction) return Object is
     (case Dir is when North => Slope_South, when South => Slope_North,
        when East => Slope_West, when West => Slope_East);

   package Map_Package is new Common.Two_Dimensional_Map
     (Row_Length => Side_Length, Col_Length => Side_Length, Object => Object);

   use Map_Package;

   subtype Side_Range is Row_Range;

   Invalid_Symbol : exception;

   function Deserialize (Symbol : Character) return Object is
     (case Symbol is when '.' => Path, when '#' => Forest,
        when '>' => Slope_East, when '^' => Slope_North,
        when 'v' => Slope_South, when '<' => Slope_West,
        when others => raise Invalid_Symbol with Symbol'Image);

   function Serialize (O : Object) return Character is
     (case O is when Forest => '#', when Path => '.', when Slope_East => '>',
        when Slope_West => '<', when Slope_North => '^',
        when Slope_South => 'v');

   package Map_Package_IO is new Common.Two_Dimensional_Map_IO
     (Doing_Example => Doing_Example, Map_Package => Map_Package);

   --  SUBSECTION
   --  motion through the map

   Goin : constant Location_Record := Location_Record'(Row => 1, Col => 2);

   Goal : constant Location_Record :=
     Location_Record'(Row => Side_Length, Col => Side_Length - 1);

   --  SUBSECTION
   --  tracking motion

   package Path_Vectors renames Map_Package.Location_Vectors;

   package Path_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Path_Vectors.Vector);
   package Path_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Path_Interfaces);

   --  SECTION
   --  I/O

   pragma Warnings (Off, "is not referenced");
   procedure Put_Location (L : Location_Record) is
      package Nat_IO is new IO.Integer_IO (Num => Natural);
   begin

      IO.Put ("(");
      Nat_IO.Put (L.Row, 0);
      IO.Put (",");
      Nat_IO.Put (L.Col, 0);
      IO.Put (")");

   end Put_Location;
   pragma Warnings (On, "is not referenced");

   --  SECTION
   --  Part 1

   function Part_1 return Ada.Containers.Count_Type is
      Queue : Path_Queues.Queue;

      Curr, Next, Best : Path_Vectors.Vector;

      Invalid_Step : exception;
   begin

      Curr.Append (Goin);
      Queue.Enqueue (Curr);

      while Natural (Queue.Current_Use) > 0 loop

         Queue.Dequeue (Curr);

         if Curr.Last_Element = Goal and then Curr.Length > Best.Length then

            Best := Curr;

         else

            declare
               Pos renames Curr.Last_Element;
               Step : Location_Record;
               D    : Drc;
            begin

               for Dir in Direction loop

                  D := Deltas (Dir);

                  if Pos.Row + D.DRow in Side_Range
                    and then Pos.Col + D.DCol in Side_Range
                    and then Map (Pos.Row + D.DRow, Pos.Col + D.DCol) /= Forest
                  then

                     Step :=
                       Location_Record'(Pos.Row + D.DRow, Pos.Col + D.DCol);

                     if not Curr.Contains (Step) then

                        if Map (Step.Row, Step.Col) = Path then

                           Next := Path_Vectors.Copy (Curr);
                           Next.Append (Step);
                           Queue.Enqueue (Next);

                        elsif Map (Step.Row, Step.Col) /= Opposite (Dir) then

                           Next := Path_Vectors.Copy (Curr);
                           Next.Append (Step);

                           case Map (Step.Row, Step.Col) is

                              when Slope_East =>
                                 Next.Append
                                   (Location_Record'(Step.Row, Step.Col + 1));

                              when Slope_North =>
                                 Next.Append
                                   (Location_Record'(Step.Row - 1, Step.Col));

                              when Slope_South =>
                                 Next.Append
                                   (Location_Record'(Step.Row + 1, Step.Col));

                              when Slope_West =>
                                 Next.Append
                                   (Location_Record'(Step.Row, Step.Col - 1));

                              when others =>
                                 raise Invalid_Step
                                   with Step.Row'Image & Step.Col'Image &
                                   D'Image;

                           end case;

                           Queue.Enqueue (Next);

                        end if;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      return Best.Length - 1;

   end Part_1;

   --  SECTION
   --  Part 2

   --  SUBSECTION
   --  graph structures

   type Fork_Path is record
      Start, Stop : Location_Record;
   end record;

   function "<" (Left, Right : Fork_Path) return Boolean is
     (Left.Start.Row < Right.Start.Row
      or else
      (Left.Start.Row = Right.Start.Row
       and then Left.Start.Col < Right.Start.Col)
      or else
      (Left.Start = Right.Start and then Left.Stop.Row < Right.Stop.Row)
      or else
      (Left.Start = Right.Start and then Left.Stop.Row = Right.Stop.Row
       and then Left.Stop.Col < Right.Stop.Col));

   All_Forks : Location_Vectors.Vector;
   --  list of all Forks

   package Fork_Path_Length_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Fork_Path, Element_Type => Natural);
   Fork_Path_Lengths : Fork_Path_Length_Maps.Map;
   --  maps Forks to the distances it takes to go from one to tother

   procedure Find_Forks is
      --  locates forks in the map
      Neighbors : Natural;
   begin

      All_Forks.Append (Goin);
      All_Forks.Append (Goal);

      for Row in Side_Range loop
         for Col in Side_Range loop

            if Map (Row, Col) /= Forest then

               Neighbors := 0;

               for D of Deltas loop
                  if Row + D.DRow in Side_Range
                    and then Col + D.DCol in Side_Range
                    and then Map (Row + D.DRow, Col + D.DCol) /= Forest
                  then
                     Neighbors := @ + 1;
                  end if;
               end loop;

               if Neighbors > 2 then
                  All_Forks.Append (Location_Record'(Row, Col));
               end if;

            end if;

         end loop;
      end loop;

      --  IO.Put_Line ("Forks:");
      --  for Each of All_Forks loop
      --     IO.Put ("   ");
      --     Put_Location (Each);
      --     IO.New_Line;
      --  end loop;

   end Find_Forks;

   procedure Map_Forks is
      --  finds the distances from each fork to the next
      Prev, Curr, Next : Location_Record;

      Steps : Natural;
   begin

      for Ith in All_Forks.First_Index .. All_Forks.Last_Index loop
         for D of Deltas loop

            Curr := All_Forks (Ith);

            if Curr.Row + D.DRow in Side_Range
              and then Curr.Col + D.DCol in Side_Range
              and then Map (Curr.Row + D.DRow, Curr.Col + D.DCol) /= Forest
            then

               Steps := 1;
               Prev  := Curr;
               Curr  := Location_Record'(Curr.Row + D.DRow, Curr.Col + D.DCol);

               while not All_Forks.Contains (Curr) loop
                  for D of Deltas loop

                     if Curr.Row + D.DRow in Side_Range
                       and then Curr.Col + D.DCol in Side_Range
                       and then Map (Curr.Row + D.DRow, Curr.Col + D.DCol) /=
                         Forest
                     then
                        Next :=
                          Location_Record'
                            (Curr.Row + D.DRow, Curr.Col + D.DCol);
                        if Next /= Prev then
                           Prev  := Curr;
                           Curr  := Next;
                           Steps := @ + 1;
                           exit;
                        end if;
                     end if;

                  end loop;
               end loop;

               --  IO.Put ("Inserting ");
               --  Put_Location (All_Forks (Ith));
               --  IO.Put (" -> ");
               --  Put_Location (Next);
               --  IO.Put_Line (":" & Steps'Image);

               Fork_Path_Lengths.Insert
                 (Fork_Path'(All_Forks (Ith), Next), Steps);

            end if;

         end loop;
      end loop;
   end Map_Forks;

   function Path_Length (Path : Path_Vectors.Vector) return Natural is
      --  total path for a vector
      Result : Natural := 0;
   begin

      for Ith in Path.First_Index .. Path.Last_Index - 1 loop
         Result :=
           @ + Fork_Path_Lengths (Fork_Path'(Path (Ith), Path (Ith + 1)));
      end loop;

      return Result;

   end Path_Length;

   function Part_2 return Natural is
      --  BFS is apparently a bad idea... but that's the route I took
      Queue : Path_Queues.Queue;

      Curr, Next, Best : Path_Vectors.Vector;

      --  Iteration   : Natural                   := 0;
      Last_Length : Ada.Containers.Count_Type := 0;
   begin

      Curr.Append (Location_Record'(Row => 1, Col => 2));
      Queue.Enqueue (Curr);

      while Natural (Queue.Current_Use) > 0 loop

         --  Iteration := @ + 1;
         --  if Iteration mod 10 = 0 then
         --     IO.Put_Line
         --       ("On iteration" & Iteration'Image & " we have" &
         --        Queue.Current_Use'Image & " paths to consider");
         --     IO.Put_Line
         --       ("Current best path has" & Best.Length'Image &
         --        " steps (less one!)");
         --  end if;

         Queue.Dequeue (Curr);

         if Curr.Length > Last_Length then
            IO.Put_Line ("path length is now" & Curr.Length'Image);
            Last_Length := Curr.Length;
            IO.Put_Line ("paths in queue:" & Queue.Current_Use'Image);
            --  IO.Put ("   Dequeued ");
            --  for L of Curr loop
            --     IO.Put ("(" & L.Row'Image & "," & L.Col'Image & ")  ");
            --  end loop;
            --  IO.New_Line;
         end if;

         if Curr.Last_Element = Goal
           and then Path_Length (Curr) > Path_Length (Best)
         then
            Best := Curr;

         else
            for Fork of All_Forks loop
               if not Curr.Contains (Fork)
                 and then
                 (Fork_Path_Lengths.Contains
                    (Fork_Path'(Curr.Last_Element, Fork))
                  or else Fork_Path_Lengths.Contains
                    (Fork_Path'(Fork, Curr.Last_Element)))
               then
                  Next := Path_Vectors.Copy (Curr);
                  Next.Append (Fork);
                  Queue.Enqueue (Next);
               end if;
            end loop;
         end if;

      end loop;

      return Path_Length (Best);

   end Part_2;

   --  function "<" (Left, Right : Location_Record) return Boolean is
   --    (Left.Row < Right.Row
   --     or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   --  package Location_Sets is new Ada.Containers.Ordered_Sets
   --    (Element_Type => Location_Record);

   --  type Discovered_Array is array (Positive range <>) of Boolean with
   --    Pack;

   --  function Explore_From
   --    (Ith : Positive; Discovered : in out Discovered_Array) return Natural
   --  is
   --     Result : Natural := 0;
   --     Here   : Location_Record renames All_Forks (Ith);
   --  begin
   --     if Here /= Goal then
   --        Discovered (Ith) := True;
   --        for Jth in
   --          All_Forks.First_Index ..
   --            All_Forks.Last_Index when not Discovered (Jth)
   --        loop
   --           if Fork_Path_Lengths.Contains
   --               ((Start => Here, Stop => All_Forks (Jth)))
   --           then
   --              Result :=
   --                Natural'Max
   --                  (Result,
   --                   Fork_Path_Lengths ((Here, All_Forks (Jth))) +
   --                   Explore_From (Jth, Discovered));
   --           end if;
   --        end loop;
   --        Discovered (Ith) := False;
   --     end if;
   --     return Result;
   --  end Explore_From;

   --  function Part_2_DFS return Natural is
   --     Discovered :
   --       Discovered_Array (All_Forks.First_Index .. All_Forks.Last_Index);
   --  begin
   --     Put_Location (All_Forks (1));
   --     IO.New_Line;
   --     return Explore_From (1, Discovered);
   --  end Part_2_DFS;

begin
   Map_Package_IO.Read_Input;
   IO.Put_Line ("the most scenic route's length is" & Part_1'Image & " steps");
   Find_Forks;
   Map_Forks;
   --  IO.Put_Line
   --    ("well, if i can climb slopes, then it's" & Part_2_DFS'Image & " steps");
   IO.Put_Line
     ("well, if i can climb slopes, then it's" & Part_2'Image & " steps");
end Day23;
