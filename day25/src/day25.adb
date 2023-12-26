pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 25: Snowverload
--
--  part 1: which three wires must you disconnect to form two distinct circuits
--
--  part 2: the usual freebie
--
--  A LOT OF THINGS ARE COMMENTED OUT because they are part of my implemntation
--  of Karger's algorithm, which remains incomplete.
--  They are designated by "KA TBD" before the comments.

with Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

with Ada.Numerics.Discrete_Random;

use all type Ada.Containers.Count_Type;

procedure Day25 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  labels / nodes

   subtype Label_String is String (1 .. 3);

   function Min (One, Tother : Label_String) return Label_String is
     (if One <= Tother then One else Tother);

   function Max (One, Tother : Label_String) return Label_String is
     (if One >= Tother then One else Tother);

   package Label_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Label_String);

   package Label_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Label_String);

   All_Labels : Label_Sets.Set;

   --  KA TBD
   --  function "=" (Left, Right : Label_Vectors.Vector) return Boolean is
   --    (Left.Length = Right.Length
   --     and then
   --     (for all Ith in Left.First_Index .. Left.Last_Index =>
   --        Left (Ith) = Right (Ith)));

   --  KA TBD
   --  package Label_Maps is new Ada.Containers.Ordered_Maps
   --    (Key_Type => Label_String, Element_Type => Label_Vectors.Vector);

   --  KA TBD
   --  Connections : Label_Maps.Map;

   --  SUBSECTION
   --  edges

   type Edge_Record is record
      One, Tother : Label_String;
   end record;

   function "<" (Left, Right : Edge_Record) return Boolean is
     (Left.One < Right.One
      or else (Left.One = Right.One and then Left.Tother < Right.Tother));

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Edge_Record);

   function "=" (Left, Right : Edge_Vectors.Vector) return Boolean is
     (Left.Length = Right.Length
      and then
      (for all Ith in Left.First_Index .. Left.Last_Index =>
         Left (Ith) = Right (Ith)));

   --  SUBSECTION
   --  mapping labels to edges

   package Label_Edge_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Label_String, Element_Type => Edge_Vectors.Vector);

   Labels_To_Edges : Label_Edge_Maps.Map;

   --  KA TBD
   --  package Label_Remapping_Maps is new Ada.Containers.Ordered_Maps
   --    (Key_Type => Label_String, Element_Type => Label_String);

   --  KA TBD
   --  Label_Remapping : Label_Remapping_Maps.Map;

   --  SECTION
   --  I/O

   procedure Read_Input is
      --  also sets up All_Labels, Labels_To_Edges, and (KA TBD) Label_Remapping
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      while not IO.End_Of_File (Input) loop
         declare
            Line       : constant String := IO.Get_Line (Input);
            Pos        : Positive        := 6;
            Key, Value : Label_String;
            Edge       : Edge_Record;
            Edges      : Edge_Vectors.Vector;
         begin

            Key := Line (1 .. 3);
            if not All_Labels.Contains (Key) then
               All_Labels.Insert (Key);
               Labels_To_Edges.Insert (Key, Edges);
               --  KA TBD
               --  Label_Remapping.Insert (Key, Key);
            end if;

            while Pos <= Line'Last loop

               Value := Line (Pos .. Pos + 2);

               if not All_Labels.Contains (Value) then
                  All_Labels.Insert (Value);
                  Labels_To_Edges.Insert (Value, Edges);
                  --  KA TBD
                  --  Label_Remapping.Insert (Value, Value);
               end if;

               Edge := Edge_Record'(Min (Key, Value), Max (Key, Value));
               Labels_To_Edges (Key).Append (Edge);
               Labels_To_Edges (Value).Append (Edge);
               Pos := Pos + 4;
            end loop;

            --  KA TBD
            --  Connections.Insert (Key, List);

         end;
      end loop;

      IO.Close (Input);

   end Read_Input;

   procedure Put (Edge : Edge_Record) is
   --  useful for debugging
   begin
      IO.Put (Edge.One & "~" & Edge.Tother);
   end Put;

   --  SECTION
   --  Part 1

   package Natural_Random is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Natural);
   Random : Natural_Random.Generator;

   --  SUBSECTION
   --  Karger's algorithm (TBD)

   --  KA TBD
   --  procedure Remap_Vertices is
   --     Source      : Label_String;
   --     Destination : Label_String;
   --  begin
   --     IO.Put_Line ("Remapping");
   --     for Cursor in Label_Remapping.Iterate loop
   --        Source      := Label_Remapping_Maps.Key (Cursor);
   --        Destination := Label_Remapping_Maps.Element (Cursor);
   --        while Destination /= Label_Remapping (Destination) loop
   --           Destination := Label_Remapping (Destination);
   --        end loop;
   --        Label_Remapping.Replace (Source, Destination);
   --        if Source /= Destination then
   --           IO.Put_Line ("   " & Source & " -> " & Destination);
   --        end if;
   --     end loop;
   --  end Remap_Vertices;

   --  KA TBD
   --  procedure Contract_Random
   --    (Vertices : in out Label_Edge_Maps.Map; Edges : in out Edge_Sets.Set)
   --  is
   --     Edge_Number    : Natural          :=
   --       Natural_Random.Random (Random, 1, Natural (Edges.Length));
   --     Edge, New_Edge : Edge_Record;
   --     Cursor         : Edge_Sets.Cursor := Edges.First;
   --     Delete_Cursor  : Edge_Vectors.Cursor;
   --     One, Tother    : Label_String;
   --  begin
   --     for Each in 1 .. Edge_Number - 1 loop
   --        Cursor := Edges.Next (Cursor);
   --     end loop;
   --     Edge   := Edge_Sets.Element (Cursor);
   --     One    := Label_Remapping (Edge.One);
   --     Tother := Label_Remapping (Edge.Tother);
   --     IO.Put_Line
   --       ("Contracting " & One & "(" & Edge.One & ")~" & Tother & "(" &
   --        Edge.Tother & ")");
   --     for Cursor in Label_Remapping.Iterate loop
   --        if Label_Remapping_Maps.Key (Cursor) = Tother then
   --           Label_Remapping.Replace_Element (Cursor, One);
   --        end if;
   --     end loop;
   --     Edges.Delete (Cursor);
   --     Delete_Cursor := Vertices (One).Find (Edge);
   --     Vertices (One).Delete (Delete_Cursor);
   --     if One /= Tother then
   --        Delete_Cursor := Vertices (Tother).Find (Edge);
   --        Vertices (Tother).Delete (Delete_Cursor);
   --        for Each of Vertices (Tother) loop
   --           if not Vertices (One).Contains (Each) then
   --              Vertices (One).Append (Each);
   --           end if;
   --        end loop;
   --        Vertices.Delete (Tother);
   --     end if;
   --     Remap_Vertices;
   --  end Contract_Random;

   --  KA TBD
   --  procedure Part_1 is
   --     Work_Vertices : Label_Edge_Maps.Map :=
   --       Label_Edge_Maps.Copy (Labels_To_Edges);
   --     Work_Edges    : Edge_Sets.Set       := Edge_Sets.Copy (Edge_Set);
   --  begin
   --     while Work_Vertices.Length > 2 loop
   --        IO.Put_Line ("There are" & Work_Vertices.Length'Image & " vertices:");
   --        for Cursor in Work_Vertices.Iterate loop
   --           IO.Put (Label_Edge_Maps.Key (Cursor) & ": ");
   --           for Each of Label_Edge_Maps.Element (Cursor) loop
   --              IO.Put (Each.One & "~" & Each.Tother & " ");
   --           end loop;
   --           IO.New_Line;
   --        end loop;
   --        IO.New_Line;
   --        IO.Put_Line ("There are" & Work_Edges.Length'Image & " edges:");
   --        for Edge of Work_Edges loop
   --           IO.Put (Edge.One & "~" & Edge.Tother & " ");
   --        end loop;
   --        IO.New_Line;
   --        IO.Put_Line ("-----");
   --        IO.New_Line;
   --        Contract_Random (Work_Vertices, Work_Edges);
   --     end loop;
   --     if Work_Edges.Length = 3 then
   --        IO.Put_Line ("Succeeded!");
   --        for Edge of Work_Edges loop
   --           IO.Put_Line (Edge'Image);
   --        end loop;
   --     else
   --        IO.Put_Line ("Failed! Still have" & Work_Edges.Length'Image);
   --     end if;
   --  end Part_1;

   --  SUBSECTION
   --  Monte Carlo algorithm

   package Edge_To_Natural is new Ada.Containers.Ordered_Maps
     (Key_Type => Edge_Record, Element_Type => Natural);

   package Label_To_Natural is new Ada.Containers.Ordered_Maps
     (Key_Type => Label_String, Element_Type => Natural);

   package Path_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Label_Vectors.Vector);
   package Path_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Path_Interfaces);

   function Find_Path (Source, Dest : Label_String) return Edge_Vectors.Vector
   is
      --  BFS our way from Source to Dest

      Queue              : Path_Queues.Queue;      --  paths to explore
      Shortest_Distances : Label_To_Natural.Map;   --  prunes bad branches

      Path, New_Path : Label_Vectors.Vector;   --  dequeued/enqueued, resp.
      Route          : Label_String;           --  next step in path

      Result : Edge_Vectors.Vector;    --  a shortest path

   begin

      --  prime the BFS
      Path.Append (Source);
      Shortest_Distances.Insert (Source, 0);
      Queue.Enqueue (Path);

      while Natural (Queue.Current_Use) > 0 loop

         Queue.Dequeue (Path);

         if Path.Last_Element = Dest then
            --  hey, we're done!
            for Ith in Path.First_Index .. Path.Last_Index - 1 loop
               declare
                  One    : constant Label_String :=
                    Min (Path (Ith), Path (Ith + 1));
                  Tother : constant Label_String :=
                    Max (Path (Ith), Path (Ith + 1));
               begin
                  Result.Append (Edge_Record'(One, Tother));
               end;
            end loop;
            return Result;
         end if;

         for Edge of Labels_To_Edges (Path.Last_Element) loop
            --  queue up possible routes

            Route :=
              (if Edge.One = Path.Last_Element then Edge.Tother else Edge.One);

            if not Path.Contains (Route)
              and then
              (not Shortest_Distances.Contains (Route)
               or else Shortest_Distances (Route) > Natural (Path.Length))
            then

               if not Shortest_Distances.Contains (Route) then
                  Shortest_Distances.Insert (Route, Natural (Path.Length));
               else
                  Shortest_Distances.Replace (Route, Natural (Path.Length));
               end if;

               New_Path := Label_Vectors.Copy (Path);
               New_Path.Append (Route);
               Queue.Enqueue (New_Path);

            end if;
         end loop;
      end loop;

      return Result;

   end Find_Path;

   --  SUBSECTION
   --  avoiding path repetition

   type Endpoints is record
      Start, Finish : Label_String;
   end record;
   --  while this is equivalent to an edge, and I was tempted to reuse it,
   --  a path is not an edge, so I went The Ada Way
   --  and defined a different type

   function "<" (Left, Right : Endpoints) return Boolean is
     (Left.Start < Right.Start
      or else (Left.Start = Right.Start and then Left.Finish < Right.Finish));

   package Path_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Endpoints);

   --  SUBSECTION
   --  the meat o'the matter

   procedure Make_The_Cut is

      Ith, Jth     : Natural;             --  numbers of the labels/nodes
      Cursor       : Label_Sets.Cursor;   --  used to find Ith, Jth in set
      Source, Dest : Label_String;        --  corresponding labels/nodes

      Searched_Paths  : Path_Sets.Set;       --  paths we've searched
      Edge_Importance : Edge_To_Natural.Map; -- # of times edge appears in path

      Removed_Edge : Edge_Record;
      Edge_Weight  : Natural := 0;

   begin

      --  find the three edges that make the minimum
      for Kth in 1 .. 3 loop

         --  clear the search information
         Edge_Weight := 0;
         Searched_Paths.Clear;
         Edge_Importance.Clear;
         for Edge_List of Labels_To_Edges loop
            for Edge of Edge_List loop
               if not Edge_Importance.Contains (Edge) then
                  Edge_Importance.Insert (Edge, 0);
               end if;
            end loop;
         end loop;

         --  find distinct paths
         for Each in 1 .. 100 loop

            loop

               Ith :=
                 Natural_Random.Random
                   (Random, 1, Natural (All_Labels.Length));
               Jth := Ith;
               while Ith = Jth loop
                  Jth :=
                    Natural_Random.Random
                      (Random, 1, Natural (All_Labels.Length));
               end loop;

               Cursor := All_Labels.First;
               for Each in 1 .. Ith - 1 loop
                  Cursor := Label_Sets.Next (Cursor);
               end loop;
               Source := Label_Sets.Element (Cursor);

               Cursor := All_Labels.First;
               for Each in 1 .. Jth - 1 loop
                  Cursor := Label_Sets.Next (Cursor);
               end loop;
               Dest := Label_Sets.Element (Cursor);

               exit when not Searched_Paths.Contains ((Source, Dest));

            end loop;

            Searched_Paths.Insert ((Source, Dest));
            for Edge of Find_Path (Source, Dest) loop
               Edge_Importance.Replace (Edge, Edge_Importance (Edge) + 1);
            end loop;

         end loop;

         --  identify highest-used edge: w/high probability it makes the cut
         for Cursor in Edge_Importance.Iterate loop
            if Edge_To_Natural.Element (Cursor) > Edge_Weight then
               Removed_Edge := Edge_To_Natural.Key (Cursor);
               Edge_Weight  := Edge_To_Natural.Element (Cursor);
            end if;
         end loop;

         IO.Put ("Removing ");
         Put (Removed_Edge);
         IO.Put_Line (" with weight" & Edge_Weight'Image);
         Edge_Importance.Delete (Removed_Edge);
         for Each of Labels_To_Edges loop
            if Each.Contains (Removed_Edge) then
               Each.Delete (Each.Find_Index (Removed_Edge));
            end if;
         end loop;

      end loop;

   end Make_The_Cut;

   function Part_1 return Natural is

      Source               : Label_String;
      Group_One, Group_Two : Label_Sets.Set;

   begin

      Make_The_Cut;

      --  sort nodes into groups: one connected to first, tother disconnected
      Source := All_Labels.First_Element;
      Group_One.Insert (Source);
      for Dest of All_Labels when Dest /= Source loop
         if Natural (Find_Path (Source, Dest).Length) > 0 then
            Group_One.Insert (Dest);
         else
            Group_Two.Insert (Dest);
         end if;
      end loop;

      IO.Put_Line
        ("Lengths:" & Group_One.Length'Image & Group_Two.Length'Image);

      return Natural (Group_One.Length * Group_Two.Length);

   end Part_1;

begin
   Natural_Random.Reset (Random);
   Read_Input;
   IO.Put_Line ("product is" & Part_1'Image);
end Day25;
