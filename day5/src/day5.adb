pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 5: If You Give A Seed A Fertilizer
--
--  part 1: help an elf figure out which of 20 seeds to plant
--          by following an almanac's mapping of seed to soil,
--          soil to fertilizer, etc.
--
--  part 2: whoops! it's not 20 seeds, but 10 seeds and 10 intervals;
--          do it again with this understanding

with Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;

procedure Day5 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Num => Long_Long_Integer);

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  constants depending on version: example or puzzle
   --
   --  i needed the example this time,
   --  so each constant has two possibilities

   Doing_Example : constant Boolean := False;

   Number_Of_Seeds : constant Natural := (if Doing_Example then 4 else 20);

   --  map sizes
   Number_Of_Seed_To_Soil : constant Natural :=
     (if Doing_Example then 2 else 42);
   Number_Of_Soil_To_Fert : constant Natural :=
     (if Doing_Example then 3 else 49);
   Number_Of_Fert_To_Watr : constant Natural :=
     (if Doing_Example then 4 else 32);
   Number_Of_Watr_To_Lite : constant Natural :=
     (if Doing_Example then 2 else 47);
   Number_Of_Lite_To_Temp : constant Natural :=
     (if Doing_Example then 3 else 21);
   Number_Of_Temp_To_Humd : constant Natural :=
     (if Doing_Example then 2 else 37);
   Number_Of_Humd_To_Locn : constant Natural :=
     (if Doing_Example then 2 else 36);

   --  SUBSECTION
   --  input data
   Initial_Seeds : array (1 .. Number_Of_Seeds) of Long_Long_Integer;

   type Map is record
      Source      : Long_Long_Integer;
      Destination : Long_Long_Integer;
      Length      : Long_Long_Integer;
   end record;

   type Map_Array is array (Positive range <>) of Map;

   Seed_To_Soil            : Map_Array (1 .. Number_Of_Seed_To_Soil);
   Soil_To_Fertilizer      : Map_Array (1 .. Number_Of_Soil_To_Fert);
   Fertilizer_To_Water     : Map_Array (1 .. Number_Of_Fert_To_Watr);
   Water_To_Light          : Map_Array (1 .. Number_Of_Watr_To_Lite);
   Light_To_Temperature    : Map_Array (1 .. Number_Of_Lite_To_Temp);
   Temperature_To_Humidity : Map_Array (1 .. Number_Of_Temp_To_Humd);
   Humidity_To_Location    : Map_Array (1 .. Number_Of_Humd_To_Locn);

   --  SECTION I/O

   procedure Read_Map (F : IO.File_Type; M : out Map_Array) is
   --  reads all mappings for M from F
   begin

      for Ith in M'Range loop

         declare
            S   : constant String := IO.Get_Line (F);
            Pos : Positive        := 1;
         begin

            Nat_IO.Get (S (Pos .. S'Last), M (Ith).Destination, Pos);
            Pos := @ + 2;
            Nat_IO.Get (S (Pos .. S'Last), M (Ith).Source, Pos);
            Pos := @ + 2;
            Nat_IO.Get (S (Pos .. S'Last), M (Ith).Length, Pos);

         end;

      end loop;

   end Read_Map;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));
      declare
         Seed_String : constant String := IO.Get_Line (Input);
         Pos         : Positive        := 8;
      begin
         for Ith in Initial_Seeds'Range loop
            Nat_IO.Get
              (Seed_String (Pos .. Seed_String'Last), Initial_Seeds (Ith),
               Pos);
            Pos := @ + 2;
         end loop;
      end;

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Seed_To_Soil);

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Soil_To_Fertilizer);

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Fertilizer_To_Water);

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Water_To_Light);

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Light_To_Temperature);

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Temperature_To_Humidity);

      IO.Skip_Line (Input, 2);
      Read_Map (Input, Humidity_To_Location);

      IO.Close (Input);
   end Read_Input;

   --  SECTION
   --  Part 1

   function Follow_Map
     (Maps : Map_Array; Source : Long_Long_Integer) return Long_Long_Integer
   is
   --  follows the path Source takes through the given map array
   begin

      for Map of Maps loop
         if Source in Map.Source .. Map.Source + Map.Length - 1 then
            return Map.Destination + (Source - Map.Source);
         end if;
      end loop;

      --  initially i overlooked this critical instruction...
      return Source;

   end Follow_Map;

   function Find_Location (Seed : Long_Long_Integer) return Long_Long_Integer
   is
      --  finds the location to plant seed by following all the maps

      Result : Long_Long_Integer := Seed;

   begin

      Result := Follow_Map (Seed_To_Soil, Result);
      Result := Follow_Map (Soil_To_Fertilizer, Result);
      Result := Follow_Map (Fertilizer_To_Water, Result);
      Result := Follow_Map (Water_To_Light, Result);
      Result := Follow_Map (Light_To_Temperature, Result);
      Result := Follow_Map (Temperature_To_Humidity, Result);
      Result := Follow_Map (Humidity_To_Location, Result);

      return Result;

   end Find_Location;

   function Part_1 return Long_Long_Integer is
      Result : Long_Long_Integer := Long_Long_Integer'Last;
   begin
      for Seed of Initial_Seeds loop
         Result := Long_Long_Integer'Min (@, Find_Location (Seed));
      end loop;
      return Result;
   end Part_1;

   --  SECTION
   --  Part 2, the first time I did it
   --  it's REALLY long, so you probably don't want to run this,
   --  but surprisingly it's not as long as I expected to take,
   --  or maybe I was just lucky!

   function Part_2 return Long_Long_Integer is
      Result : Long_Long_Integer := Long_Long_Integer'Last;
   begin

      for Ith in 1 .. Initial_Seeds'Length / 2 loop

         --  you'll want updates
         IO.Put_Line ("Seed number" & Ith'Image);

         for Seed in
           Initial_Seeds (Ith * 2 - 1) ..
             Initial_Seeds (Ith * 2 - 1) + Initial_Seeds (Ith * 2) - 1
         loop
            Result := Long_Long_Integer'Min (@, Find_Location (Seed));
         end loop;

      end loop;

      return Result;

   end Part_2;

   --  SECTION
   --  Part 2, done more efficiently
   --
   --  treat each interval as a proper interval,
   --  and follow the interval's endpoints through the map,
   --  splitting the interval when necessary

   type Interval is record
      Left, Right : Long_Long_Integer;
   end record;

   package Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Interval);

   function Split_Over_Map
     (Maps : Map_Array; I : Interval_Vectors.Vector)
      return Interval_Vectors.Vector
   is
      --  splits I according to how it interacts with the map array

      Result : Interval_Vectors.Vector := I;

   begin

      for Map of Maps loop

         declare
            Temp  : Interval_Vectors.Vector;
            Left  : constant Long_Long_Integer := Map.Source;
            Right : constant Long_Long_Integer := Map.Source + Map.Length - 1;
         begin

            --  the diagrams on each branch were really,
            --   REALLY helpful during debugging,
            --  and probably help read and comprehend, so they're staying
            for I of Result loop

               if I.Left < Left then

                  if I.Right < Left then        --  [*******] (-----)
                     Temp.Append (I);

                  elsif I.Right <= Right then   --  [***(***]-)
                     Temp.Append
                       (Interval'(Left => I.Left, Right => Left - 1));
                     Temp.Append (Interval'(Left => Left, Right => I.Right));

                  else                          --  [*(***)*]
                     Temp.Append
                       (Interval'(Left => I.Left, Right => Left - 1));
                     Temp.Append (Interval'(Left, Right));
                     Temp.Append
                       (Interval'(Left => Right + 1, Right => I.Right));

                  end if;

               elsif I.Left <= Right then

                  if I.Right <= Right then      --  (---[*******]-)
                     --  IO.Put_Line ("(---[*******]-)");
                     Temp.Append (I);

                  else                          --  (---[**)****]
                     --  IO.Put_Line ("(---[**)****]");
                     Temp.Append (Interval'(Left => I.Left, Right => Right));
                     Temp.Append
                       (Interval'(Left => Right + 1, Right => I.Right));

                  end if;

               else                             --  (-----) [*******]
                  Temp.Append (I);

               end if;

            end loop;

            Result := Temp;

         end;

      end loop;

      return Result;

   end Split_Over_Map;

   function Map_And_Split_Intervals
     (Maps : Map_Array; Intervals : Interval_Vectors.Vector)
      return Interval_Vectors.Vector
   is
      --  follows all intervals through the map array, splitting if necessary
      Result : Interval_Vectors.Vector := Split_Over_Map (Maps, Intervals);

   begin

      for Interval of Result loop
         for Map of Maps loop

            if Interval.Left in Map.Source .. Map.Source + Map.Length - 1 then
               Interval.Left := (Map.Destination - Map.Source) + Interval.Left;
               Interval.Right :=
                 (Map.Destination - Map.Source) + Interval.Right;
               exit;
            end if;

         end loop;
      end loop;

      return Result;

   end Map_And_Split_Intervals;

   function Part_2_Efficiently return Long_Long_Integer is
      --  assumes the maps are sorted!
      Result    : Long_Long_Integer := Long_Long_Integer'Last;
      Intervals : Interval_Vectors.Vector;
   begin

      for Ith in 1 .. Initial_Seeds'Length / 2 loop
         Intervals.Append
           (Interval'
              (Left  => Initial_Seeds (2 * Ith - 1),
               Right =>
                 Initial_Seeds (2 * Ith - 1) + Initial_Seeds (2 * Ith) - 1));
      end loop;

      Intervals := Map_And_Split_Intervals (Seed_To_Soil, Intervals);
      Intervals := Map_And_Split_Intervals (Soil_To_Fertilizer, Intervals);
      Intervals := Map_And_Split_Intervals (Fertilizer_To_Water, Intervals);
      Intervals := Map_And_Split_Intervals (Water_To_Light, Intervals);
      Intervals := Map_And_Split_Intervals (Light_To_Temperature, Intervals);
      Intervals :=
        Map_And_Split_Intervals (Temperature_To_Humidity, Intervals);
      Intervals := Map_And_Split_Intervals (Humidity_To_Location, Intervals);

      for Interval of Intervals loop
         Result := Long_Long_Integer'Min (Result, Interval.Left);
      end loop;

      return Result;

   end Part_2_Efficiently;

   procedure Sort_Maps is

      function "<" (Left, Right : Map) return Boolean is
        (Left.Source < Right.Source);
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Map, Array_Type => Map_Array);

   begin
      Sort (Seed_To_Soil);
      Sort (Soil_To_Fertilizer);
      Sort (Fertilizer_To_Water);
      Sort (Water_To_Light);
      Sort (Light_To_Temperature);
      Sort (Temperature_To_Humidity);
      Sort (Humidity_To_Location);
   end Sort_Maps;

   procedure Put_Map (Maps : Map_Array) is
      --  useful for debugging

      Width : constant Positive := (if Doing_Example then 3 else 11);

   begin

      for Map of Maps loop
         IO.Put ("   ");
         Nat_IO.Put (Map.Source, Width);
         IO.Put (" .. ");
         Nat_IO.Put (Map.Source + Map.Length - 1, Width);
         IO.Put (" -> ");
         Nat_IO.Put (Map.Destination, Width);
         IO.Put (" .. ");
         Nat_IO.Put (Map.Destination + Map.Length - 1, Width);
         IO.New_Line;
      end loop;

   end Put_Map;

   procedure Put_Maps is
   begin
      IO.Put_Line ("Seed to Soil");
      Put_Map (Seed_To_Soil);
      IO.Put_Line ("Soil to Fertilizer");
      Put_Map (Soil_To_Fertilizer);
      IO.Put_Line ("Fertilizer to Water");
      Put_Map (Fertilizer_To_Water);
      IO.Put_Line ("Water to Light");
      Put_Map (Water_To_Light);
      IO.Put_Line ("Light to Temperature");
      Put_Map (Light_To_Temperature);
      IO.Put_Line ("Temperature to Humidity");
      Put_Map (Temperature_To_Humidity);
      IO.Put_Line ("Humidity to Location");
      Put_Map (Humidity_To_Location);
   end Put_Maps;

begin
   Read_Input;
   IO.Put_Line ("the closest location that needs a seed is" & Part_1'Image);
   Sort_Maps;
   --  Put_Maps;
   IO.Put_Line ("no, it's actually" & Part_2_Efficiently'Image);
   --  IO.Put_Line ("no, it's actually" & Part_2'Image);
end Day5;
