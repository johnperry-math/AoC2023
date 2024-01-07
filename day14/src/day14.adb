pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 14: Parabolic Reflector Dish
--
--  part 1: compute the load incurred by the movable rocks after tilting north
--
--  part 2: compute the load after 1 billion spin cycles

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day14 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;
   Visualize     : constant Boolean := False;

   --  SECTION
   --  global types and variables

   type Object is (Movable, Immovable, Ash);

   type Load is range 0 .. 2**64 - 1;
   --  this pre-emptive attack on large values turned out to be unnecessary

   Side_Length : constant Positive := (if Doing_Example then 10 else 100);
   subtype Side_Range is Positive range 1 .. Side_Length;

   System : array (Side_Range, Side_Range) of Object;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
      Invalid_Character : exception;
   begin

      IO.Open
        (Input, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt"));

      for Row in System'Range (1) loop
         declare
            S : constant String := IO.Get_Line (Input);
         begin

            for Col in System'Range (2) loop
               System (Row, Col) :=
                 (case S (Col) is when '#' => Immovable, when 'O' => Movable,
                    when '.' => Ash,
                    when others => raise Invalid_Character with S (Col)'Image);

            end loop;
         end;
      end loop;

      IO.Close (Input);
   end Read_Input;

   procedure Put_System is
   --  useful for debugging
   begin

      for Row in Side_Range loop
         for Col in Side_Range loop

            IO.Put
              ((case System (Row, Col) is when Movable => 'O',
                  when Immovable => '#', when Ash => '.'));

         end loop;
         IO.New_Line;
      end loop;
   end Put_System;

   procedure Write_Visualization (Frame : in out Natural) is
      --  who doesn't like a pretty picture? or even an animation?

      Output : IO.File_Type;

      Ground : constant Natural := 128;
      Roller : constant Natural := 196;
      Still  : constant Natural := 0;

      Suffix       : String (1 .. 4) := [others => '0'];
      Naive_Suffix : constant String := Frame'Image;

   begin

      Suffix (4) := Naive_Suffix (Naive_Suffix'Last);
      if Frame >= 10 then
         Suffix (3) := Naive_Suffix (Naive_Suffix'Last - 1);
      end if;
      if Frame >= 100 then
         Suffix (2) := Naive_Suffix (Naive_Suffix'Last - 2);
      end if;
      if Frame >= 1_000 then
         Suffix (3) := Naive_Suffix (2);
      end if;

      IO.Create (Output, IO.Out_File, "frames/frame_" & Suffix & ".pgm");

      --  header
      IO.Put_Line (Output, "P2");
      IO.Put_Line (Output, Positive'Image (System'Length (2) * 2));
      IO.Put_Line (Output, Positive'Image (System'Length (1) * 2));
      IO.Put_Line (Output, "255"); -- max color
      IO.New_Line (Output);

      --  data
      for Row in System'Range (1) loop
         --  double each row for visibility
         for Each in 1 .. 2 loop
            for Col in System'Range (2) loop

               --  double each column for visibility
               IO.Put
                 (Output,
                  (case System (Row, Col) is when Ash => Ground'Image,
                     when Movable => Roller'Image,
                     when Immovable => Still'Image));
               IO.Put
                 (Output,
                  (case System (Row, Col) is when Ash => Ground'Image,
                     when Movable => Roller'Image,
                     when Immovable => Still'Image));

            end loop;
            IO.New_Line (Output);
         end loop;
      end loop;

      IO.Close (Output);

      Frame := @ + 1;

   end Write_Visualization;

   --  SECTION
   --  Part 1

   function Part_1 return Load is
      --  counts the number of rows each movable rock would move north,
      --  taking into account the number of movable rocks in the way,
      --  then uses that to determine the load

      Result : Load := 0;

   begin

      for Row in System'Range (1) loop
         for Col in System'Range (2) loop

            if System (Row, Col) = Movable then
               declare
                  Travel_Row         : Natural := Row;
                  Movable_In_The_Way : Natural := 0;
               begin

                  while Travel_Row > 1
                    and then System (Travel_Row - 1, Col) /= Immovable
                  loop

                     Travel_Row := @ - 1;
                     if System (Travel_Row, Col) = Movable then
                        Movable_In_The_Way := @ + 1;
                     end if;

                  end loop;

                  Result :=
                    @ +
                    Load (Side_Length - (Travel_Row + Movable_In_The_Way) + 1);

               end;
            end if;
         end loop;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2
   --
   --  the trick we used in part 1 does not seem useful for part 2;
   --  we need to record the result of every tilt

   procedure Tilt_North is
   begin
      for Row in System'Range (1) loop
         for Col in System'Range (2) loop

            if System (Row, Col) = Movable then

               declare
                  Travel_Row : Natural := Row;
               begin

                  while Travel_Row > 1
                    and then System (Travel_Row - 1, Col) = Ash
                  loop
                     Travel_Row := @ - 1;
                  end loop;

                  System (Row, Col)        := Ash;
                  System (Travel_Row, Col) := Movable;

               end;

            end if;

         end loop;
      end loop;
   end Tilt_North;

   procedure Tilt_South is
   begin
      for Row in reverse System'Range (1) loop
         for Col in System'Range (2) loop

            if System (Row, Col) = Movable then

               declare
                  Travel_Row : Natural := Row;
               begin

                  while Travel_Row < System'Last (1)
                    and then System (Travel_Row + 1, Col) = Ash
                  loop
                     Travel_Row := @ + 1;
                  end loop;

                  System (Row, Col)        := Ash;
                  System (Travel_Row, Col) := Movable;

               end;

            end if;

         end loop;
      end loop;
   end Tilt_South;

   procedure Tilt_West is
   begin
      for Row in System'Range (1) loop
         for Col in System'Range (2) loop

            if System (Row, Col) = Movable then

               declare
                  Travel_Col : Natural := Col;
               begin

                  while Travel_Col > 1
                    and then System (Row, Travel_Col - 1) = Ash
                  loop
                     Travel_Col := @ - 1;
                  end loop;

                  System (Row, Col)        := Ash;
                  System (Row, Travel_Col) := Movable;

               end;

            end if;

         end loop;
      end loop;
   end Tilt_West;

   procedure Tilt_East is
   begin
      for Row in System'Range (1) loop
         for Col in reverse System'Range (2) loop

            if System (Row, Col) = Movable then

               declare
                  Travel_Col : Natural := Col;
               begin

                  while Travel_Col < System'Last (1)
                    and then System (Row, Travel_Col + 1) = Ash
                  loop
                     Travel_Col := @ + 1;
                  end loop;

                  System (Row, Col)        := Ash;
                  System (Row, Travel_Col) := Movable;

               end;

            end if;

         end loop;
      end loop;
   end Tilt_East;

   function Part_2 return Load is

      package Load_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Load);

      Result, Final_Load : Load;
      Results            : Load_Vectors.Vector;

      Iterations       : constant Positive := 200;
      --  hopefully 200 is enough to see the pattern
      --  if not, try raising this a bit;
      --  I'd be shocked if 1_000 didn't do the trick
      Potential_Period : constant Positive := 100;
      Insufficient_Iterations : exception;
      --  when the number of spins isn't sufficient
      --  to determine the period

      Frame : Natural := 0;

   begin

      for Each in 1 .. Iterations loop

         Tilt_North;
         if Visualize then
            Write_Visualization (Frame);
         end if;
         Tilt_West;
         if Visualize then
            Write_Visualization (Frame);
         end if;
         Tilt_South;
         if Visualize then
            Write_Visualization (Frame);
         end if;
         Tilt_East;
         if Visualize then
            Write_Visualization (Frame);
         end if;

         --  record the current load

         Result := 0;

         for Row in Side_Range loop
            for Col in Side_Range loop

               if System (Row, Col) = Movable then
                  Result := @ + Load (Side_Length - Row + 1);
               end if;

            end loop;
         end loop;

         Results.Append (Result);

      end loop;

      --  determine the period from the recorded data

      Final_Load := 0;

      for Period in 3 .. Potential_Period loop
         if
           (for all Ith in 0 .. Period - 1 =>
              Results (Iterations - Ith) = Results (Iterations - Ith - Period))
         then
            IO.Put_Line ("Period is" & Period'Image);
            Final_Load :=
              Results
                (Iterations - Period +
                 ((1_000_000_000 - Iterations) mod Period));
            exit;
         end if;
      end loop;

      if Final_Load = 0 then
         raise Insufficient_Iterations
           with "recompile with more spin cycles or a higher potential period";
      end if;

      return Final_Load;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("The total load is" & Part_1'Image);
   IO.Put_Line ("After 1 billion spin cycles, the load is" & Part_2'Image);
end Day14;
