pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 22: Sand slabs
--
--  part 1: how many sand slabs can we disintegrate without making others fall?
--
--  part 2: find the sum of the slabs that will fall when we delete each slab

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

procedure Day22 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;

   Max_X, Max_Y, Max_Z : Natural := 0;
   --  the largest x, y, and z values in the input

   type Location_Record is record
      X, Y, Z : Natural;
   end record;

   type Brick_Record is record
      First, Last : Location_Record;
   end record;
   --  later we will ensure that First.Z < Last.Z

   function "<" (Left, Right : Brick_Record) return Boolean is
     (Left.First.Z < Right.First.Z
      or else
      (Left.First.Z = Right.First.Z and then Left.First.X < Right.First.X)
      or else
      (Left.First.Z = Right.First.Z and then Left.First.X = Right.First.X
       and then Left.First.Y < Right.First.Y));
   --  you might think this doesn't suffice, but:
   --  no two bricks can occupy the same location(s), so we should never
   --  encounter a situation where a brick's first location isn't enough
   --  to distinguish it from another brick's first location

   package Brick_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Brick_Record);

   package Brick_Sorter is new Brick_Vectors.Generic_Sorting;

   All_Bricks : Brick_Vectors.Vector;

   type Space is
     array (Natural range <>, Natural range <>, Natural range <>) of Boolean;

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  brick & location output

   procedure Put_Location (L : Location_Record) is
   --  useful for debugging
   begin
      Nat_IO.Put (L.X, 1);
      IO.Put (',');
      Nat_IO.Put (L.Y, 1);
      IO.Put (',');
      Nat_IO.Put (L.Z, 1);
   end Put_Location;

   procedure Put_Brick (Brick : Brick_Record) is
   --  useful for debugging
   begin
      Put_Location (Brick.First);
      IO.Put ('~');
      Put_Location (Brick.Last);
   end Put_Brick;

   --  SUBSECTION
   --  input

   procedure Read_Input is

      Input : IO.File_Type;

      Filename : constant String :=
        (if Doing_Example then "example.txt" else "input.txt");

   begin

      IO.Open (Input, IO.In_File, Filename);

      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
            Pos  : Positive        := 1;

            B    : Brick_Record;
            Temp : Location_Record;

            Differing_Dimensions : Natural := 0;
         begin

            Nat_IO.Get (Line, B.First.X, Pos);
            Max_X := Natural'Max (Max_X, B.First.X);
            Pos   := @ + 2;

            Nat_IO.Get (Line (Pos .. Line'Last), B.First.Y, Pos);
            Max_Y := Natural'Max (Max_Y, B.First.Y);
            Pos   := @ + 2;

            Nat_IO.Get (Line (Pos .. Line'Last), B.First.Z, Pos);
            Max_Z := Natural'Max (Max_Z, B.First.Z);
            Pos   := @ + 2;

            Nat_IO.Get (Line (Pos .. Line'Last), B.Last.X, Pos);
            Max_X := Natural'Max (Max_X, B.Last.X);
            Pos   := @ + 2;
            if B.First.X /= B.Last.X then
               Differing_Dimensions := @ + 1;
            end if;

            Nat_IO.Get (Line (Pos .. Line'Last), B.Last.Y, Pos);
            Max_Y := Natural'Max (Max_Y, B.Last.Y);
            Pos   := @ + 2;
            if B.First.Y /= B.Last.Y then
               Differing_Dimensions := @ + 1;
            end if;

            Nat_IO.Get (Line (Pos .. Line'Last), B.Last.Z, Pos);
            Max_Z := Natural'Max (Max_Z, B.Last.Z);
            if B.First.Z /= B.Last.Z then
               Differing_Dimensions := @ + 1;
            end if;

            if B.First.Z > B.Last.Z then
               Temp    := B.First;
               B.First := B.Last;
               B.Last  := Temp;
            end if;

            All_Bricks.Append (B);

            if Differing_Dimensions > 1 then    --  none of these, thankfully
               IO.Put ("Brick_Record is two-dimensional:");
               Put_Brick (B);
               IO.New_Line;
               --  elsif Differing_Dimensions = 0 then      --  lots of these
               --     IO.Put ("Brick_Record is zero-dimensional:");
               --     Put_Brick (B);
               --     IO.New_Line;
            end if;

         end;
      end loop;

      --  briefly useful in debugging
      --  IO.Put_Line ("Max's:" & Max_X'Image & Max_Y'Image & Max_Z'Image);

   end Read_Input;

   --  SUBSECTION
   --  drawing a space

   type Perspective_Enum is (X_Per, Y_Per);

   procedure Draw (Filled : Space; Perspective : Perspective_Enum) is
   --  this attempts to match the images given in the puzzle's exposition,
   --  except that we don't label filled spaces by their brick labels
   begin

      for Z in reverse 1 .. Max_Z loop

         if Perspective = X_Per then
            for X in 0 .. Max_X loop
               if (for some Y in 0 .. Max_Y => Filled (X, Y, Z)) then
                  IO.Put ('X');
               else
                  IO.Put ('.');
               end if;
            end loop;

         else
            for Y in 0 .. Max_Y loop
               if (for some X in 0 .. Max_X => Filled (X, Y, Z)) then
                  IO.Put ('Y');
               else
                  IO.Put ('.');
               end if;
            end loop;
         end if;

         IO.Put_Line (Z'Image);

      end loop;

      for Each in 1 .. (if Perspective = X_Per then Max_X else Max_Y) + 1 loop
         IO.Put ('-');
      end loop;
      IO.Put_Line (" 0");

   end Draw;

   --  SECTION
   --  Parts 1 and 2

   function Is_Blocked (B : Brick_Record; Used : Space) return Boolean is
      --  returns True iff B is unable to drop in Used
      --  because another brick is beneath it

      L_Min_X : constant Natural := Natural'Min (B.First.X, B.Last.X);
      L_Max_X : constant Natural := Natural'Max (B.First.X, B.Last.X);
      L_Min_Y : constant Natural := Natural'Min (B.First.Y, B.Last.Y);
      L_Max_Y : constant Natural := Natural'Max (B.First.Y, B.Last.Y);
      --  a declare expression would be ideal here, but
      --  the ada formatter hasn't caught up to that feature yet
      --  and i don't want to deal with constant complaints from VSCode

   begin

      return
        (for some X in L_Min_X .. L_Max_X =>
           (for some Y in L_Min_Y .. L_Max_Y => Used (X, Y, B.First.Z - 1)));

   end Is_Blocked;

   procedure Drop
     (B : in out Brick_Record; Used : in out Space; Dropped : out Boolean)
   is
      --  drops B as far as it can in Used, updating both B, Used, and Dropped

      Falling : Boolean := True;

   begin

      --  IO.Put_Line ("Start falling at" & B.First.Z'Image);

      Dropped := False;

      while Falling loop

         if B.First.Z = 1 or else Is_Blocked (B, Used) then
            Falling := False;

         else
            B.First.Z := @ - 1;
            B.Last.Z  := @ - 1;
            Dropped   := True;
         end if;

      end loop;

      --  IO.Put_Line ("Stop falling at" & B.First.Z'Image);

      for X in B.First.X .. B.Last.X loop
         for Y in B.First.Y .. B.Last.Y loop
            for Z in B.First.Z .. B.Last.Z loop
               Used (X, Y, Z) := True;
            end loop;
         end loop;
      end loop;

   end Drop;

   procedure Fall_In
     (This        : in out Space; Bricks : in out Brick_Vectors.Vector;
      Num_Dropped :    out Natural)
   is
      --  drops all Bricks in This, starting from the lowest to the highest
      --  correctness assumes that Bricks has already been so sorted
      --  updates Num_Dropped to indicate how many bricks changed position

      Dropped : Boolean;

   begin

      Num_Dropped := 0;

      for Brick of Bricks loop
         Drop (Brick, This, Dropped);
         if Dropped then
            Num_Dropped := @ + 1;
         end if;
      end loop;

   end Fall_In;

   --  SUBSECTION
   --  Part 1

   function Can_Remove (Ith : Positive; Used : Space) return Boolean is
      --  returns True iff we can drop the Ith All_Bricks
      --  without causing another brick to fall
      --
      --  correctness assumes All_Bricks has been sorted

      Brick : Brick_Vectors.Reference_Type renames All_Bricks (Ith);

      L_Min_X : constant Natural := Natural'Min (Brick.First.X, Brick.Last.X);
      L_Max_X : constant Natural := Natural'Max (Brick.First.X, Brick.Last.X);
      L_Min_Y : constant Natural := Natural'Min (Brick.First.Y, Brick.Last.Y);
      L_Max_Y : constant Natural := Natural'Max (Brick.First.Y, Brick.Last.Y);
      Z       : constant Natural := Brick.Last.Z;

      Removable : Boolean := True;
      --  guilty until proven innocent

      Scratch : Space (Used'Range (1), Used'Range (2), Used'Range (3)) := Used;

   begin

      --  initialize the scratch space to empty, adding one at a time
      for X in L_Min_X .. L_Max_X loop
         for Y in L_Min_Y .. L_Max_Y loop
            for Z in Brick.First.Z .. Brick.Last.Z loop
               Scratch (X, Y, Z) := False;
            end loop;
         end loop;
      end loop;

      for Jth in
        Ith + 1 .. All_Bricks.Last_Index when Removable
      and then All_Bricks (Jth).First.Z <= Z + 1
      loop

         if All_Bricks (Jth).First.Z = Z + 1
           and then not Is_Blocked (All_Bricks (Jth), Scratch)
         then
            Removable := False;
         end if;

         --  Jth := @ + 1;

      end loop;

      return Removable;

   end Can_Remove;

   function Part_1 return Natural is

      Result : Natural := 0;

      Used : Space (0 .. Max_X, 0 .. Max_Y, 0 .. Max_Z) :=
        [others => [others => [others => False]]];

      Num_Dropped : Natural;  --  not used here, but needed for signature

   begin

      Used := [others => [others => [others => False]]];

      Fall_In (Used, All_Bricks, Num_Dropped);
      IO.Put_Line ("we dropped" & Num_Dropped'Image & "!");
      Brick_Sorter.Sort (All_Bricks);

      for Ith in All_Bricks.First_Index .. All_Bricks.Last_Index loop
         if Can_Remove (Ith, Used) then
            Result := @ + 1;
         end if;
      end loop;

      return Result;

   end Part_1;

   --  SUBSECTION
   --  Part 2

   function Part_2 return Natural is

      Result : Natural := 0;

      Clear   : constant Space (0 .. Max_X, 0 .. Max_Y, 0 .. Max_Z) :=
        [others => [others => [others => False]]];
      Scratch : Space (0 .. Max_X, 0 .. Max_Y, 0 .. Max_Z)          :=
        [others => [others => [others => False]]];

      Brick_Scratch : Brick_Vectors.Vector;

      Num_Dropped : Natural;

   begin

      for Ith in All_Bricks.First_Index .. All_Bricks.Last_Index loop
         Scratch       := Clear;
         Brick_Scratch := Brick_Vectors.Copy (All_Bricks);
         Brick_Scratch.Delete (Ith);
         Fall_In (Scratch, Brick_Scratch, Num_Dropped);
         Result := @ + Num_Dropped;
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   Brick_Sorter.Sort (All_Bricks);
   IO.Put_Line ("We can disintegrate" & Part_1'Image & " bricks");
   IO.Put_Line ("Sum of bricks that would fall is" & Part_2'Image);
end Day22;
