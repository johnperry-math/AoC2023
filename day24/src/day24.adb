pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 24: Never Tell Me The Odds
--
--  part 1: how many hailstones intersect, in a certain area, in the future?
--
--  part 2: from what position can you throw a stone
--          so that it hits every hailstone?

with Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

procedure Day24 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   Doing_Example : constant Boolean := False;

   --  SUBSECTION
   --  data as floating point

   type Float_14 is digits 16;

   package Float_14_IO is new IO.Float_IO (Num => Float_14);

   Part_1_Min : constant Float_14 :=
     (if Doing_Example then 7.0 else 200_000_000_000_000.0);
   Part_1_Max : constant Float_14 :=
     (if Doing_Example then 27.0 else 400_000_000_000_000.0);

   type Hailstone_Record is record
      X, Y, Z    : Float_14;
      Dx, Dy, Dz : Float_14;
   end record;

   package Hailstone_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hailstone_Record);

   All_Hailstones : Hailstone_Vecs.Vector;

   type Reduction_Matrix is array (1 .. 6, 1 .. 7) of Float_14;
   --  used to perform a primitive Gaussian elimination

   --  SUBSECTION
   --  data as very large integer

   --  Long_Integer is mis-documented in gnat: it's -(2**64) .. +(2**64 - 1),
   --  NOT -(2**32) .. +(2**32 - 1); swapping the definitions below proves it
   --
   --  type Ludicrous_Size is range -(2**32) .. +(2**32 - 1)
   subtype Ludicrous_Size is Long_Integer;
   package Ludicrous_IO is new IO.Integer_IO (Num => Ludicrous_Size);

   type Ludicrous_Stone_Record is record
      X, Y, Z    : Ludicrous_Size;
      Dx, Dy, Dz : Ludicrous_Size;
   end record;

   package Ludicrous_Stone_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Ludicrous_Stone_Record);

   All_Ludicrous : Ludicrous_Stone_Vecs.Vector;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;

      Filename : constant String :=
        (if Doing_Example then "example.txt" else "input.txt");
   begin

      IO.Open (Input, IO.In_File, Filename);

      while not IO.End_Of_File (Input) loop
         declare
            Line         : constant String := IO.Get_Line (Input);
            Hailstone    : Hailstone_Record;
            Ludicrous    : Ludicrous_Stone_Record;
            Pos_1, Pos_2 : Positive        := 1;
         begin

            Float_14_IO.Get (Line, Hailstone.X, Pos_1);
            Ludicrous_IO.Get (Line, Ludicrous.X, Pos_2);

            Pos_1 := @ + 3;
            Pos_2 := @ + 3;
            Float_14_IO.Get (Line (Pos_1 .. Line'Last), Hailstone.Y, Pos_1);
            Ludicrous_IO.Get (Line (Pos_2 .. Line'Last), Ludicrous.Y, Pos_2);

            Pos_1 := @ + 3;
            Pos_2 := @ + 3;
            Float_14_IO.Get (Line (Pos_1 .. Line'Last), Hailstone.Z, Pos_1);
            Ludicrous_IO.Get (Line (Pos_2 .. Line'Last), Ludicrous.Z, Pos_2);

            Pos_1 := @ + 4;
            Pos_2 := @ + 4;
            Float_14_IO.Get (Line (Pos_1 .. Line'Last), Hailstone.Dx, Pos_1);
            Ludicrous_IO.Get (Line (Pos_2 .. Line'Last), Ludicrous.Dx, Pos_2);

            Pos_1 := @ + 3;
            Pos_2 := @ + 3;
            Float_14_IO.Get (Line (Pos_1 .. Line'Last), Hailstone.Dy, Pos_1);
            Ludicrous_IO.Get (Line (Pos_2 .. Line'Last), Ludicrous.Dy, Pos_2);

            Pos_1 := @ + 3;
            Pos_2 := @ + 3;
            Float_14_IO.Get (Line (Pos_1 .. Line'Last), Hailstone.Dz, Pos_1);
            Ludicrous_IO.Get (Line (Pos_2 .. Line'Last), Ludicrous.Dz, Pos_2);

            All_Hailstones.Append (Hailstone);
            All_Ludicrous.Append (Ludicrous);

         end;
      end loop;

      IO.Close (Input);

   end Read_Input;

   procedure Put_Matrix (Matrix : Reduction_Matrix) is
   begin
      for Row in 1 .. 6 loop
         for Col in 1 .. 7 loop
            Float_14_IO.Put
              (Matrix (Row, Col), Fore => 15, Aft => 0, Exp => 0);
            IO.Put (' ');
         end loop;
         IO.New_Line;
      end loop;
   end Put_Matrix;

   --  SECTION
   --  Parts 1 and 2

   type Intersection (Valid : Boolean) is record
      case Valid is
         when True =>
            X, Y, Z : Float_14;
            --  location of valid intersection
         when False =>
            null;
      end case;
   end record;

   function Intersect_In_Future (Ith, Jth : Positive) return Intersection is
      --  returns a valid intersection iff the Ith and Jth hailstones intersect
      --  at some point in the future when ignoring motion in the z direction
      --
      --  in particular, you get False if the stones are parallel in x-y plane
      --  or if the intersection is "backward" with respect to current position

      P_Ith, P_Jth : Hailstone_Record;
      M_Ith, M_Jth : Float_14;            --  slopes
      X, Y         : Float_14;            --  (X, Y) location of intersection

   begin

      P_Ith := All_Hailstones (Ith);
      P_Jth := All_Hailstones (Jth);

      M_Ith := P_Ith.Dy / P_Ith.Dx;
      M_Jth := P_Jth.Dy / P_Jth.Dx;

      if abs (M_Ith - M_Jth) > 0.000_000_000_01 then  --  probably not parallel

         X :=
           ((P_Jth.Y - P_Ith.Y) + (M_Ith * P_Ith.X - M_Jth * P_Jth.X)) /
           (M_Ith - M_Jth);

         if X in Part_1_Min .. Part_1_Max                --  in desired range
           and then (X - P_Ith.X) / P_Ith.Dx > 0.0       --  in the future
         then

            Y := M_Ith * (X - P_Ith.X) + P_Ith.Y;

            if Y in Part_1_Min .. Part_1_Max then        --  in desired range
               if (Y - P_Jth.Y) / P_Jth.Dy > 0.0 then    --  in the future
                  return
                    Intersection'(Valid => True, X => X, Y => Y, Z => 0.0);
               end if;
            end if;

         end if;
      end if;

      return Intersection'(Valid => False);

   end Intersect_In_Future;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin

      for Ith in All_Hailstones.First_Index .. All_Hailstones.Last_Index - 1
      loop
         for Jth in Ith + 1 .. All_Hailstones.Last_Index loop
            if Intersect_In_Future (Ith, Jth).Valid then
               Result := @ + 1;
            end if;
         end loop;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2, done two ways

   --  SUBSECTION
   --  this essentially translates TheZigerionScammer's Python solution
   --  i understand the general idea, but i'm a bit lost on a few details

   function "<" (Left, Right : Ludicrous_Stone_Record) return Boolean is
     (Left.X < Right.X or else (Left.X = Right.X and then Left.Y < Right.Y)
      or else
      (Left.X = Right.X and then Left.Y = Right.Y and then Left.Z < Right.Z)
      or else
      (Left.X = Right.X and then Left.Y = Right.Y and then Left.Z = Right.Z
       and then Left.Dx < Right.Dx)
      or else
      (Left.X = Right.X and then Left.Y = Right.Y and then Left.Z = Right.Z
       and then Left.Dx = Right.Dx and then Left.Dy < Right.Dy)
      or else
      (Left.X = Right.X and then Left.Y = Right.Y and then Left.Z = Right.Z
       and then Left.Dx = Right.Dx and then Left.Dy = Right.Dy
       and then Left.Dz < Right.Dz));

   package Ludicrous_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Ludicrous_Size);

   function Part_2 return Ludicrous_Size is
      package Sorter is new Ludicrous_Stone_Vecs.Generic_Sorting;

      First, Second : Ludicrous_Stone_Record;

      Dx, Dy, Dz          : Ludicrous_Size;
      Difference          : Ludicrous_Size;
      X_Pos, Y_Pos, Z_Pos : Ludicrous_Size;
      Time                : Ludicrous_Size;

      New_X_Set, New_Y_Set, New_Z_Set                   : Ludicrous_Sets.Set;
      Potential_X_Set, Potential_Y_Set, Potential_Z_Set : Ludicrous_Sets.Set;

      First_M, Second_M, Ca, Cb : Float_14;
   begin

      --  he doesn't mention it, but this next line is load-bearing
      Sorter.Sort (All_Ludicrous);

      --  find hailstones that travel a constant distance apart
      --  in some dimension
      for Ith in All_Ludicrous.First_Index .. All_Ludicrous.Last_Index - 1 loop
         First := All_Ludicrous (Ith);
         for Jth in Ith + 1 .. All_Ludicrous.Last_Index loop
            Second := All_Ludicrous (Jth);

            --  x dimension
            if First.Dx = Second.Dx and then abs (First.Dx) > 100 then

               New_X_Set.Clear;

               --  find the difference in the x values;
               --  check whether this is congruent to difference in velocity;
               --  if so, these are parallel in x dimension
               Difference := Second.X - First.X;

               for V in Ludicrous_Size (-1_000) .. Ludicrous_Size (1_000) loop
                  if V /= First.Dx and then Difference mod (V - First.Dx) = 0
                  then
                     New_X_Set.Insert (V);
                  end if;
               end loop;

               if Potential_X_Set.Is_Empty then
                  Potential_X_Set := Ludicrous_Sets.Copy (New_X_Set);
               else
                  Potential_X_Set := Potential_X_Set.Intersection (New_X_Set);
               end if;

            end if;

            -- y dimension
            if First.Dy = Second.Dy and then abs (First.Dy) > 100 then

               New_Y_Set.Clear;

               Difference := Second.Y - First.Y;

               for V in Ludicrous_Size (-1_000) .. Ludicrous_Size (1_000) loop
                  if V /= First.Dy and then Difference mod (V - First.Dy) = 0
                  then
                     New_Y_Set.Insert (V);
                  end if;
               end loop;

               if Potential_Y_Set.Is_Empty then
                  Potential_Y_Set := Ludicrous_Sets.Copy (New_Y_Set);
               else
                  Potential_Y_Set := Potential_Y_Set.Intersection (New_Y_Set);
               end if;

            end if;

            if First.Dz = Second.Dz and then abs (First.Dz) > 100 then

               New_Z_Set.Clear;

               Difference := Second.Z - First.Z;

               for V in Ludicrous_Size (-1_000) .. Ludicrous_Size (1_000) loop
                  if V /= First.Dz and then Difference mod (V - First.Dz) = 0
                  then
                     New_Z_Set.Insert (V);
                  end if;
               end loop;

               if Potential_Z_Set.Is_Empty then
                  Potential_Z_Set := Ludicrous_Sets.Copy (New_Z_Set);
               else
                  Potential_Z_Set := Potential_Z_Set.Intersection (New_Z_Set);
               end if;

            end if;

         end loop;
      end loop;

      --  at this point there should be only one element;
      --  obtain its Dx, Dy, Dz
      Dx := Ludicrous_Sets.Element (Potential_X_Set.First);
      Dy := Ludicrous_Sets.Element (Potential_Y_Set.First);
      Dz := Ludicrous_Sets.Element (Potential_Z_Set.First);

      First  := All_Ludicrous (1);
      Second := All_Ludicrous (2);

      --  i get lost here
      --  this seems to compute the slope of the slopes?
      --  but then it's used as if it's a regular slope
      First_M  := Float_14 (First.Dy - Dy) / Float_14 (First.Dx - Dx);
      Second_M := Float_14 (Second.Dy - Dy) / Float_14 (Second.Dx - Dx);
      --  no idea what this is about
      Ca       := Float_14 (First.Y) - First_M * Float_14 (First.X);
      Cb       := Float_14 (Second.Y) - Second_M * Float_14 (Second.X);
      --  find the point where we must originate
      X_Pos    := Ludicrous_Size ((Cb - Ca) / (First_M - Second_M));
      Y_Pos    := Ludicrous_Size (First_M * Float_14 (X_Pos) + Ca);
      Time     := (X_Pos - First.X) / (First.Dx - Dx);
      Z_Pos    := First.Z + (First.Dz - Dz) * Time;

      return X_Pos + Y_Pos + Z_Pos;

   end Part_2;

   --  SUBSECTION
   --  part 2 via groebner basis-like methods

   function Part_2_By_GB return Ludicrous_Size is

      First, Second, Third : Hailstone_Record;
      --  three non-parallel stones

      Found : Boolean := False;
      --  whether we've found the stones yet

      Matrix  : Reduction_Matrix;
      Pivoter : Float_14;

      X, Y, Z : Float_14;
      --  the solution

   begin

      --  find 3 intersecting stones

      for Ith in
        All_Hailstones.First_Index ..
          All_Hailstones.Last_Index - 2 when not Found
      loop
         for Jth in Ith + 1 .. All_Hailstones.Last_Index - 1 when not Found
         loop

            declare
               Int : constant Intersection := Intersect_In_Future (Ith, Jth);
            begin

               if Int.Valid then

                  First  := All_Hailstones (Ith);
                  Second := All_Hailstones (Jth);

                  for Kth in
                    Jth + 1 .. All_Hailstones.Last_Index when not Found
                  loop
                     declare
                        Int : constant Intersection :=
                          Intersect_In_Future (Jth, Kth);
                     begin
                        if Int.Valid then
                           Third := All_Hailstones (Kth);
                           Found := True;
                        end if;
                     end;
                  end loop;

               end if;
            end;
         end loop;
      end loop;

      --  matrix columns set up as
      --  | dx | dy | dz | x | y | z |
      --  we want only x, y, z, so this makes for a little less work
      --
      --  matrix coefficients determined by some Groebner basis-like methods:
      --
      --  * we want to know x, y, z, dx, dy, dz
      --  * we know First, Second, Third
      --  * the problem implies the following equations:
      --       x + dx * t1 = First.x + First.dx * t1
      --    ...and so forth for y, z, dy, dz, t2, t3, Second, Third
      --  * rewrite the equations to eliminate the quadratic terms
      --    and you get equations with the coefficients below
      Matrix :=
        [[First.Y - Second.Y, Second.X - First.X, 0.0, Second.Dy - First.Dy,
        First.Dx - Second.Dx, 0.0,
        -(First.X * First.Dy - First.Y * First.Dx - Second.X * Second.Dy +
         Second.Y * Second.Dx)],
        [First.Z - Second.Z, 0.0, Second.X - First.X, Second.Dz - First.Dz,
        0.0, First.Dx - Second.Dx,
        -(First.X * First.Dz - First.Z * First.Dx - Second.X * Second.Dz +
         Second.Z * Second.Dx)],
        [0.0, First.Z - Second.Z, Second.Y - First.Y, 0.0,
        Second.Dz - First.Dz, First.Dy - Second.Dy,
        -(First.Y * First.Dz - First.Z * First.Dy - Second.Y * Second.Dz +
         Second.Z * Second.Dy)],
        [First.Y - Third.Y, Third.X - First.X, 0.0, Third.Dy - First.Dy,
        First.Dx - Third.Dx, 0.0,
        -(First.X * First.Dy - First.Y * First.Dx - Third.X * Third.Dy +
         Third.Y * Third.Dx)],
        [First.Z - Third.Z, 0.0, Third.X - First.X, Third.Dz - First.Dz, 0.0,
        First.Dx - Third.Dx,
        -(First.X * First.Dz - First.Z * First.Dx - Third.X * Third.Dz +
         Third.Z * Third.Dx)],
        [0.0, First.Z - Third.Z, Third.Y - First.Y, 0.0, Third.Dz - First.Dz,
        First.Dy - Third.Dy,
        -(First.Y * First.Dz - First.Z * First.Dy - Third.Y * Third.Dz +
         Third.Z * Third.Dy)]];

      --  make upper-triangular
      --  the following approach is quite naive and not very good in general
      for Pivot in 1 .. 5 loop

         if Matrix (Pivot, Pivot) = 0.0 then
            for Col in Pivot .. 7 loop
               Pivoter                 := Matrix (Pivot, Col);
               Matrix (Pivot, Col)     := Matrix (Pivot + 1, Col);
               Matrix (Pivot + 1, Col) := Pivoter;
            end loop;
         end if;

         for Row in Pivot + 1 .. 6 loop
            Pivoter := Matrix (Row, Pivot);
            for Col in Pivot .. 7 loop
               Matrix (Row, Col) :=
                 @ - Pivoter / Matrix (Pivot, Pivot) * Matrix (Pivot, Col);
            end loop;
         end loop;

      end loop;

      --  now clear last two non-pivot columns in last 3 rows
      for Row in 4 .. 5 loop
         Pivoter := Matrix (Row, 6);
         for Col in 6 .. 7 loop
            Matrix (Row, Col) := @ - Pivoter / Matrix (6, 6) * Matrix (6, Col);
         end loop;
      end loop;
      Pivoter := Matrix (4, 5);
      for Col in 5 .. 7 loop
         Matrix (4, Col) := @ - Pivoter / Matrix (5, 5) * Matrix (5, Col);
      end loop;

      --  we now have a matrix whose last 3 rows have the form
      --    0 0 0 a 0 0 b
      --    0 0 0 0 c 0 d
      --    0 0 0 0 0 e f
      --  this is easy to solve

      X := Matrix (4, 7) / Matrix (4, 4);
      Y := Matrix (5, 7) / Matrix (5, 5);
      Z := Matrix (6, 7) / Matrix (6, 6);

      return Ludicrous_Size (X + Y + Z);

   end Part_2_By_GB;

begin
   Read_Input;
   IO.Put_Line
     ("exactly" & Part_1'Image & " intersections occur in test area");
   IO.Put_Line ("magical number via gb is" & Part_2_By_GB'Image);
   IO.Put_Line ("magical number via magic is" & Part_2'Image);
end Day24;
