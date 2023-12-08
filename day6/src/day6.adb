pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 6: Wait For It
--
--  part 1: For how many times can you press the button on each boat
--          to guarantee a record-breaking win in the boat race?
--
--  part 2: whoops! it's just 1 race with extraordinarily bad kerning

with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Day6 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   package EF renames Ada.Numerics.Elementary_Functions;

   --  SECTION
   --  global types and constants

   type Race_Record is record
      Race_Time : Natural;
      Distance  : Natural;
   end record;

   Races : array (1 .. 4) of Race_Record;

   --  SECTION
   --  I/O

   procedure Read_Input is
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      declare
         Pos        : Positive;
         Race_Times : constant String := IO.Get_Line (Input);
         Distances  : constant String := IO.Get_Line (Input);
      begin

         --  get the race times
         Nat_IO.Get
           (Race_Times (10 .. Race_Times'Last), Races (1).Race_Time, Pos);
         Nat_IO.Get
           (Race_Times (Pos + 1 .. Race_Times'Last), Races (2).Race_Time, Pos);
         Nat_IO.Get
           (Race_Times (Pos + 1 .. Race_Times'Last), Races (3).Race_Time, Pos);
         Nat_IO.Get
           (Race_Times (Pos + 1 .. Race_Times'Last), Races (4).Race_Time, Pos);

         --  get the distances
         Nat_IO.Get
           (Distances (10 .. Distances'Last), Races (1).Distance, Pos);
         Nat_IO.Get
           (Distances (Pos + 1 .. Distances'Last), Races (2).Distance, Pos);
         Nat_IO.Get
           (Distances (Pos + 1 .. Distances'Last), Races (3).Distance, Pos);
         Nat_IO.Get
           (Distances (Pos + 1 .. Distances'Last), Races (4).Distance, Pos);

      end;

      IO.Close (Input);

   end Read_Input;

   function Part_1 return Natural is
      --  time for some quadratic formula!
      Result : Natural := 1;
   begin

      for Race of Races loop

         declare
            T_Avail : constant Float := Float (Race.Race_Time);
            D_Beat  : constant Float := Float (Race.Distance);
            Lower   : Float          :=
              Float'Rounding
                ((T_Avail - EF.Sqrt (T_Avail**2 - 4.0 * D_Beat)) / 2.0);
            Upper   : Float          :=
              Float'Rounding
                ((T_Avail + EF.Sqrt (T_Avail**2 - 4.0 * D_Beat)) / 2.0);
         begin

            --  make sure we actually beat the distance
            --  (in both the example and in my input
            --  we'd tie the distance at least once)
            if (T_Avail - Lower) * Lower <= D_Beat then
               Lower := @ + 1.0;
            end if;
            if (T_Avail - Upper) * Upper <= D_Beat then
               Upper := @ - 1.0;
            end if;

            Result := Result * (Natural (Upper) - Natural (Lower) + 1);
         end;

      end loop;

      return Result;

   end Part_1;

   function Part_2 return Natural is
      type Max_Float is digits 18;
      package MEF is new Ada.Numerics.Generic_Elementary_Functions
        (Float_Type => Max_Float);
      T_Avail      : Max_Float := Max_Float (Races (4).Race_Time);
      D_Beat       : Max_Float := Max_Float (Races (4).Distance);
      T_Digits     : Natural   :=
        Natural (Max_Float'Ceiling (MEF.Log (T_Avail, 10.0)));
      D_Digits     : Natural   :=
        Natural (Max_Float'Ceiling (MEF.Log (D_Beat, 10.0)));
      Lower, Upper : Max_Float;
   begin

      for I in reverse 1 .. 3 loop
         --  pretty basic base-10 number construction:
         --  shift left by the needed number of digits, then...
         T_Avail  :=
           @ +
           Max_Float
             (Max_Float (Races (I).Race_Time) * Max_Float (10)**T_Digits);
         D_Beat   :=
           @ +
           Max_Float
             (Max_Float (Races (I).Distance) * Max_Float (10)**D_Digits);
         --  ...determine how many digits the next number must shift
         T_Digits :=
           @ +
           Natural
             (Max_Float'Ceiling
                (MEF.Log (Max_Float (Races (I).Race_Time), 10.0)));
         D_Digits :=
           @ +
           Natural
             (Max_Float'Ceiling
                (MEF.Log (Max_Float (Races (I).Distance), 10.0)));
      end loop;

      --  same formula as part 1 here
      Lower :=
        Max_Float'Rounding
          ((T_Avail - MEF.Sqrt (T_Avail**2 - 4.0 * D_Beat)) / 2.0);
      Upper :=
        Max_Float'Rounding
          ((T_Avail + MEF.Sqrt (T_Avail**2 - 4.0 * D_Beat)) / 2.0);
      if (T_Avail - Lower) * Lower <= D_Beat then
         Lower := @ + 1.0;
      end if;
      if (T_Avail - Upper) * Upper <= D_Beat then
         Upper := @ - 1.0;
      end if;

      return Natural (Upper) - Natural (Lower) + 1;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("You can beat the record in" & Part_1'Image & " ways.");
   IO.Put_Line ("No, it's actually" & Part_2'Image & " ways.");
end Day6;
