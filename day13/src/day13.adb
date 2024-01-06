pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 13: Point of Incidence
--
--  part 1: compute a number depending on the single row or column of symmetry
--          in each matrix
--
--  part 2: compute the same number _after_ "correcting" by changing the single
--          entry that gives you a different row or column of symmetry in each
--          matrix

with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

procedure Day13 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   type Object is (Ash, Rock);

   type Map is array (Positive range <>, Positive range <>) of Object;

   package Map_Vecs is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Map);

   All_Maps : Map_Vecs.Vector;

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  output, useful for debugging (and boy oh boy was it)

   function Repr (O : Object) return Character is
     (if O = Ash then '.' else '#');

   procedure Put_Map (M : Map) is
   begin

      for Row in M'Range (1) loop
         for Col in M'Range (2) loop
            IO.Put (Repr (M (Row, Col)));
         end loop;
         IO.New_Line;
      end loop;

   end Put_Map;

   package Map_Input_Vecs is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);
   Map_Input : Map_Input_Vecs.Vector;

   procedure Finalize (Map_Input : Map_Input_Vecs.Vector) is
   --  turns Map_Input into a bona fide Map and appends it to All_Maps
   begin

      declare
         Rows : constant Positive := Positive (Map_Input.Length);
         Cols : constant Positive := Map_Input.First_Element'Length;
         M    : Map (1 .. Rows, 1 .. Cols);
      begin

         for Row in 1 .. Rows loop
            for Col in 1 .. Cols loop
               M (Row, Col) :=
                 (if Map_Input (Row) (Col) = '.' then Ash else Rock);
            end loop;
         end loop;

         All_Maps.Append (M);

      end;

   end Finalize;

   procedure Read_Input is
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      while not IO.End_Of_File (Input) loop

         declare
            S : constant String := IO.Get_Line (Input);
         begin

            if S'Length > 0 then
               Map_Input.Append (S);

            else
               Finalize (Map_Input);
               Map_Input.Clear;

            end if;
         end;

      end loop;

      if not Map_Input.Is_Empty then
         Finalize (Map_Input);
      end if;

      IO.Close (Input);

   end Read_Input;

   --  SECTION
   --  Part 1

   type Axis_Of_Symmetry (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Value : Positive;
         when False =>
            null;
      end case;
   end record;

   function Detect_Horizontal_Axis (M : Map) return Axis_Of_Symmetry is
   begin

      for Row in 2 .. M'Last (1) loop

         if
           (for all Offset in 1 .. Natural'Min (Row - 1, M'Last (1) - Row + 1)
            =>
              (for all Col in 1 .. M'Last (2) =>
                 M (Row - Offset, Col) = M (Row + Offset - 1, Col)))
         then
            return Axis_Of_Symmetry'(Valid => True, Value => Row - 1);
         end if;

      end loop;

      return Axis_Of_Symmetry'(Valid => False);

   end Detect_Horizontal_Axis;

   function Detect_Vertical_Axis (M : Map) return Axis_Of_Symmetry is
   begin

      for Col in 2 .. M'Last (2) loop
         if
           (for all Offset in 1 .. Natural'Min (Col - 1, M'Last (2) - Col + 1)
            =>

              (for all Row in 1 .. M'Last (1) =>
                 M (Row, Col - Offset) = M (Row, Col + Offset - 1)))
         then
            return Axis_Of_Symmetry'(Valid => True, Value => Col - 1);
         end if;

      end loop;

      return Axis_Of_Symmetry'(Valid => False);

   end Detect_Vertical_Axis;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin

      for M of All_Maps loop

         declare
            Maybe_Axis : constant Axis_Of_Symmetry :=
              Detect_Horizontal_Axis (M);
         begin
            if Maybe_Axis.Valid then
               Result := @ + 100 * Maybe_Axis.Value;
            end if;
         end;

         declare
            Maybe_Axis : constant Axis_Of_Symmetry := Detect_Vertical_Axis (M);
         begin
            if Maybe_Axis.Valid then
               Result := @ + Maybe_Axis.Value;
            end if;
         end;

      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   type Inconsistency_Tracker is record
      Inconsistencies : Natural;
      Row             : Positive;
   end record;

   function Find_Horizontal_Axis (M : Map) return Axis_Of_Symmetry is

      function Count_Inconsistencies
        (Accumulator : Inconsistency_Tracker; Offset : Natural)
         return Inconsistency_Tracker
      is

         function Add_When_Different
           (Accumulator : Inconsistency_Tracker; Col : Natural)
            return Inconsistency_Tracker is
           ((Row             => Accumulator.Row,
             Inconsistencies =>
               Accumulator.Inconsistencies +
               (if
                  M (Accumulator.Row - Offset, Col) =
                  M (Accumulator.Row + Offset - 1, Col)
                then 0
                else 1)));

         Inconsistencies : constant Inconsistency_Tracker
            := [for Col in M'Range (2) => Col]'Reduce
               (Add_When_Different, Accumulator);

      begin
         return Inconsistencies;
      end Count_Inconsistencies;

   begin

      for Row in 2 .. M'Last (1) loop

         declare
            Reduction : constant Inconsistency_Tracker
               := [for Offset in 1 .. Natural'Min
                        (Row - 1, M'Last (1) - Row + 1) => Offset]'Reduce
                     (Count_Inconsistencies,
                        (Inconsistencies => 0, Row => Row));
         begin

            if Reduction.Inconsistencies = 1 then
               return Axis_Of_Symmetry'(Valid => True, Value => Row - 1);
            end if;

         end;
      end loop;

      return Axis_Of_Symmetry'(Valid => False);

   end Find_Horizontal_Axis;

   function Find_Vertical_Axis (M : Map) return Axis_Of_Symmetry is
   begin

      for Col in 2 .. M'Last (2) loop

         declare
            Result                    : Axis_Of_Symmetry;
            Number_Of_Inconsistencies : Natural := 0;
         begin

            for Offset in
              1 ..
                Natural'Min
                  (Col - 1,
                   M'Last (2) - Col + 1) when Number_Of_Inconsistencies <=
            1
            loop
               for Row in 1 .. M'Last (1) loop

                  if M (Row, Col - Offset) /= M (Row, Col + Offset - 1) then
                     Number_Of_Inconsistencies := @ + 1;
                     if Number_Of_Inconsistencies = 1 then
                        --  we have at least one location that does not reflect
                        Result := (Valid => True, Value => Col - 1);
                     else
                        --  alas, we have two locations that do not reflect
                        exit;
                     end if;
                  end if;

               end loop;
            end loop;

            if Number_Of_Inconsistencies = 1 then
               return Result;
            end if;

         end;
      end loop;

      return Axis_Of_Symmetry'(Valid => False);

   end Find_Vertical_Axis;

   function Part_2 return Natural is
      Result     : Natural := 0;
      Found_Axis : Boolean;
   begin

      for M of All_Maps loop

         Found_Axis := False;

         declare
            Maybe_Movable : constant Axis_Of_Symmetry :=
              Find_Horizontal_Axis (M);
         begin
            if Maybe_Movable.Valid then
               Found_Axis := True;
               Result     := @ + 100 * Maybe_Movable.Value;
            end if;
         end;

         declare
            Maybe_Movable : constant Axis_Of_Symmetry :=
              Find_Vertical_Axis (M);
         begin
            if Maybe_Movable.Valid then
               Found_Axis := True;
               Result     := @ + Maybe_Movable.Value;
            end if;
         end;

         if not Found_Axis then
            --  this would be a problem
            IO.Put_Line ("Did not find axis for");
            Put_Map (M);
            IO.New_Line;
         end if;

      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("Summary of notes is" & Part_1'Image);
   IO.Put_Line ("After cleaning smudges, summary is" & Part_2'Image);
end Day13;
