pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 19: Aplenty
--
--  part 1: sum the "rating numbers" of the parts that survive the rules
--
--  part 2: how many products can make it through the rules?

with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day19 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Num => Natural);

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  parts, attributes, labels

   type Attribute is (Cool, Music, Aero, Shine);
   type Part is array (Attribute) of Natural;
   type Label is new String (1 .. 3);
   --  this is easier than dealing with indefinite types...

   Accepted : constant Label := "A  ";
   Rejected : constant Label := "R  ";

   --  SUBSECTION
   --  criteria and rules

   type Test_Type is (Smaller, Larger);

   type Criterion is record
      Attr  : Attribute;
      Test  : Test_Type;
      Value : Natural;
      Dest  : Label;
   end record;
   --  this means, "Attr Test Value : Dest"
   --  so the criterion `a<2006:q` becomes
   --  (Attr => Aero, Test => Smaller, Value => 2006, Dest : "q  ")

   package Criterion_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Criterion);

   type Rule is record
      Criteria : Criterion_Vectors.Vector;
      Default  : Label;
   end record;

   package Rule_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Label, Element_Type => Rule);
   package Part_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Part);

   All_Rules : Rule_Maps.Map;
   All_Parts : Part_Vectors.Vector;

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  Input

   procedure Get_Label (S : String; L : out Label; First : in out Positive) is
      Last : Positive := First;
   begin

      L := [others => ' '];

      while Last < S'Last
        and then
        (S (Last) in 'a' .. 'z' or else S (Last) = 'A' or else S (Last) = 'R')
      loop
         L (Last - First + 1) := S (Last);
         Last                 := @ + 1;
      end loop;

      First := Last - 1;

   end Get_Label;

   procedure Get_Natural      --  grumble grumble ":" grumble grumble
     (S : String; Value : out Natural; Pos : in out Positive)
   is
   begin

      Value := 0;

      while Pos <= S'Last and then S (Pos) in '0' .. '9' loop
         Value := @ * 10 + Character'Pos (S (Pos)) - Character'Pos ('0');
         Pos   := @ + 1;
      end loop;

      Pos := @ - 1;

   end Get_Natural;

   procedure Get_Part (S : String; P : out Part) is
      Pos : Positive := 1;
   begin

      Nat_IO.Get (S (S'First + 3 .. S'Last), P (Cool), Pos);
      Nat_IO.Get (S (S'First + 3 + Pos .. S'Last), P (Music), Pos);
      Nat_IO.Get (S (S'First + 3 + Pos .. S'Last), P (Aero), Pos);
      Nat_IO.Get (S (S'First + 3 + Pos .. S'Last), P (Shine), Pos);

   end Get_Part;

   procedure Get_Criterion
     (S : String; C : out Criterion; First : in out Positive)
   is

      Last : Positive := First;

      Attr      : Attribute;
      New_Label : Label := [others => ' '];
      Test      : Test_Type;
      Limit     : Natural;

      Invalid_Attribute : exception;

   begin

      Attr :=
        (case S (Last) is when 'a' => Aero, when 'm' => Music,
           when 's' => Shine, when 'x' => Cool,
           when others => raise Invalid_Attribute);
      Test := (if S (Last + 1) = '<' then Smaller else Larger);
      Last := @ + 2;

      Get_Natural (S (Last .. S'Last), Limit, Last);
      Last := @ + 2;

      Get_Label (S, New_Label, Last);
      First := Last + 2;

      C.Attr  := Attr;
      C.Dest  := New_Label;
      C.Test  := Test;
      C.Value := Limit;

   end Get_Criterion;

   procedure Read_Input is
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      --  get the rules
      loop
         declare
            S   : constant String := IO.Get_Line (Input);
            Pos : Positive        := 1;
            L   : Label           := [others => ' '];
            R   : Rule;
         begin

            if S'Length = 0 then
               exit;
            end if;

            Get_Label (S, L, Pos);

            --  get criteria
            while S (Pos) /= '}' loop

               Pos := @ + 2;

               while (for some I in Pos .. S'Last => S (I) = ',') loop
                  declare
                     Crit : Criterion;
                  begin
                     Get_Criterion (S (Pos .. S'Last), Crit, Pos);
                     R.Criteria.Append (Crit);
                  end;
               end loop;

               Get_Label (S (Pos .. S'Last), R.Default, Pos);
               Pos := @ + 1;

            end loop;

            All_Rules.Insert (L, R);

         end;
      end loop;

      while not IO.End_Of_File (Input) loop
         declare
            S : constant String := IO.Get_Line (Input);
            P : Part;
         begin
            Get_Part (S, P);
            All_Parts.Append (P);
         end;
      end loop;

   end Read_Input;

   --  SUBSECTION
   --  Output

   procedure Put_Part (P : Part) is
   begin
      for A in Attribute loop
         IO.Put (A'Image);
         IO.Put (":");
         IO.Put (P (A)'Image & "; ");
      end loop;
   end Put_Part;

   procedure Put_Criterion (C : Criterion) is
   begin
      IO.Put (C.Attr'Image);
      if C.Test = Smaller then
         IO.Put (" < ");
      else
         IO.Put (" > ");
      end if;
      IO.Put (C.Value'Image & ":" & String (C.Dest));
   end Put_Criterion;

   procedure Put_Rule (R : Rule) is
   begin
      IO.Put ("[ ");
      for C of R.Criteria loop
         Put_Criterion (C);
         IO.Put (" ");
      end loop;
      IO.Put ("; " & String (R.Default) & " ]");
   end Put_Rule;

   --  SECTION
   --  Part 1

   function Apply_Rule (P : Part; L0 : Label) return Label is
      L          : Label   := L0;
      R          : Rule;
      Found_Rule : Boolean := False;
      --  if we don't find a rule, we need to assign the default
   begin

      Found_Rule := False;
      R          := All_Rules (L);

      for C of R.Criteria loop

         if (C.Test = Smaller and then P (C.Attr) < C.Value)
           or else (C.Test = Larger and then P (C.Attr) > C.Value)
         then
            Found_Rule := True;
            L          := C.Dest;
            exit;
         end if;

      end loop;

      if not Found_Rule then
         L := R.Default;
      end if;

      return L;

   end Apply_Rule;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin

      for P of All_Parts loop
         declare
            L : Label := "in ";
         begin

            while L /= Accepted and then L /= Rejected loop
               L := Apply_Rule (P, L);
               if L = Accepted then
                  Result := @ + P (Cool) + P (Music) + P (Aero) + P (Shine);
               end if;
            end loop;

         end;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   --  SUBSECTION
   --  types and variables particular to Part 2

   type Ludicrous_Size is range 0 .. 2**64 - 1;
   --  ;-)

   type Attribute_Array is array (Attribute) of Natural;

   type Trace is record
      Location   : Label;
      Mins, Maxs : Attribute_Array;
   end record;
   --  a Trace records where parts within the given intervals might end up
   --  where an interval is defined by Min (A) .. Max (A) for each attribute A

   package Trace_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Trace);

   package Trace_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Trace_Interface);

   --  SUBSECTION
   --  more Output

   procedure Put_Trace (T : Trace) is
   begin
      IO.Put ("'" & String (T.Location) & "':");
      for A in Attribute loop
         IO.Put (" [" & T.Mins (A)'Image & "," & T.Maxs (A)'Image & "]");
      end loop;
   end Put_Trace;

   function Part_2 return Ludicrous_Size is

      Result, Product : Ludicrous_Size := 0;

      Traces          : Trace_Queues.Queue;
      T, TLess, TMore : Trace;
      --  TLess, TMore used in a split

      R : Rule;

      Found : Boolean;
      --  whether a Criterion applied to a dequeued T

   begin

      Traces.Enqueue
        (Trace'
           (Location => "in ", Mins => [others => 1],
            Maxs     => [others => 4_000]));

      while Natural (Traces.Current_Use) > 0 loop

         --  IO.Put_Line ("queue has" & Traces.Current_Use'Image);
         --  i never get above the low 300's! :-)

         Traces.Dequeue (T);

         if T.Location = Accepted then
            Product := 1;
            for A in Attribute loop
               Product := @ * Ludicrous_Size ((T.Maxs (A) - T.Mins (A) + 1));
            end loop;
            Found  := True;
            Result := @ + Product;

         elsif T.Location = Rejected then
            Found := True;

         else

            R     := All_Rules (T.Location);
            Found := False;

            for C of R.Criteria loop

               if C.Test = Smaller then

                  if T.Maxs (C.Attr) < C.Value then
                     --  all values satisfy; pass to Dest and requeue
                     if C.Dest /= Rejected then
                        T.Location := C.Dest;
                        Traces.Enqueue (T);
                     end if;
                     Found := True;

                  elsif T.Mins (C.Attr) >= C.Value then
                     --  no values satisfy; pass to next rule
                     null;

                  else
                     --  split into two new rules
                     --  the one that satisfies is sent to the new destination
                     TMore               := T;
                     TMore.Mins (C.Attr) := C.Value;
                     Traces.Enqueue (TMore);
                     if C.Dest /= Rejected then
                        TLess               := T;
                        TLess.Maxs (C.Attr) := C.Value - 1;
                        TLess.Location      := C.Dest;
                        Traces.Enqueue (TLess);
                     end if;
                     Found := True;

                  end if;

               else  --  C.Test = Larger

                  if T.Mins (C.Attr) > C.Value then
                     --  all values satisfy; pass to Dest and requeue
                     if C.Dest /= Rejected then
                        T.Location := C.Dest;
                        Traces.Enqueue (T);
                     end if;
                     Found := True;

                  elsif T.Maxs (C.Attr) <= C.Value then
                     --  passes through to next rule
                     null;

                  else
                     --  split into two new rules
                     --  the one that satisfies is sent to the new destination
                     TLess               := T;
                     TLess.Maxs (C.Attr) := C.Value;
                     Traces.Enqueue (TLess);
                     if C.Dest /= Rejected then
                        TMore               := T;
                        TMore.Mins (C.Attr) := C.Value + 1;
                        TMore.Location      := C.Dest;
                        Traces.Enqueue (TMore);
                     end if;
                     Found := True;

                  end if;
               end if;

               if Found then
                  exit;
               end if;

            end loop;

            if not Found then
               T.Location := R.Default;
               Traces.Enqueue (T);
            end if;

         end if;
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("Sum of accepted parts' ratings is" & Part_1'Image);
   IO.Put_Line ("Number of acceptable parts is" & Part_2'Image);
end Day19;
