pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 14: Lens Library
--
--  part 1: compute the sum of results of HASH algorithm on each sequence entry
--
--  part 2: use the algorithm to insert and remove lenses in a sequence of
--          boxes, sum the focal power when done

with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

procedure Day15 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  I/O

   function Read_Input return String is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      declare
         S : constant String := IO.Get_Line (Input);
      begin
         return S;
      end;
   end Read_Input;

   Initialization : constant String := Read_Input;
   --  how ya like that ;-)

   --  SECTION
   --  HASH algorithm

   subtype Hash_Value is Natural range 0 .. 255;

   function Hash (S : String) return Hash_Value is
      Result : Hash_Value := 0;
   begin

      for Each of S when Each /= ' ' loop
         Result := ((@ + Character'Pos (Each)) * 17) mod 256;
      end loop;

      return Result;

   end Hash;

   --  SECTION
   --  Part 1

   function Part_1 return Natural is
      Result      : Natural  := 0;
      Left, Right : Positive := Initialization'First;
   begin

      while Right <= Initialization'Last loop

         while Right + 1 <= Initialization'Last
           and then Initialization (Right + 1) /= ','
         loop
            Right := @ + 1;
         end loop;

         Result := @ + Hash (Initialization (Left .. Right));
         Right  := @ + 2;
         Left   := Right;

      end loop;

      return Result;
   end Part_1;

   --  SECTION
   --  Part 2

   --  SUBSECTION
   --  lens labeling and boxes

   subtype Digit is Positive range 1 .. 9;
   package Digit_IO is new IO.Integer_IO (Num => Digit);

   subtype Lens is String;

   type Labeled_Lens (Length : Positive) is record
      Label        : Lens (1 .. Length);
      Focal_Length : Digit;
   end record;

   --  despite the puzzle's highly suggestive use of the word HASHMAP,
   --  a proper hash map does not strike me as giving much benefit
   --  over an array / vector IN THIS CASE;
   --  since vectors work fine, I'm using that

   package Box_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Labeled_Lens);

   Boxes : array (Hash_Value) of Box_Vectors.Vector;

   function Find_Label (L : Lens; My_Box : Box_Vectors.Vector) return Positive
      --  ARGH Ada.Find_Index compares the entire lens, whereas
      --  I need to find only the label

   is
      Position : Positive := My_Box.First_Index;
   begin

      while Position <= My_Box.Last_Index and then My_Box (Position).Label /= L
      loop
         Position := @ + 1;
      end loop;

      return Position;
   end Find_Label;

   function Focusing_Power
     (L : Labeled_Lens; Slot_Number : Natural) return Natural is
     ((1 + Hash (L.Label)) * Slot_Number * L.Focal_Length);

   function Part_2 (S : String) return Natural is
      Result                  : Natural  := 0;
      Label_Left, Label_Right : Positive := S'First;
   begin

      while Label_Right <= S'Last loop

         --  get the label
         while S (Label_Right + 1) in 'a' .. 'z' loop
            Label_Right := @ + 1;
         end loop;

         declare

            L : Lens := S (Label_Left .. Label_Right);

            Box : constant Hash_Value := Hash (L);
            My_Box renames Boxes (Box);

            Position : Positive;

         begin

            if S (Label_Right + 1) = '-' then         --  remove from box

               Position := Find_Label (L, My_Box);
               if Position <= My_Box.Last_Index then
                  My_Box.Delete (Position, 1);
               end if;

               Label_Right := @ + 3;

            else                                      --  add to box

               if S (Label_Right + 1) /= '=' then     --  error; doesn't occur
                  IO.Put_Line
                    ("uh-oh;" & Label_Right'Image & " is unexpected: " &
                     S (Label_Left .. Label_Right + 1));
               end if;

               declare
                  Focal_Length : Digit;
                  Pos          : Positive;
               begin

                  Digit_IO.Get
                    (S (Label_Right + 2 .. Label_Right + 2), Focal_Length,
                     Pos);

                  Position := Find_Label (L, My_Box);
                  if Position <= My_Box.Last_Index then
                     --  it already has it, so change focal length
                     My_Box (Position).Focal_Length := Focal_Length;
                  else
                     --  it doesn't have it, so add to end
                     My_Box.Append
                       (Labeled_Lens'
                          (Length => Label_Right - Label_Left + 1, Label => L,
                           Focal_Length => Focal_Length));
                  end if;

               end;

               Label_Right := @ + 4;

            end if;
         end;

         Label_Left := Label_Right;

      end loop;

      for Box of Boxes loop
         for Ith in Box.First_Index .. Box.Last_Index loop
            Result := @ + Focusing_Power (Box (Ith), Ith);
         end loop;
      end loop;

      return Result;

   end Part_2;

begin
   IO.Put_Line ("Sum of hash results is" & Part_1'Image);
   --  IO.Put_Line
   --    ("Focusing power is" &
   --     Part_2 ("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")'Image);
   IO.Put_Line ("Focusing power is" & Part_2 (Initialization)'Image);
end Day15;
