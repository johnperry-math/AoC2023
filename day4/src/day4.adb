pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 4: Scratchcards
--
--  part 1: determine the total points won on each scratchcard
--
--  part 2: whoops! points are inappropriate; rather, you win copies of cards
--          and get to play them, too. how many cards do you have at the end?

with Ada.Text_IO;

procedure Day4 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Num => Natural);

   --  SECTION
   --  global types and variables

   subtype Number_Range is Natural range 0 .. 99;
   --  the numbers that can appear on a card (though 0 doesn't seem to appear)

   type Number_Set is array (Number_Range) of Boolean;

   type Card_Setup is record
      On_Card, You_Have : Number_Set := [others => False];
   end record;
   --  On_Card (Ith) is true if the Ith number appears on the card
   --  You_Have (Ith) is true if you have the Ith number

   subtype Card_Range is Positive range 1 .. 201;
   --  the number of cards

   Cards : array (Card_Range) of Card_Setup;

   --  SECTION
   --  I/O

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      for Card in Cards'Range loop

         declare
            S     : constant String := IO.Get_Line (F);
            Idx   : Positive        := 11;
            Value : Natural;
         begin

            --  get values on the card
            for Each in 1 .. 10 loop
               Nat_IO.Get (S (Idx .. S'Last), Value, Idx);
               Idx                          := @ + 2;
               Cards (Card).On_Card (Value) := True;
            end loop;

            Idx := 43;

            --  get values you have
            for Each in 1 .. 25 loop
               Nat_IO.Get (S (Idx .. S'Last), Value, Idx);
               Idx                           := @ + 2;
               Cards (Card).You_Have (Value) := True;
            end loop;

         end;

      end loop;

   end Read_Input;

   procedure Put_Games is
   --  useful for debugging
   begin

      for Card in Cards'Range loop

         IO.Put ("Card  ");
         Nat_IO.Put (Card, 2);
         IO.Put (": ");

         for Ith in Number_Range loop
            if Cards (Card).On_Card (Ith) then
               Nat_IO.Put (Ith, 2);
               IO.Put (' ');
            end if;
         end loop;

         IO.Put ("| ");

         for Ith in Number_Range loop
            if Cards (Card).You_Have (Ith) then
               Nat_IO.Put (Ith, 2);
               IO.Put (' ');
            end if;
         end loop;

         IO.New_Line;

      end loop;

   end Put_Games;

   --  Parts 1 and 2

   function Matches (Card : Card_Setup) return Natural is
   --  how many numbers on the card match the numbers you have

      Result : Natural := 0;

   begin

      for Ith in Number_Range loop
         if Card.On_Card (Ith) and then Card.You_Have (Ith) then
            Result := @ + 1;
         end if;
      end loop;

      return Result;

   end Matches;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin
      for Card of Cards loop
         Result := @ + 2**(Matches (Card) - 1);
      end loop;
      return Result;
   end Part_1;

   function Part_2 return Natural is
      Copies : array (Card_Range) of Natural := [others => 1];
      Number : Natural;
   begin
      for Ith in Card_Range loop
         Number := Matches (Cards (Ith));
         IO.Put_Line (Ith'Image & " matches" & Number'Image);
         IO.Put_Line ("There are" & Copies (Ith)'Image & " copies");
         for Offset in 1 .. Number loop
            IO.Put_Line ("Card" & Natural'Image (Ith + Offset));
            Copies (Ith + Offset) := @ + Copies (Ith);
         end loop;
      end loop;
      return Copies'Reduce ("+", 0);
   end Part_2;

begin
   Read_Input;
   Put_Games;
   IO.Put_Line ("The cards are worth" & Part_1'Image & " points");
   IO.Put_Line ("You end up with" & Part_2'Image & " cards");
end Day4;
