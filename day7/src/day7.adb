pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 7: Camel Cards
--
--  part 1: rank the hands of cards to determine their total winnings
--
--  part 2: repeat, under slightly new rules

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day7 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   --  SECTION
   --  global types and variables

   type Face is
     (One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen,
      King, Ace);

   type Five_Faces is array (1 .. 5) of Face;

   type Hand is record
      Cards : Five_Faces;
      Bid   : Natural;
   end record;

   package All_Hands_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hand);

   All_Hands : All_Hands_Vectors.Vector;

   type Hand_Type is
     (High_Card, One_Pair, Two_Pair, Three_Of_A_Kind, Full_House,
      Four_Of_A_Kind, Five_Of_A_Kind);

   --  SECTION
   --  I/O

   Invalid_Card : exception;

   procedure Read_Input is
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      while not IO.End_Of_File (Input) loop
         declare
            S   : constant String := IO.Get_Line (Input);
            H   : Hand;
            Pos : Positive;
         begin

            Nat_IO.Get (S (7 .. S'Last), H.Bid, Pos);

            for Pos in 1 .. 5 loop
               H.Cards (Pos) :=
                 (case S (Pos) is when '1' => One, when '2' => Two,
                    when '3' => Three, when '4' => Four, when '5' => Five,
                    when '6' => Six, when '7' => Seven, when '8' => Eight,
                    when '9' => Nine, when 'T' => Ten, when 'J' => Jack,
                    when 'Q' => Queen, when 'K' => King, when 'A' => Ace,
                    when others => raise Invalid_Card with S (Pos)'Image);
            end loop;

            All_Hands.Append (H);

         end;
      end loop;

      IO.Close (Input);

   end Read_Input;

   procedure Put_Hand (H : Hand) is
   --  useful for debugging
   begin
      for I in Five_Faces'Range loop
         case H.Cards (I) is
            when One =>
               IO.Put ('1');
            when Two =>
               IO.Put ('2');
            when Three =>
               IO.Put ('3');
            when Four =>
               IO.Put ('4');
            when Five =>
               IO.Put ('5');
            when Six =>
               IO.Put ('6');
            when Seven =>
               IO.Put ('7');
            when Eight =>
               IO.Put ('8');
            when Nine =>
               IO.Put ('9');
            when Ten =>
               IO.Put ('T');
            when Jack =>
               IO.Put ('J');
            when Queen =>
               IO.Put ('Q');
            when King =>
               IO.Put ('K');
            when Ace =>
               IO.Put ('A');
         end case;
      end loop;
      IO.Put (' ');
      Nat_IO.Put (H.Bid, 0);
   end Put_Hand;

   --  SECTION
   --  Part 1

   function Rank (H : Hand) return Hand_Type is
      --  ranks the hand by counting the number of identical faces,
      --  taking care not to overwrite 3's by 2's or vice-versa

      Counts : array (Face) of Natural := [others => 0];
      Most_Identical, Second_Most_Identical : Positive range 1 .. 5   := 1;
      --  highest and second-highest frequency of cards

   begin

      --  count each card first
      for C of H.Cards loop
         Counts (C) := @ + 1;
      end loop;

      --  check the counts to determine the frequencies
      for C of Counts loop

         case C is
            when 5 =>
               Most_Identical := 5;

            when 4 =>
               Most_Identical := 4;

            when 3 =>
               if Most_Identical = 2 then
                  Second_Most_Identical := 2;
               end if;
               Most_Identical := 3;

            when 2 =>
               if Most_Identical < 2 then
                  Most_Identical := 2;
               else
                  Second_Most_Identical := 2;
               end if;

            when others =>
               null;

         end case;

      end loop;

      --  finally! identify the ranks
      return
        (case Most_Identical is when 1 => High_Card,
           when 2 =>
             (if Second_Most_Identical = 1 then One_Pair else Two_Pair),
           when 3 =>
             (if Second_Most_Identical = 1 then Three_Of_A_Kind
              else Full_House),
           when 4 => Four_Of_A_Kind, when 5 => Five_Of_A_Kind);

   end Rank;

   function "<" (Left, Right : Hand) return Boolean is
      --  Ada's default behavior on enumerated types make this VERY easy

      Left_Rank, Right_Rank : Hand_Type;

   begin

      Left_Rank  := Rank (Left);
      Right_Rank := Rank (Right);

      if Left_Rank < Right_Rank then
         return True;

      elsif Left_Rank > Right_Rank then
         return False;

      else

         for Ith in Five_Faces'Range loop

            if Left.Cards (Ith) < Right.Cards (Ith) then
               return True;

            elsif Left.Cards (Ith) > Right.Cards (Ith) then
               return False;

            end if;
         end loop;
      end if;

      return False;

   end "<";

   package Hand_Sorter is new All_Hands_Vectors.Generic_Sorting;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin

      Hand_Sorter.Sort (All_Hands);

      for Ith in All_Hands.First_Index .. All_Hands.Last_Index loop
         Result := @ + Ith * All_Hands (Ith).Bid;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   function Rank_With_Joker (H : Hand) return Hand_Type is
      --  essentially the same as Rank, except that
      --  * we count the jokers in a separate variable,
      --    rather than in the main array
      --  * we then loop through the main array, determining
      --    how the joker would best help the hand

      Counts : array (Face) of Natural := [others => 0];
      Most_Identical, Second_Most_Identical : Positive range 1 .. 5   := 1;
      Num_Jokers                            : Natural                 := 0;

   begin

      --  count all cards, jack separately
      for C of H.Cards loop
         if C = Jack then
            Num_Jokers := @ + 1;
         else
            Counts (C) := @ + 1;
         end if;
      end loop;

      --  determine the frequencies
      for C of Counts loop

         case C is

            when 5 =>
               Most_Identical := 5;

            when 4 =>
               Most_Identical := 4;

            when 3 =>
               if Most_Identical = 2 then
                  Second_Most_Identical := 2;
               end if;
               Most_Identical := 3;

            when 2 =>
               if Most_Identical < 2 then
                  Most_Identical := 2;
               else
                  Second_Most_Identical := 2;
               end if;

            when others =>
               null;

         end case;
      end loop;

      --  apply the joking jack
      if Num_Jokers > 0 then

         case Num_Jokers is

            when 5 | 4 =>
               Most_Identical := 5;

            when 3 =>
               if Most_Identical = 2 then
                  Most_Identical := 5;
               else
                  Most_Identical := 4;
               end if;

            when 2 =>
               if Most_Identical = 3 then
                  Most_Identical := 5;
               elsif Most_Identical = 2 then
                  Most_Identical := 4;
               else
                  Most_Identical := 3;
               end if;

            when others =>    --  1
               Most_Identical := @ + 1;

         end case;
      end if;

      --  finally! identify the ranks as before
      return
        (case Most_Identical is when 1 => High_Card,
           when 2 =>
             (if Second_Most_Identical = 1 then One_Pair else Two_Pair),
           when 3 =>
             (if Second_Most_Identical = 1 then Three_Of_A_Kind
              else Full_House),
           when 4 => Four_Of_A_Kind, when 5 => Five_Of_A_Kind);

   end Rank_With_Joker;

   New_Ranking : constant array (Face) of Natural :=
     [Jack => 0, One => 1, Two => 2, Three => 3, Four => 4, Five => 5,
     Six   => 6, Seven => 7, Eight => 8, Nine => 9, Ten => 10, Queen => 11,
     King  => 12, Ace => 13];
   --  used to break ties in ranking; remaps each variant of the enumeration
   --  to a number that will order it correctly

   function Alternate_Order (Left, Right : Hand) return Boolean is
      Left_Rank, Right_Rank : Hand_Type;
   begin

      Left_Rank  := Rank_With_Joker (Left);
      Right_Rank := Rank_With_Joker (Right);

      if Left_Rank < Right_Rank then
         return True;

      elsif Left_Rank > Right_Rank then
         return False;

      else
         for Ith in Five_Faces'Range loop
            --  New_Ranking re-ordering the cards! :-)
            if New_Ranking (Left.Cards (Ith)) < New_Ranking (Right.Cards (Ith))
            then
               return True;
            elsif New_Ranking (Left.Cards (Ith)) >
              New_Ranking (Right.Cards (Ith))
            then
               return False;
            end if;
         end loop;
      end if;

      return False;

   end Alternate_Order;

   package Alternate_Sorter is new All_Hands_Vectors.Generic_Sorting
     ("<" => Alternate_Order);

   function Part_2 return Natural is
      Result : Natural := 0;
   begin

      Alternate_Sorter.Sort (All_Hands);

      for Ith in All_Hands.First_Index .. All_Hands.Last_Index loop
         Result := @ + Ith * All_Hands (Ith).Bid;
      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("the hands' total winnings are" & Part_1'Image);
   IO.Put_Line
     ("with the new rule, the hands' total winnings are" & Part_2'Image);
end Day7;
