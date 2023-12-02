pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
use all type Ada.Containers.Count_Type;

procedure Day2 is

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   type Colors is (Blue, Green, Red);

   package Color_IO is new IO.Enumeration_IO (Enum => Colors);

   type Hand is array (Colors) of Natural;

   Bounds : constant Hand := [Blue => 14, Green => 13, Red => 12];

   package Hand_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hand);

   function "=" (Left, Right : Hand_Vectors.Vector) return Boolean is
     (Left.Length = Right.Length
      and then
      (for all I in 1 .. Positive (Left.Length) => Left (I) = Right (I)));

   package Game_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hand_Vectors.Vector);

   Games : Game_Vectors.Vector;

   procedure Read_IO is
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, "input.txt");
      while not IO.End_Of_File (F) loop
         declare
            S    : constant String := IO.Get_Line (F);
            Idx  : Positive        := 7;
            Game : Hand_Vectors.Vector;
         begin
            while S (Idx) /= ':' loop
               Idx := @ + 1;
            end loop;
            Idx := @ + 2;
            while Idx < S'Last loop
               declare
                  New_Hand : Hand := [others => 0];
                  Number   : Positive;
                  Color    : Colors;
               begin
                  while Idx < S'Last and then S (Idx) /= ';' loop
                     Positive_IO.Get (S (Idx .. S'Last), Number, Idx);
                     Idx := @ + 2;
                     Color_IO.Get (S (Idx .. S'Last), Color, Idx);
                     Idx              := @ + 1;
                     New_Hand (Color) := Number;
                     if Idx < S'Last and then S (Idx) /= ';' then
                        Idx := @ + 2;
                     end if;
                  end loop;
                  Game.Append (New_Hand);
               end;
               Idx := @ + 2;
            end loop;
            Games.Append (Game);
         end;
      end loop;
   end Read_IO;

   procedure Put_Game (G : Hand_Vectors.Vector) is
   begin
      for H of G loop
         for C in Colors loop
            if H (C) > 0 then
               IO.Put (H (C)'Image & ' ' & C'Image);
            end if;
         end loop;
         IO.Put (';');
      end loop;
   end Put_Game;

   procedure Put_Games is
   begin
      for G of Games loop
         Put_Game (G);
         IO.New_Line;
      end loop;
   end Put_Games;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin
      for I in Games.First_Index .. Games.Last_Index loop
         --  Positive_IO.Put (I, 3);
         --  IO.Put (' ');
         declare
            G      : Hand_Vectors.Vector renames Games (I);
            Passes : Boolean := True;
         begin
            for H of G when Passes loop
               for C in Colors when Passes loop
                  Passes := @ and then H (C) <= Bounds (C);
               end loop;
            end loop;
            --  IO.Put ((if Passes then "V " else "X "));
            --  Put_Game (G);
            --  IO.New_Line;
            if Passes then
               Result := @ + I;
            end if;
         end;
      end loop;
      return Result;
   end Part_1;

   function Power (H : Hand) return Natural is
     (H (Blue) * H (Green) * H (Red));

   function Minimum_Playable (G : Hand_Vectors.Vector) return Hand is
      Result : Hand := [others => 0];
   begin
      for H of G loop
         for C in Colors loop
            Result (C) := Natural'Max (@, H (C));
         end loop;
      end loop;
      return Result;
   end Minimum_Playable;

   function Part_2 return Natural is
      Result : Natural := 0;
   begin
      for G of Games loop
         Result := @ + Power (Minimum_Playable (G));
      end loop;
      return Result;
   end Part_2;

begin
   Read_IO;
   --  Put_Games;
   IO.Put_Line ("Sum of valid game indices is" & Part_1'Image);
   IO.Put_Line ("Sum of power of minimum sets is" & Part_2'Image);
end Day2;
