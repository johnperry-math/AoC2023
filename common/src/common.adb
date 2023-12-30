pragma Ada_2022;

with Ada.Text_IO;

package body Common is

   package IO renames Ada.Text_IO;

   package body Two_Dimensional_Map is

      function "<" (Left, Right : Location_Record) return Boolean is
        (Left.Row < Right.Row
         or else (Left.Row = Right.Row and then Left.Col < Right.Col));

   end Two_Dimensional_Map;

   package body Two_Dimensional_Map_IO is

      Map renames Map_Package.Map;

      procedure Read_Input
        (Filename : String :=
           (if Doing_Example then "example.txt" else "input.txt"))
      is
         Input : IO.File_Type;
      begin
         IO.Open (Input, IO.In_File, Filename);
         for Row in Map'Range (1) loop
            declare
               Line : constant String := IO.Get_Line (Input);
            begin
               for Col in Map'Range (2) loop
                  Map (Row, Col) := Deserialize (Line (Col));
               end loop;
            end;
         end loop;
      end Read_Input;

      procedure Put_Map is
      begin
         for Row in Map'Range (1) loop
            for Col in Map'Range (2) loop
               IO.Put (Serialize (Map (Row, Col)));
            end loop;
            IO.New_Line;
         end loop;
      end Put_Map;

   end Two_Dimensional_Map_IO;

end Common;
