MODULE Stringer;

IMPORT FIO;

VAR A: ARRAY CARDINAL OF CHAR;
VAR Input: FIO.File;

BEGIN
   Input := FIO.OpenToRead("Stringer.mod");
   FIO.ReadString(Input, A);
   FIO.Close(Input);
END Stringer.
