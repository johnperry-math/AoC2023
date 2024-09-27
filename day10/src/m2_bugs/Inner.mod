IMPLEMENTATION MODULE Inner;

IMPORT InOut;

PROCEDURE P (Num: CARDINAL);
VAR J: CARDINAL;
BEGIN
   InOut.WriteString("Whoops"); InOut.WriteLn;
   FOR I := 1 TO Num DO
      InOut.WriteString("Whoops"); InOut.WriteLn;
   END;
END P; 

BEGIN
END Inner.
