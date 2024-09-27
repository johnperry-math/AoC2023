IMPLEMENTATION MODULE Alternate;

IMPORT InOut;

PROCEDURE P(A: Array2);
VAR   
   I, J: CARDINAL;
BEGIN  
   FOR I := 0 TO HIGH(A) DO
      FOR J := 0 TO HIGH(A[I]) DO
         InOut.Write(A[I][J]);
      END;
   END;
   InOut.WriteLn;
END P;

END Alternate.
