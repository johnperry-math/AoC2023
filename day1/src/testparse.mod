MODULE TestParse;

IMPORT CharClass, ConvTypes, FIO, InOut, Strings, WholeStr;

TYPE

   ParsedDigit = RECORD
      CASE Valid: BOOLEAN OF
      TRUE:
         Value: CARDINAL;
      ELSE
      END;
   END;

PROCEDURE Digit(C: CHAR): CARDINAL;
BEGIN
   RETURN ORD(C) - ORD('0');
END Digit;

PROCEDURE ParseThree(S: ARRAY OF CHAR; Idx: CARDINAL): ParsedDigit;
VAR
   Result: ParsedDigit;
   Candidate: ARRAY [1 .. 3] OF CHAR;
BEGIN
   Result.Valid := FALSE;
   Candidate[1] := S[Idx];
   Candidate[2] := S[Idx + 1];
   Candidate[3] := S[Idx + 2];
   IF Strings.Compare(Candidate, "one") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 1;
   ELSIF Strings.Compare(Candidate, "two") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 2;
   ELSIF Strings.Compare(Candidate, "six") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 6;
   END;
   RETURN Result;
END ParseThree;

PROCEDURE ParseFour(S: ARRAY OF CHAR; Idx: CARDINAL): ParsedDigit;
VAR
   Result: ParsedDigit;
   Candidate: ARRAY [1 .. 4] OF CHAR;
BEGIN
   Result.Valid := FALSE;
   Candidate[1] := S[Idx];
   Candidate[2] := S[Idx + 1];
   Candidate[3] := S[Idx + 2];
   Candidate[4] := S[Idx + 3];
   IF Strings.Compare(Candidate, "four") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 4;
   ELSIF Strings.Compare(Candidate, "five") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 5;
   ELSIF Strings.Compare(Candidate, "nine") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 9;
   END;
   RETURN Result;
END ParseFour;

PROCEDURE ParseFive(S: ARRAY OF CHAR; Idx: CARDINAL): ParsedDigit;
VAR
   Result: ParsedDigit;
   Candidate: ARRAY [1 .. 5] OF CHAR;
BEGIN
   Result.Valid := FALSE;
   Candidate[1] := S[Idx];
   Candidate[2] := S[Idx + 1];
   Candidate[3] := S[Idx + 2];
   Candidate[4] := S[Idx + 3];
   Candidate[5] := S[Idx + 4];
   IF Strings.Compare(Candidate, "three") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 3;
   ELSIF Strings.Compare(Candidate, "seven") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 7;
   ELSIF Strings.Compare(Candidate, "eight") = Strings.equal THEN
      Result.Valid := TRUE;
      Result.Value := 8;
   END;
   RETURN Result;
END ParseFive;

PROCEDURE ParseWord(S: ARRAY OF CHAR; Idx: CARDINAL): ParsedDigit;
VAR
   Result, MaybeResult: ParsedDigit;
BEGIN
   Result.Valid := FALSE;
   IF Idx + 2 <= Strings.Length(S) THEN
      MaybeResult := ParseThree(S, Idx);
      IF MaybeResult.Valid THEN
         Result := MaybeResult;
      END;
   END;
   IF Idx + 3 <= Strings.Length(S) THEN
      MaybeResult := ParseFour(S, Idx);
      IF MaybeResult.Valid THEN
         Result := MaybeResult;
      END;
   END;
   IF Idx + 4 <= Strings.Length(S) THEN
      MaybeResult := ParseFive(S, Idx);
      IF MaybeResult.Valid THEN
         Result := MaybeResult;
      END;
   END;
   RETURN Result;
END ParseWord;

PROCEDURE CorrectParse(S: ARRAY OF CHAR): CARDINAL;
VAR
   FirstDigit: CARDINAL;
   LastDigit: CARDINAL;
   Idx: CARDINAL;
   MaybeParsed: ParsedDigit;
   Last_Loc: CARDINAL;
BEGIN
   FirstDigit := 0;
   FOR Idx := 0 TO Strings.Length(S) - 1 DO
      IF CharClass.IsNumeric(S[Idx]) THEN
         IF FirstDigit = 0 THEN
            FirstDigit := Digit(S[Idx]);
            LastDigit := FirstDigit;
            Last_Loc := Idx;
         ELSE
            LastDigit := Digit(S[Idx]);
            Last_Loc := Idx;
         END;
      ELSE
         MaybeParsed := ParseWord(S, Idx);
         IF MaybeParsed.Valid THEN
            IF FirstDigit = 0 THEN
               FirstDigit := MaybeParsed.Value;
               LastDigit := FirstDigit;
               Last_Loc := Idx;
            ELSE
               IF Last_Loc < Idx THEN
                  LastDigit := MaybeParsed.Value;
                  Last_Loc := Idx;
               END;
            END;
         END;
      END;
   END;
   RETURN FirstDigit * 10 + LastDigit;
END CorrectParse;

BEGIN
   InOut.WriteInt(CorrectParse("4ninejfpd1jmmnnzjdtk5sjfttvgtdqspvmnhfbm"), 0); InOut.WriteLn;
END TestParse.