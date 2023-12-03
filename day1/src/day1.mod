(*
   Advent of Code 2023

   Day 1: Trebuchet?!

   In Modula-2!

   This is basically a port of the Ada version.
   See that file for detailed comments,
   and see the README for a discussion of the experience.
*)

MODULE Day1;

IMPORT CharClass, FIO, InOut, Strings;

TYPE

   CodeType = ARRAY [1 .. 100] OF CHAR;

   ParsedDigit = RECORD
      CASE Valid: BOOLEAN OF
      TRUE:
         Value: CARDINAL;
      ELSE
      END;
   END;

VAR

   Input: FIO.File;
   Code: CodeType;
   Codes: ARRAY [1 .. 1000] OF CodeType;

PROCEDURE WriteCode(Code: ARRAY OF CHAR);
BEGIN
   InOut.WriteString(Code);
END WriteCode;

PROCEDURE ReadInput;
VAR
   Idx: CARDINAL;
BEGIN
   Input := FIO.OpenToRead("input.txt");
   FOR Idx := 1 TO 1000 DO
      FIO.ReadString(Input, Codes[Idx]);
   END;
   FIO.Close(Input);
END ReadInput;

PROCEDURE Digit(C: CHAR): CARDINAL;
BEGIN
   RETURN ORD(C) - ORD('0');
END Digit;

PROCEDURE NaiveParse(S: ARRAY OF CHAR): INTEGER;
VAR
   Idx: CARDINAL;
   FirstDigit, LastDigit: INTEGER;
BEGIN
   Idx := 0;

   WHILE NOT CharClass.IsNumeric(S[Idx]) DO
      INC(Idx);
   END;

   FirstDigit := Digit(S[Idx]);
   (* raise an exception if Result is not strAllRight *)

   LastDigit := FirstDigit;
   WHILE Idx <= Strings.Length(S) DO
      IF CharClass.IsNumeric(S[Idx]) THEN
         LastDigit := Digit(S[Idx]);
      END;
      INC(Idx);
   END;

   RETURN FirstDigit * 10 + LastDigit;
END NaiveParse;

PROCEDURE Part1(): INTEGER;
VAR
   Idx, Result: CARDINAL;
BEGIN
   Result := 0;
   FOR Idx := 1 TO 1000 DO
      INC(Result, NaiveParse(Codes[Idx]));
   END;
   RETURN Result;
END Part1;

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
BEGIN
   FirstDigit := 0;
   FOR Idx := 0 TO Strings.Length(S) - 1 DO
      IF CharClass.IsNumeric(S[Idx]) THEN
         IF FirstDigit = 0 THEN
            FirstDigit := Digit(S[Idx]);
            LastDigit := FirstDigit;
         ELSE
            LastDigit := Digit(S[Idx]);
         END;
      ELSE
         MaybeParsed := ParseWord(S, Idx);
         IF MaybeParsed.Valid THEN
            IF FirstDigit = 0 THEN
               FirstDigit := MaybeParsed.Value;
               LastDigit := FirstDigit;
            ELSE
               LastDigit := MaybeParsed.Value;
            END;
         END;
      END;
   END;
   RETURN FirstDigit * 10 + LastDigit;
END CorrectParse;

PROCEDURE Part2(): INTEGER;
VAR
   Idx, Result: CARDINAL;
BEGIN
   Result := 0;
   FOR Idx := 1 TO 1000 DO
      INC(Result, CorrectParse(Codes[Idx]));
   END;
   RETURN Result;
END Part2;

BEGIN

   ReadInput;

   InOut.WriteString("sum of calibration values is ");
   InOut.WriteInt(Part1(), 0);
   InOut.WriteLn;

   InOut.WriteString("sum of corrected calibration values is ");
   InOut.WriteInt(Part2(), 0);
   InOut.WriteLn;

END Day1.