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

   CodeIndexRange = [1 .. 100];
   CodeRange = [1 .. 1000];

   CodeType = ARRAY CodeIndexRange OF CHAR;

   ParsedDigit = RECORD
      CASE Valid: BOOLEAN OF
      TRUE:
         Value: CARDINAL;
      ELSE
      END;
   END;

   String5 = ARRAY [1 .. 5] OF CHAR;

   NumberSpelling = RECORD
      Value: CARDINAL;
      Spelling: String5;
   END;

   NumberSpellings = ARRAY [1 .. 3] OF NumberSpelling;

CONST

   String3Map = NumberSpellings {
      NumberSpelling { 1, "one  " },
      NumberSpelling { 2, "two  " },
      NumberSpelling { 6, "six  " }
   };

   String4Map = NumberSpellings {
      NumberSpelling { 4, "four " },
      NumberSpelling { 5, "five " },
      NumberSpelling { 9, "nine " }
   };

   String5Map = NumberSpellings {
      NumberSpelling { 3, "three" },
      NumberSpelling { 7, "seven" },
      NumberSpelling { 8, "eight" }
   };

VAR

   Input: FIO.File;
   Code: CodeType;
   Codes: ARRAY CodeRange OF CodeType;

PROCEDURE WriteCode(Code: ARRAY OF CHAR);
BEGIN
   InOut.WriteString(Code);
END WriteCode;

PROCEDURE ReadInput;
VAR
   Idx: CARDINAL;
BEGIN
   Input := FIO.OpenToRead("input.txt");
   FOR Idx := MIN(CodeRange) TO MAX(CodeRange) DO
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
   FOR Idx := MIN(CodeRange) TO MAX(CodeRange) DO
      INC(Result, NaiveParse(Codes[Idx]));
   END;
   RETURN Result;
END Part1;

PROCEDURE ParseNum(
   S: ARRAY OF CHAR;
   Idx: CARDINAL
): ParsedDigit;
VAR
   J, K: CARDINAL;
   Result: ParsedDigit;
   Candidate: ARRAY [1 .. 5] OF CHAR;
BEGIN;
   Result.Valid := FALSE;
   FOR J := 1 TO 3 DO
      Candidate[1 + J - 1] := S[Idx + J - 1];
   END;
   Candidate[4] := ' ';
   Candidate[5] := ' ';
   FOR K := 1 TO 3 DO
      IF Strings.Compare(Candidate, String3Map[K].Spelling) = Strings.equal
      THEN
         Result.Valid := TRUE;
         Result.Value := String3Map[K].Value;
      END;
   END;
   IF (NOT Result.Valid) & (Idx + 3 <= Strings.Length(S)) THEN
      Candidate[4] := S[Idx + 3];
      FOR K := 1 TO 3 DO
         IF Strings.Compare(Candidate, String4Map[K].Spelling) = Strings.equal
         THEN
            Result.Valid := TRUE;
            Result.Value := String4Map[K].Value;
         END;
      END;
   END;
   IF (NOT Result.Valid) & (Idx + 4 <= Strings.Length(S)) THEN
      Candidate[5] := S[Idx + 4];
      FOR K := 1 TO 3 DO
         IF Strings.Compare(Candidate, String5Map[K].Spelling) = Strings.equal
         THEN
            Result.Valid := TRUE;
            Result.Value := String5Map[K].Value;
         END;
      END;
   END;
   RETURN Result;
END ParseNum;

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
         MaybeParsed := ParseNum(S, Idx);
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
   FOR Idx := MIN(CodeRange) TO MAX(CodeRange) DO
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