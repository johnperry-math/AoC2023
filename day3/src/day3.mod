(*
   Advent of Code 2023

   Day 3: Gear Ratios

   In Modula-2!

   This is basically a port of the Ada version.
   See that file for detailed comments,
   and see the README for a discussion of the experience.
*)

MODULE Day3;

IMPORT CharClass, FIO, InOut, Strings;

TYPE

   Constraints = [1 .. 140];
   SchematicRow = ARRAY Constraints OF CHAR;

   UsedLocation = RECORD
      Row, Col: Constraints;
   END;

   TwoLocations = RECORD
      CASE Valid: BOOLEAN OF
      TRUE:
            First, Second: UsedLocation;
      | ELSE
      END;
   END;

VAR

   Schematic: ARRAY Constraints OF SchematicRow;

   UsedLocations: ARRAY Constraints, Constraints OF BOOLEAN;

PROCEDURE Digit(C: CHAR): CARDINAL;
BEGIN
   RETURN ORD(C) - ORD('0');
END Digit;

PROCEDURE ReadInput;
VAR
   Input: FIO.File;
   Idx: Constraints;
   Eol: CHAR;
BEGIN
   Input := FIO.OpenToRead("input.txt");
   FOR Idx := MIN(Constraints) TO MAX(Constraints) DO
      FIO.ReadString(Input, Schematic[Idx]);
      (* ARGH *)
      Eol := FIO.ReadChar(Input);
   END;
   FIO.Close(Input);
END ReadInput;

PROCEDURE InitializeUsedLocations;
VAR
   Row, Col: Constraints;
BEGIN
   FOR Row := MIN(Constraints) TO MAX(Constraints) DO
      FOR Col := MIN(Constraints) TO MAX(Constraints) DO
         UsedLocations[Row, Col] := FALSE;
      END;
   END;
END InitializeUsedLocations;

PROCEDURE ExpandUnused(Row, Col: Constraints): CARDINAL;
VAR
   Result: CARDINAL;
   Idx, Left, Right: Constraints;
BEGIN
   Result := 0;
   Left := Col;
   Right := Col;

   WHILE (Left > MIN(Constraints)) AND (CharClass.IsNumeric(Schematic[Row, Left - 1])) DO
      DEC(Left);
   END;

   WHILE (Right < MAX(Constraints)) AND (CharClass.IsNumeric(Schematic[Row, Right + 1])) DO
      INC(Right);
   END;

   IF NOT UsedLocations[Row, Left] THEN
      FOR Idx := Left TO Right DO
         Result := Result * 10 + Digit(Schematic[Row, Idx]);
      END;
      UsedLocations[Row, Left] := TRUE;
   END;

   RETURN Result;
END ExpandUnused;

PROCEDURE InConstraints(Value: INTEGER): BOOLEAN;
BEGIN
   RETURN (Value >= 0) AND (Value <= MAX(Constraints));
END InConstraints;

PROCEDURE UnusedNeighbors(Row, Col: Constraints): CARDINAL;
VAR
   Result: CARDINAL;
   RowOffset, ColOffset: [-1..1];
   C: CHAR;
BEGIN
   Result := 0;
   FOR RowOffset := -1 TO 1 DO
      FOR ColOffset := -1 TO 1 DO
         IF InConstraints(Row + RowOffset) AND InConstraints(Col + ColOffset)
         THEN
            C := Schematic[Row + RowOffset, Col + ColOffset];
            IF CharClass.IsNumeric(C) THEN
               INC(Result, ExpandUnused(Row + RowOffset, Col + ColOffset));
            END;
         END;
      END;
   END;
   RETURN Result;
END UnusedNeighbors;

PROCEDURE Part1(): CARDINAL;
VAR
   Result: CARDINAL;
   Row, Col: Constraints;
   C: CHAR;
BEGIN
   Result := 0;

   FOR Row := MIN(Constraints) TO MAX(Constraints) DO
      FOR Col := MIN(Constraints) TO MAX(Constraints) DO
         C := Schematic[Row, Col];
         IF NOT (CharClass.IsNumeric(C) OR (C = '.')) THEN
            INC(Result, UnusedNeighbors(Row, Col));
         END;
      END;
   END;

   RETURN Result;
END Part1;

PROCEDURE Expand(Row, Col: Constraints): UsedLocation;
VAR
   Left: Constraints;
   Result: UsedLocation;
BEGIN
   Left := Col;

   WHILE InConstraints(Left - 1)
      AND CharClass.IsNumeric(Schematic[Row, Left - 1])
   DO
      DEC(Left);
   END;

   Result.Row := Row;
   Result.Col := Left;

   RETURN Result;
END Expand;

PROCEDURE EqualLocation(Left, Right: UsedLocation): BOOLEAN;
BEGIN
   RETURN (Left.Row = Right.Row) AND (Left.Col = Right.Col);
END EqualLocation;

PROCEDURE TwoAdjacencies(Row, Col: Constraints): TwoLocations;
(* AUGH
CONST
   InvalidAdjacencies = TwoLocations { FALSE };
*)
VAR
   Result: TwoLocations;
   Adjacencies: CARDINAL;
   First, Second, New: UsedLocation;
   RowOffset, ColOffset: [-1..1];
BEGIN
   Adjacencies := 0;
   Result.Valid := FALSE;

   FOR RowOffset := -1 TO 1 DO
      FOR ColOffset := -1 TO 1 DO
         IF InConstraints(Row + RowOffset) AND InConstraints(Col + ColOffset)
            AND CharClass.IsNumeric(Schematic[Row + RowOffset, Col + ColOffset])
         THEN
            New := Expand(Row + RowOffset, Col + ColOffset);

            CASE Adjacencies OF
              0:  Adjacencies := 1;
                  First := New;
            | 1:  IF NOT EqualLocation(First, New) THEN
                     Adjacencies := 2;
                     Second := New;
                  END;
            | 2:  IF NOT (
                     EqualLocation(First, New) OR EqualLocation(Second, New)
                  ) THEN
                     Adjacencies := 3;
                  END;
            ELSE
            END;
         END;
      END;
   END;

   IF Adjacencies = 2 THEN
      Result.Valid := TRUE;
      Result.First := First;
      Result.Second := Second;
   END;

   RETURN Result;
END TwoAdjacencies;

PROCEDURE ValueAt(L: UsedLocation): CARDINAL;
VAR
   Left: Constraints;
   Value: CARDINAL;
BEGIN
   Left := L.Col;
   Value := 0;

   WHILE (Left <= MAX(Constraints))
      AND CharClass.IsNumeric(Schematic[L.Row, Left])
   DO
      Value := Value * 10 + Digit(Schematic[L.Row, Left]);
      INC(Left);
   END;

   RETURN Value;
END ValueAt;

PROCEDURE GearRatio (Where: TwoLocations): CARDINAL;
VAR
   First, Second: CARDINAL;
BEGIN
   First := ValueAt(Where.First);
   Second := ValueAt(Where.Second);
   RETURN First * Second;
END GearRatio;

PROCEDURE Part2(): CARDINAL;
VAR
   Result: CARDINAL;
   Row, Col: Constraints;
   Adjacencies: TwoLocations;
BEGIN
   Result := 0;
   FOR Row := MIN(Constraints) TO MAX(Constraints) DO
      FOR Col := MIN(Constraints) TO MAX(Constraints) DO
         IF Schematic[Row, Col] = '*' THEN
            Adjacencies := TwoAdjacencies(Row, Col);
            IF Adjacencies.Valid THEN
               INC(Result, GearRatio(Adjacencies));
            END;
         END;
      END;
   END;
   RETURN Result;
END Part2;

BEGIN
   InitializeUsedLocations;
   ReadInput;

   InOut.WriteString("sum of part numbers is ");
   (* AUGH *)
   InOut.WriteInt(Part1(), 0);
   InOut.WriteLn;

   InOut.WriteString("sum of gear ratios is ");
   InOut.WriteInt(Part2(), 0);
   InOut.WriteLn;
END Day3.