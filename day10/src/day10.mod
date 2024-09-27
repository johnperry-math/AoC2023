MODULE Day10;

FROM SYSTEM IMPORT ADR, CAST;

FROM EXCEPTIONS IMPORT ExceptionSource, AllocateSource, RAISE;

IMPORT InOut;

IMPORT Common;

TYPE Pipe = (
    Vertical,
    Horizontal,
    SE_Or_WN,
    SW_Or_EN,
    WS_Or_NE,
    ES_Or_NW,
    Ground,
    Start
);

PROCEDURE Deserialize (Symbol: CHAR): Common.Object;
VAR
    Result: Common.Object;
    P: Pipe;
BEGIN
    CASE Symbol OF
        '|' : P :=  Vertical;
      | '-' : P :=  Horizontal;
      | 'L' : P :=  SE_Or_WN;
      | 'J' : P :=  SW_Or_EN;
      | 'F' : P :=  WS_Or_NE;
      | '7' : P :=  ES_Or_NW;
      | '.' : P :=  Ground;
      | 'S' : P :=  Start;
    END;
    Result.Value := CAST(CARDINAL, P);
    RETURN Result;
END Deserialize;

PROCEDURE Serialize (O: Common.Object): CHAR;
VAR
    Result: CHAR;
    P: Pipe;
BEGIN
    P := CAST(Pipe, O.Value);
    CASE P OF
        Vertical: Result := '|';
      |  Horizontal : Result := '-';
      |  SE_Or_WN   : Result := 'L';
      |  SW_Or_EN   : Result := 'J';
      |  ES_Or_NW   : Result := '7';
      |  WS_Or_NE   : Result := 'F';
      |  Ground     : Result := '.';
      |  Start      : Result := 'S';
    END;
    RETURN Result;
END Serialize;

CONST SIDE_LENGTH = 140;

TYPE RowRange = [1..SIDE_LENGTH];
TYPE ColRange = [1..SIDE_LENGTH];

VAR Map: ARRAY RowRange, ColRange OF Common.Object;

TYPE ObjectArray = ARRAY Pipe OF Common.Object;
CONST Objects = ObjectArray {
    Common.Object {CAST(CARDINAL, Vertical)},
    Common.Object {CAST(CARDINAL, Horizontal)},
    Common.Object {CAST(CARDINAL, SE_Or_WN)},
    Common.Object {CAST(CARDINAL, SW_Or_EN)},
    Common.Object {CAST(CARDINAL, WS_Or_NE)},
    Common.Object {CAST(CARDINAL, ES_Or_NW)},
    Common.Object {CAST(CARDINAL, Ground)},
    Common.Object {CAST(CARDINAL, Start)}
};

CONST PipeObject = Common.Object {
    CAST(CARDINAL, Vertical)
};

VAR StartLocation: Common.LocationRecord;

PROCEDURE FindStart;
VAR
    Row: RowRange;
    Col: ColRange;
BEGIN
    FOR Row := 1 TO SIDE_LENGTH DO
        FOR Col := 1 TO SIDE_LENGTH DO
            IF Map [Row, Col].Value = Objects[Start].Value THEN
                StartLocation := Common.LocationRecord {Row, Col};
                RETURN;
            END;
        END;
    END;
END FindStart;

VAR TraversedMap : ARRAY RowRange, ColRange OF CHAR;

PROCEDURE InitializeTraversedMap;
VAR
    Row: RowRange;
    Col: ColRange;
BEGIN
    FOR Row := 1 TO SIDE_LENGTH DO
        FOR Col := 1 TO SIDE_LENGTH DO
            TraversedMap [Row, Col] := ' ';
        END;
    END;
END InitializeTraversedMap;

TYPE Animal = RECORD
    Curr, Prev: Common.LocationRecord;
END;

VAR SourceOfException: ExceptionSource;

TYPE Day10Exceptions = (LeftLoop, CycleLoop, CannotMove, DidNotMove);

PROCEDURE CanMove (Here, From: Common.LocationRecord): BOOLEAN;
VAR
    Current: Pipe;
BEGIN
    Current := CAST (Pipe, Map [From.Row, From.Col].Value);
    CASE CAST (Pipe, Map [Here.Row, Here.Col].Value) OF
        Vertical:
        RETURN ((From.Col = Here.Col) AND (From.Row - Here.Row = 1)
            AND (Current # Horizontal)
            AND (Current # ES_Or_NW)
            AND (Current # WS_Or_NE))
        OR
            ((From.Col = Here.Col) AND (From.Row - Here.Row = -1)
            AND (Current # Horizontal)
            AND (Current # SW_Or_EN)
            AND (Current # SE_Or_WN));
      | Horizontal:
        RETURN ((From.Row = Here.Row) AND (From.Col - Here.Col = 1)
            AND (Current # SE_Or_WN)
            AND (Current # WS_Or_NE)
            AND (Current # Vertical))
        OR
            ((From.Row = Here.Row) AND (From.Col - Here.Col = -1)
            AND (Current # SW_Or_EN)
            AND (Current # ES_Or_NW)
            AND (Current # Vertical));
      | WS_Or_NE:
        RETURN ((From.Row - Here.Row = 1) AND (From.Col = Here.Col)
            AND (Current # Horizontal)
            AND (Current # ES_Or_NW)
            AND (Current # WS_Or_NE))
        OR
            ((From.Row = Here.Row) AND (From.Col - Here.Col = 1)
            AND (Current # SE_Or_WN)
            AND (Current # WS_Or_NE)
            AND (Current # Vertical));
      | ES_Or_NW:
        RETURN ((From.Row - Here.Row = 1) AND (From.Col = Here.Col)
           AND (Current # ES_Or_NW)
           AND (Current # Horizontal)
           AND (Current # WS_Or_NE))
        OR ((From.Row = Here.Row) AND (From.Col - Here.Col = -1)
           AND (Current # ES_Or_NW)
           AND (Current # SW_Or_EN)
           AND (Current # Vertical));
      | SE_Or_WN:
        RETURN ((From.Row - Here.Row = -1) AND (From.Col = Here.Col)
           AND (Current # Horizontal)
           AND (Current # SE_Or_WN)
           AND (Current # SW_Or_EN))
        OR ((From.Row = Here.Row) AND (From.Col - Here.Col = 1)
           AND (Current # WS_Or_NE)
           AND (Current # SE_Or_WN)
           AND (Current # Vertical));
      | SW_Or_EN:
        RETURN ((From.Row = Here.Row) AND (From.Col - Here.Col = -1)
           AND (Current # ES_Or_NW)
           AND (Current # Vertical)
           AND (Current # SW_Or_EN))
        OR ((From.Row - Here.Row = -1) AND (From.Col = Here.Col)
           AND (Current # Horizontal)
           AND (Current # SE_Or_WN)
           AND (Current # SW_Or_EN));
      | Ground: RAISE (SourceOfException, ORD(LeftLoop), "Ground");
      | Start: RAISE (SourceOfException, ORD(CycleLoop), "Start");
    END;
    RAISE(SourceOfException, ORD(CannotMove), "Unknown reason");
    RETURN FALSE;
END CanMove;

PROCEDURE RecordMotion (Me: Animal);
VAR
    Row, Col: CARDINAL;
BEGIN
    Row := Me.Curr.Row;
    Col := Me.Curr.Col;

    IF Row = Me.Prev.Row THEN
        IF Col < Me.Prev.Col THEN
            TraversedMap [Row, Col] := '<';
        ELSE
            TraversedMap [Row, Col] := '>';
        END;
    ELSIF Row < Me.Prev.Row THEN
        TraversedMap [Row, Col] := '^';
    ELSE
        TraversedMap [Row, Col] := 'v';
    END;
END RecordMotion;

PROCEDURE Move (VAR Me : Animal; ButNotHere: Common.LocationRecord);
VAR
    Option: Common.LocationRecord;
    DRow, DCol : Common.Nudge;
BEGIN
    FOR DRow := -1 TO 1 DO
        IF (Me.Curr.Row + DRow >= 1) AND (Me.Curr.Row + DRow <= SIDE_LENGTH) THEN
            FOR DCol := -1 TO 1 DO
                IF (Me.Curr.Col + DCol >= 1) AND (Me.Curr.Col + DCol <= SIDE_LENGTH) THEN
                    Option.Row := Me.Curr.Row + DRow;
                    Option.Col := Me.Curr.Col + DCol;
                    IF ((Option.Row # Me.Prev.Row)         OR (Option.Col # Me.Prev.Col))
                        AND ((Option.Row # ButNotHere.Row) OR (Option.Col # ButNotHere.Col))
                        AND (CAST(Pipe, Map[Option.Row, Option.Col]) # Ground)
                        AND CanMove (Option, Me.Curr)
                    THEN
                        Me.Prev.Row := Me.Curr.Row;
                        Me.Prev.Col := Me.Curr.Col;
                        Me.Curr.Row := Option.Row;
                        Me.Curr.Col := Option.Col;
                        RETURN;
                    END;
                END;
            END;
        END;
    END;
    RAISE (SourceOfException, DidNotMove, "current position immovable");
    END Move;

PROCEDURE Part_1() : CARDINAL;
VAR
    First, Second: Animal;
    Step: CARDINAL;
BEGIN
    First.Curr.Row := StartLocation.Row;
    First.Curr.Col := StartLocation.Col;
    First.Prev.Row := StartLocation.Row;
    First.Prev.Col := StartLocation.Col;
    Second.Curr.Row := StartLocation.Row;
    Second.Curr.Col := StartLocation.Col;
    Second.Prev.Row := StartLocation.Row;
    Second.Prev.Col := StartLocation.Col;

    TraversedMap [StartLocation.Row, StartLocation.Col] := 'S';
    Move (First, StartLocation);
    RecordMotion (First);
    Move (Second, First.Curr);
    RecordMotion (Second);

    Step := 1;
    LOOP
        Move (First, StartLocation);
        RecordMotion (First);
        IF (First.Curr.Row = Second.Curr.Row)
        AND (First.Curr.Col = Second.Curr.Col)
        THEN
            EXIT;
        END;
        Move (Second, StartLocation);
        IF (First.Curr.Row = Second.Curr.Row)
        AND (First.Curr.Col = Second.Curr.Col)
        THEN
            EXIT;
        END;
        Step := Step + 1;
    END;

    RETURN Step + 1;    
    
END Part_1;

BEGIN
    AllocateSource (SourceOfException);
    InitializeTraversedMap;
    Common.ReadInput("input.txt", ADR(Map), SIDE_LENGTH, SIDE_LENGTH, Deserialize);
    FindStart;
    (*InOut.WriteString("Start location is ");
    InOut.WriteCard(StartLocation.Row, 0); InOut.WriteString(", ");
    InOut.WriteCard(StartLocation.Col, 0); InOut.WriteLn;*)
    (*Common.PutMap(ADR(Map), SIDE_LENGTH, SIDE_LENGTH, Serialize);*)
    InOut.WriteString ("From entrance to farthest point takes ");
    InOut.WriteCard (Part_1 (), 0);
    InOut.WriteString (" steps");
    InOut.WriteLn;
END Day10.