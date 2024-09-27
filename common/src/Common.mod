(*
Types, packages, and subprograms useful for Advent of Code
*)

IMPLEMENTATION MODULE Common;

FROM SYSTEM IMPORT ADR, CAST, TSIZE;

IMPORT FIO, InOut, Storage;

PROCEDURE Opposite(Left, Right: Direction): BOOLEAN;
VAR 
    Result: BOOLEAN;
BEGIN
    CASE Left OF
        North: Result := Right = South;
    |   South: Result := Right = North;
    |   East : Result := Right = West;
    |   West : Result := Right = East;
    END;
    RETURN Result;
END Opposite;

PROCEDURE NewMap(Row_Length, Col_Length: CARDINAL): MapRef;
VAR
    Result: MapRef;
BEGIN
    Storage.ALLOCATE (Result, Row_Length * Col_Length * TSIZE (Object));
    RETURN Result;              
END NewMap;

PROCEDURE ReleaseMap (VAR Map: MapRef; Rows, Cols: CARDINAL);
BEGIN
    Storage.DEALLOCATE (Map, Rows * Cols * TSIZE (Object));
    Map := NIL;
END ReleaseMap;

TYPE LocationArray = ARRAY CARDINAL OF LocationRecord;
TYPE LocationArrayPointer = POINTER TO LocationArray;

TYPE LocationVectorRecord = RECORD
    Size, LastIndex: CARDINAL;
    Values: LocationArrayPointer;
END;

TYPE LocationVector = POINTER TO LocationVectorRecord;

PROCEDURE NewVector(): LocationVector;
VAR
    Result: LocationVector;
    Values: LocationArrayPointer;
BEGIN
    Storage.ALLOCATE (Result, SIZE(LocationVectorRecord));
    Storage.ALLOCATE (Values, 1000 * TSIZE (LocationRecord));
    Result^.Values := Values;
    Result^.Size := 1000;
    Result^.LastIndex := 0;
    RETURN Result;
END NewVector;

PROCEDURE ReleaseVector (VAR Vector: LocationVector);
BEGIN
    Storage.DEALLOCATE (Vector^.Values, Vector^.Size * TSIZE (LocationRecord));
    Storage.DEALLOCATE (Vector, TSIZE (LocationVectorRecord));
    Vector := NIL;
END ReleaseVector;

PROCEDURE Append (VAR Vector: LocationVector; Location: LocationRecord);
VAR
    Temp: LocationArrayPointer;
    I: CARDINAL;
BEGIN
    IF Vector^.Size = Vector^.LastIndex THEN
        Storage.ALLOCATE (Temp, 2 * Vector^.Size * TSIZE (LocationRecord));
        FOR I := 0 TO Vector^.Size - 1 DO
            Temp^[I] := Vector^.Values^[I];
        END;
        Storage.DEALLOCATE (Vector^.Values, Vector^.Size * TSIZE (LocationRecord));
        Vector^.Values := Temp;
        Vector^.Size := Vector^.Size * 2;
    END;
    Vector^.Values^[Vector^.LastIndex] := Location;
    INC(Vector^.LastIndex);
END Append;

PROCEDURE Element (Vector: LocationVector; Index: CARDINAL): LocationRecord;
BEGIN
    RETURN Vector^.Values^[Index - 1];
END Element;

TYPE LocationQueueRecord = RECORD
    Size, FirstIndex, LastIndex: CARDINAL;
    Number: CARDINAL;
    Values  : LocationArrayPointer;
END;

TYPE LocationQueue = POINTER TO LocationQueueRecord;

PROCEDURE NewQueue(): LocationQueue;
CONST INITIAL_SIZE = 1000; (* Tested with 10 to make sure reallocation works *)
VAR
    Result: LocationQueue;
    Values: LocationArrayPointer;
BEGIN
    Storage.ALLOCATE (Result, TSIZE(LocationQueueRecord));
    Storage.ALLOCATE (Values, INITIAL_SIZE * TSIZE(LocationRecord));
    Result^.Values := Values;
    Result^.Size := INITIAL_SIZE;
    Result^.FirstIndex := 0;
    Result^.LastIndex := 0;
    Result^.Number := 0;
    RETURN Result;
END NewQueue;

PROCEDURE ReleaseQueue(VAR Queue: LocationQueue);
BEGIN
    Storage.DEALLOCATE (Queue^.Values, Queue^.Size * TSIZE (LocationRecord));
    Storage.DEALLOCATE (Queue, TSIZE (LocationQueueRecord));
    Queue := NIL;   
END ReleaseQueue;

PROCEDURE IsEmpty(Queue: LocationQueue): BOOLEAN;
VAR
    Result: BOOLEAN;
BEGIN
    Result := Queue^.Number = 0;
    (* Result := Queue^.FirstIndex = Queue^.LastIndex; *)
    RETURN Result;  
END IsEmpty;

PROCEDURE IsFull(Queue: LocationQueue): BOOLEAN;
VAR
    Result: BOOLEAN;
BEGIN
    (* Result := (Queue^.FirstIndex = 0) AND (Queue^.LastIndex = Queue^.Size - 1);
    Result := Result OR (
        (Queue^.FirstIndex > Queue^.LastIndex)
            AND (Queue^.FirstIndex - 1 = Queue^.LastIndex)
    ); *)
    Result := Queue^.Number = Queue^.Size;
    RETURN Result;
END IsFull;

PROCEDURE Enqueue (VAR Queue: LocationQueue; Location: LocationRecord);
VAR
    Temp: LocationArrayPointer;
    I, J: CARDINAL;
BEGIN
    IF IsFull(Queue) THEN
        InOut.WriteString ("Re-sizing"); InOut.WriteLn;
        Storage.ALLOCATE (Temp, 2 * Queue^.Size * TSIZE (LocationRecord));
        J := Queue^.FirstIndex;
        FOR I := 0 TO Queue^.Size - 1 DO
            Temp^[I] := Queue^.Values^[J];
            INC(J);
            IF J = Queue^.Size THEN
                J := 0;
            END;
        END;
        Storage.DEALLOCATE (Queue^.Values, Queue^.Size * TSIZE (LocationRecord));
        Queue^.Values := Temp;
        Queue^.Size := Queue^.Size * 2;
        Queue^.FirstIndex := 0;
        Queue^.LastIndex := Queue^.Number - 1;
    END;
    Queue^.Values^[Queue^.LastIndex] := Location;
    INC(Queue^.LastIndex);
    IF Queue^.LastIndex = Queue^.Size THEN
        Queue^.LastIndex := 0;
    END;
    INC(Queue^.Number);
END Enqueue;

PROCEDURE Dequeue (VAR Queue: LocationQueue): LocationRecord;
VAR
    Result: LocationRecord;
BEGIN
    Result := Queue^.Values^[Queue^.FirstIndex];
    Queue^.FirstIndex := Queue^.FirstIndex + 1;
    IF Queue^.FirstIndex = Queue^.Size THEN
        Queue^.FirstIndex := 0;
    END;
    DEC(Queue^.Number);
    RETURN Result;
END Dequeue;

PROCEDURE ReadInput(Filename: ARRAY OF CHAR; Map: MapLine; Rows, Cols: CARDINAL; Deserialize: Deserializer);
VAR
    Input: FIO.File;
    Row, Col: CARDINAL;
    Line: ARRAY [1..1000] OF CHAR;
    Eol: CHAR;
BEGIN
    Input := FIO.OpenToRead(Filename);
    FOR Row := 0 TO Rows - 1 DO
        FIO.ReadString(Input, Line);
        FOR Col := 0 TO Cols - 1 DO
            Map^[Row * Cols + Col] := Deserialize(Line[Col + 1]);
        END;
    END;    
    FIO.Close(Input);
END ReadInput;

PROCEDURE PutMap(Map: MapLine; Rows, Cols: CARDINAL; Serialize: Serializer);
VAR
    Row, Col: CARDINAL;
BEGIN
    FOR Row := 0 TO Rows - 1 DO
        FOR Col := 0 TO Cols - 1 DO
            InOut.Write(Serialize(Map^[Row * Cols + Col]));
        END;
        InOut.WriteLn;
    END;
END PutMap;

END Common.