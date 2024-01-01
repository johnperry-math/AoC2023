pragma Ada_2022;

--  Advent of Code 2023
--
--  John Perry
--
--  Day 20: Pulse Propagation
--
--  part 1: determine product of low and high pulses after 1000 button presses
--
--  part 2: determine how many button presses it will take to activate machine

with Ada.Text_IO;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

with Common;

procedure Day20 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  modules

   Flipped_Off renames False;    -- convenient for legibility later on

   type Module_Enum is (Broadcast, Conjunction, Flipflop);
   type Pulse_Enum is (Low, High);

   type Label is new String (1 .. 2);

   package Label_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Label);

   type Memory_Record is record
      Source     : Label;
      Last_Pulse : Pulse_Enum;
   end record;
   --  memory record for a Conjunction

   package Memory_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Memory_Record);

   type Module_Record (Kind : Module_Enum) is record
      Children : Label_Vecs.Vector;
      case Kind is
         when Conjunction =>
            Memory : Memory_Vecs.Vector;
         when Flipflop =>
            Flip_State : Boolean;
         when Broadcast =>
            null;
      end case;
   end record;

   package Module_Child_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Label, Element_Type => Module_Record);

   All_Modules : Module_Child_Maps.Map;

   --  SUBSECTION
   --  Pulsing

   type Ludicrous_Size is range 0 .. 2**64 - 1;

   package Math is new Common.Mathematics
     (Base_Type => Ludicrous_Size, Zero => 0);

   Pulses : array (Pulse_Enum) of Ludicrous_Size := [others => 0];

   --  SECTION
   --  I/O

   procedure Get_Children (Text : String; Children : out Label_Vecs.Vector) is
      --  reads children from Text,
      --     where Text'First should index the first child'slabel
      --  stores result in Children

      Pos : Positive := Text'First;

   begin
      while Pos < Text'Last loop
         Children.Append (Label (Text (Pos .. Pos + 1)));
         Pos := @ + 4;
      end loop;
   end Get_Children;

   procedure Read_Input is
      Input : IO.File_Type;
   begin

      IO.Open (Input, IO.In_File, "input.txt");

      while not IO.End_Of_File (Input) loop

         declare
            Children : Label_Vecs.Vector;
            Line     : constant String := IO.Get_Line (Input);
         begin

            if Line (1) = '%' then
               --  Flipflop
               Get_Children (Line (8 .. Line'Last), Children);
               declare
                  Module : constant Module_Record :=
                    (Kind     => Flipflop, Flip_State => Flipped_Off,
                     Children => Children);
               begin
                  All_Modules.Insert (Label (Line (2 .. 3)), Module);
               end;

            elsif Line (1) = '&' then
               --  Conjunction
               Get_Children (Line (8 .. Line'Last), Children);
               declare
                  Module : constant Module_Record :=
                    (Kind => Conjunction, Memory => <>, Children => Children);
               begin
                  All_Modules.Insert (Label (Line (2 .. 3)), Module);
               end;

            else
               --  Broadcast
               Get_Children (Line (16 .. Line'Last), Children);
               declare
                  Module : constant Module_Record :=
                    (Kind => Broadcast, Children => Children);
               begin
                  All_Modules.Insert ("tx", Module);
               end;

            end if;
         end;
      end loop;

      IO.Close (Input);

   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   procedure Setup_Memories is
   --  need to initialize each conjunction's memories to know their source
   begin

      --  set up memory
      for Cursor in All_Modules.Iterate loop
         for Child_Label of Module_Child_Maps.Element (Cursor).Children loop

            if All_Modules.Contains (Child_Label) then
               declare
                  Child : constant Module_Child_Maps.Reference_Type :=
                    All_Modules.Reference (Child_Label);
               begin

                  if Child.Kind = Conjunction then
                     Child.Memory.Append
                       (Memory_Record'
                          (Source     => Module_Child_Maps.Key (Cursor),
                           Last_Pulse => Low));
                  end if;

               end;
            end if;

         end loop;
      end loop;

   end Setup_Memories;

   --  SUBSECTION
   --  broadcast and pulse

   type Broadcast_Record is record
      Source, Dest : Label;
      Kind         : Pulse_Enum;
   end record;

   package Broadcast_Queue_Interface is new Ada.Containers
     .Synchronized_Queue_Interfaces
     (Element_Type => Broadcast_Record);
   package Broadast_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Broadcast_Queue_Interface);

   Broadcast_Queue : Broadast_Queues.Queue;

   procedure Pulse
     (Module : in out Module_Record; Pulse : Pulse_Enum; Source, Self : Label)
   is
      Propagate : Pulse_Enum := Low;
   begin

      case Module.Kind is

         when Broadcast =>
            for Child of Module.Children loop
               Broadcast_Queue.Enqueue
                 (Broadcast_Record'
                    (Source => Self, Dest => Child, Kind => Pulse));
            end loop;
            --  adding 1 because no one else records a pulse to Broadcast
            Pulses (Propagate) :=
              @ + Ludicrous_Size (Module.Children.Length) + 1;

         when Conjunction =>
            declare
               Ith : Positive := Module.Memory.First_Index;
            begin

               --  find source in memories
               --  this should never fail if Setup_Memories ran properly
               while Module.Memory (Ith).Source /= Source loop
                  Ith := @ + 1;
               end loop;
               Module.Memory (Ith).Last_Pulse := Pulse;

               if (for some Memory of Module.Memory => Memory.Last_Pulse = Low)
               then
                  Propagate := High;
               end if;
               for Child of Module.Children loop
                  Broadcast_Queue.Enqueue
                    (Broadcast_Record'
                       (Source => Self, Dest => Child, Kind => Propagate));
               end loop;

               Pulses (Propagate) :=
                 @ + Ludicrous_Size (Module.Children.Length);

            end;

         when Flipflop =>
            --  does nothing when pulse is high
            if Pulse = Low then
               Module.Flip_State := not Module.Flip_State;
               if Module.Flip_State then
                  Propagate := High;
               end if;
               for Child of Module.Children loop
                  Broadcast_Queue.Enqueue
                    (Broadcast_Record'
                       (Source => Self, Dest => Child, Kind => Propagate));
               end loop;
               Pulses (Propagate) :=
                 @ + Ludicrous_Size (Module.Children.Length);
            end if;

      end case;
   end Pulse;

   --  SUBSECTION
   --  the main action

   package Label_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Label, Element_Type => Ludicrous_Size);

   procedure Parts_1_and_2 is
      BR   : Broadcast_Record;
      Each : Ludicrous_Size := 0;

      Rx_Source         : Label;
      Rx_Source_Sources : Label_Maps.Map;
   begin

      --  first identify the sources of rx's source
      --  (hope there's just one source of rx itself!)
      --  rx's source first (for me it's "nc")
      for Cursor in All_Modules.Iterate loop
         if Module_Child_Maps.Element (Cursor).Children.Contains ("rx") then
            Rx_Source := Module_Child_Maps.Key (Cursor);
            exit;
         end if;
      end loop;
      --  now its sources (i.e., "nc"'s')
      for Cursor in All_Modules.Iterate loop
         if Module_Child_Maps.Element (Cursor).Children.Contains (Rx_Source)
         then
            Rx_Source_Sources.Insert (Module_Child_Maps.Key (Cursor), 0);
         end if;
      end loop;

      --  now get a reindeer to press the button,
      --  first hooking up a machine that rewards it with a sugar lick
      --  every time it pushes it
      Button_Press :
      loop

         Each := @ + 1;
         Broadcast_Queue.Enqueue
           (Broadcast_Record'(Source => "bt", Dest => "tx", Kind => Low));

         while Natural (Broadcast_Queue.Current_Use) > 0 loop

            Broadcast_Queue.Dequeue (BR);
            if BR.Dest /= "rx" then

               Pulse (All_Modules (BR.Dest), BR.Kind, BR.Source, BR.Dest);

               if BR.Dest = Rx_Source and then BR.Kind = High
                 and then Rx_Source_Sources (BR.Source) = 0
               then
                  IO.Put_Line
                    ("On button press" & Each'Image & " " &
                     String (Rx_Source) & " received " & BR.Kind'Image &
                     " from " & String (BR.Source));
                  Rx_Source_Sources.Replace (BR.Source, Each);
                  if (for all Source of Rx_Source_Sources => Source /= 0) then
                     --  on to part 2!
                     exit Button_Press;
                  end if;
               end if;
            end if;

         end loop;

         if Each = 1_000 then    --  part 1
            IO.Put_Line
              ("after 1000 button presses, low pulses:" & Pulses (Low)'Image &
               " high pulses:" & Pulses (High)'Image);
            IO.Put_Line
              ("Product of high and low pulses is" &
               Ludicrous_Size'Image (Pulses (High) * Pulses (Low)));
         end if;

      end loop Button_Press;

      --  part 2
      declare
         Result : Ludicrous_Size := 1;
      begin

         for Each of Rx_Source_Sources loop
            Result := Math.Lcm (Result, Each);
         end loop;
         IO.Put_Line
           ("It will require" & Result'Image &
            " presses to start the machine");

      end;
   end Parts_1_and_2;

begin
   Read_Input;
   Setup_Memories;
   Parts_1_and_2;
end Day20;
