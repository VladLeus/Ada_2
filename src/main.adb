with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;

procedure Main is
   Dim : constant Integer := 200000;
   Thread_Num : constant Integer := 2;

   Arr : array(1..Dim) of Integer;
   subtype Random_Range is Integer range 1 .. Dim;

   package R is new
     Ada.Numerics.Discrete_Random (Random_Range);
   use R;

   procedure Init_Arr is
      Gen : Generator;
      X: Random_Range;
   begin
      Reset (Gen);

      for I in 1..Dim loop
         X := Random(Gen);
         Arr(I) := X;
      end loop;

      Arr(Random (Gen)) := -1;
   end Init_Arr;

   function part_min(start_index, finish_index : in Integer) return Integer is
      Min_Value : Integer := Integer'Last;
      Min_Index : Integer := 0;
   begin
      for I in start_index..finish_index loop
         if Arr(I) < Min_Value then
            Min_Value := Arr(I);
            Min_Index := I;
         end if;
      end loop;
      return Min_Index;
   end part_min;

   task type Starter_Thread is
      entry Start(Start_Index, Finish_Index : in Integer);
   end Starter_Thread;

   protected Part_Manager is
      procedure Set_Part_Min(Min_Index : in Integer);
      entry Get_Min(Min_Index : out Integer);
   private
      Tasks_Count : Integer := 0;
      Min1 : Integer := Integer'Last;
      Min_Index1 : Integer := 0;
   end Part_Manager;

   protected body Part_Manager is
      procedure Set_Part_Min(Min_Index : in Integer) is
      begin
         if Arr(Min_Index) < Min1 then
            Min1 := Arr(Min_Index);
            Min_Index1 := Min_Index;
         end if;
         Tasks_Count := Tasks_Count + 1;
      end Set_Part_Min;

      entry Get_Min(Min_Index : out Integer) when Tasks_Count = Thread_Num is
      begin
         Min_Index := Min_Index1;
      end Get_Min;
   end Part_Manager;

   task body Starter_Thread is
      Min_Index : Integer := 0;
      Start_Index, Finish_Index : Integer;
   begin
      accept Start(Start_Index, Finish_Index : in Integer) do
         Starter_Thread.Start_Index := Start_Index;
         Starter_Thread.Finish_Index := Finish_Index;
      end Start;
      Min_Index := part_min(Start_Index => Start_Index,
                            Finish_Index => Finish_Index);
      Part_Manager.Set_Part_Min(Min_Index);
   end Starter_Thread;

   function Parallel_Min return Integer is
   Min_Index : Integer := 0;
   Thread : array(1..Thread_Num) of Starter_Thread;
   Range_Size : Integer := Dim / Thread_Num;
begin
   for I in 1..Thread_Num loop
      Thread(I).Start((I - 1) * Range_Size + 1, I * Range_Size);
   end loop;

   Part_Manager.Get_Min(Min_Index);
   return Min_Index;
end Parallel_Min;


   Min_Index : Integer;
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Time_Span;
begin
   Start_Time := Clock;
   Init_Arr;
   Min_Index := Parallel_Min;
   Stop_Time    := Clock;
   Elapsed_Time := Stop_Time - Start_Time;
   Put_Line("Minimum value in array: " & Integer'Image(Arr(Min_Index)));
   Put_Line("Index of minimum value: " & Integer'Image(Min_Index));
   Put_Line ("Elapsed time: "
             & Duration'Image
                 (To_Duration (Elapsed_Time))
             & " seconds");
end Main;
