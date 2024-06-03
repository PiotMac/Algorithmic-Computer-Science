with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Synchronous_Task_Control;
with Ada.Command_Line;
with Ada.Strings;
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Vectors;     use Ada.Containers;
use Ada;

procedure Dining_Philosophers is
   -- Number of philosophers
   Num_Philosophers : constant Integer := 5;

   -- Number of meals each philosopher must eat
   Meals_Per_Philosopher : constant Integer := 3;

   -- Time of eating and THINKING
   Time_Factor : constant Float := 1.0;

   -- Forks represented by protected objects
   protected type Fork is
      entry Pick_Up;
      entry Put_Down;
   private
      Available : Boolean := True;
   end Fork;

   protected body Fork is
      entry Pick_Up when Available is
      begin
         Available := False;
      end Pick_Up;

      entry Put_Down when not Available is
      begin
         Available := True;
      end Put_Down;
   end Fork;

   task type Philosopher (ID : Integer; LeftFork, RightFork : not null access Fork);
   
   task body Philosopher is
      Rand : Generator;
   begin
      Reset (Rand);
      for Life_Cycle in 1..Meals_Per_Philosopher loop
         Put_Line ("Philosopher[" & Integer'Image (ID) & " ] --> THINKING . . .");
         delay Duration (Random (Rand) * Time_Factor);
         Put_Line ("Philosopher[" & Integer'Image (ID) & " ] --> HUNGRY!");
         LeftFork.Pick_Up;
         RightFork.Pick_Up;
         Put_Line ("Philosopher[" & Integer'Image (ID) & " ] --> EATING . . .");
         delay Duration (Random (Rand) * Time_Factor);
         LeftFork.Put_Down;
         RightFork.Put_Down;
         Put_Line ("Philosopher[" & Integer'Image (ID) & " ] --> MEALS YET TO EAT: " & Integer'Image (Meals_Per_Philosopher - Life_Cycle));
      end loop;
      Put_Line ("Philosopher[" & Integer'Image (ID) & " ] --> LEAVES!");
   end Philosopher;

   Forks : array (1..Num_Philosophers) of aliased Fork; -- Forks for hungry philosophers
   Philosophers : array (1..Num_Philosophers) of access Philosopher;

begin
   for I in 1..Num_Philosophers loop
      Philosophers (I) := new Philosopher (I, Forks (I)'Access, Forks ((I mod Num_Philosophers) + 1)'Access);
   end loop;

   null;
end Dining_Philosophers;
