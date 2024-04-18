with Ada.Text_IO;
use Ada.Text_IO;

with Library_Iterative;
with Library_Recursive;

--with Library;
--use Library;

procedure Main is
    Sol_Iterative : Library_Iterative.Solution;
    Sol_Recursive : Library_Recursive.Solution;
begin
   Put_Line("##### ITERATIVE #####");
   Put_Line("12! = " & Library_Iterative.UInt64'Image(Library_Iterative.Factorial(12)));
   Put_Line("GCD(893724, 2947228) = " & Library_Iterative.UInt64'Image(Library_Iterative.GCD(893724, 2947228)));

   Sol_Iterative := Library_Iterative.Diophantine_Equation(2718, 3872, 2);
   Put_Line("Solution to the diophantine equation '2718x + 3872y = 2': x = " & Library_Iterative.Int64'Image(Sol_Iterative.X) & ", y = " & Library_Iterative.Int64'Image(Sol_Iterative.Y));

   Put_Line("##### RECURSIVE #####");
   Put_Line("12! = " & Library_Recursive.UInt64'Image(Library_Recursive.Factorial(12)));
   Put_Line("GCD(893724, 2947228) = " & Library_Recursive.UInt64'Image(Library_Recursive.GCD(893724, 2947228)));

   Sol_Recursive := Library_Recursive.Diophantine_Equation(2718, 3872, 2);
   Put_Line("Solution to the diophantine equation '2718x + 3872y = 2': x = " & Library_Recursive.Int64'Image(Sol_Recursive.X) & ", y = " & Library_Recursive.Int64'Image(Sol_Recursive.Y));
end Main;