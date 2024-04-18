with Ada.Text_IO;
use Ada.Text_IO;
with Interfaces.C;
use Interfaces.C;

with x86_64_linux_gnu_bits_types_h;
use x86_64_linux_gnu_bits_types_h;

with library_iterative_h;
use library_iterative_h;

procedure Iterative_Main is
    Sol_Iterative : Solution;
begin
   Put_Line("##### ITERATIVE #####");
   Put_Line("12! = " & uu_uint64_t'Image(factorial_iterative(12)));
   Put_Line("GCD(893724, 2947228) = " & uu_uint64_t'Image(gcd_iterative(893724, 2947228)));

   Sol_Iterative := diophantine_equation_iterative(2718, 3872, 2);
   Put_Line("Solution to the diophantine equation '2718x + 3872y = 2': x = " & uu_int64_t'Image(Sol_Iterative.X) & ", y = " & uu_int64_t'Image(Sol_Iterative.Y));
end Iterative_Main;