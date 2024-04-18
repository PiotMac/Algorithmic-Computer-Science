with Ada.Text_IO;
use Ada.Text_IO;
with Interfaces.C;
use Interfaces.C;

with x86_64_linux_gnu_bits_types_h;
use x86_64_linux_gnu_bits_types_h;

with library_recursive_h;
use library_recursive_h;

procedure Recursive_Main is
    Sol_Recursive : Solution;
begin
   Put_Line("##### RECURSIVE #####");
   Put_Line("12! = " & uu_uint64_t'Image(factorial_recursive(12)));
   Put_Line("GCD(893724, 2947228) = " & uu_uint64_t'Image(gcd_recursive(893724, 2947228)));

   Sol_Recursive := diophantine_equation_recursive(2718, 3872, 2);
   Put_Line("Solution to the diophantine equation '2718x + 3872y = 2': x = " & uu_int64_t'Image(Sol_Recursive.X) & ", y = " & uu_int64_t'Image(Sol_Recursive.Y));
end Recursive_Main;