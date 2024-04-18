pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;

package library_recursive_h is

   type Solution is record
      x : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- ./library_recursive.h:7
      y : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- ./library_recursive.h:8
   end record
   with Convention => C_Pass_By_Copy;  -- ./library_recursive.h:9

   function factorial_recursive (n : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t  -- ./library_recursive.h:11
   with Import => True, 
        Convention => C, 
        External_Name => "factorial_recursive";

   function gcd_recursive (a : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t; b : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t  -- ./library_recursive.h:12
   with Import => True, 
        Convention => C, 
        External_Name => "gcd_recursive";

   function diophantine_equation_recursive
     (a : x86_64_linux_gnu_bits_stdint_intn_h.int64_t;
      b : x86_64_linux_gnu_bits_stdint_intn_h.int64_t;
      c : x86_64_linux_gnu_bits_stdint_intn_h.int64_t) return Solution  -- ./library_recursive.h:13
   with Import => True, 
        Convention => C, 
        External_Name => "diophantine_equation_recursive";

end library_recursive_h;
