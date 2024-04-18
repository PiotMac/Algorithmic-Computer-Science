pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;

package library_iterative_h is

   type Solution is record
      x : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- ./library_iterative.h:7
      y : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- ./library_iterative.h:8
   end record
   with Convention => C_Pass_By_Copy;  -- ./library_iterative.h:9

   function factorial_iterative (n : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t  -- ./library_iterative.h:11
   with Import => True, 
        Convention => C, 
        External_Name => "factorial_iterative";

   function gcd_iterative (a : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t; b : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t  -- ./library_iterative.h:12
   with Import => True, 
        Convention => C, 
        External_Name => "gcd_iterative";

   function diophantine_equation_iterative
     (a : x86_64_linux_gnu_bits_stdint_intn_h.int64_t;
      b : x86_64_linux_gnu_bits_stdint_intn_h.int64_t;
      c : x86_64_linux_gnu_bits_stdint_intn_h.int64_t) return Solution  -- ./library_iterative.h:13
   with Import => True, 
        Convention => C, 
        External_Name => "diophantine_equation_iterative";

end library_iterative_h;
