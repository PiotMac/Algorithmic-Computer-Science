package Library_Recursive is

   type UInt64 is mod 2**64;
   type Int64 is range -2**63 .. 2**63 - 1;

   type Solution is record
      X : Int64;
      Y : Int64;
   end record;

   function Factorial(N : UInt64) return UInt64
      with
         Export => True,
         Convention => C,
         External_Name => "factorial_recursive";
   
   function GCD(A, B : UInt64) return UInt64
      with
         Export => True,
         Convention => C,
         External_Name => "gcd_recursive";

   function Diophantine_Equation(A, B, C : Int64) return Solution
      with
         Export => True,
         Convention => C,
         External_Name => "diophantine_equation_recursive";
   
end Library_Recursive;