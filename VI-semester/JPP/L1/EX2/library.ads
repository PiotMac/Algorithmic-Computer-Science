package Library is

   type Solution is record
      X : Integer;
      Y : Integer;
   end record;

   function Factorial_Iterative(N : Natural) return Positive;
   function Factorial_Recursive(N : Natural) return Positive;
   
   function GCD_Iterative(A, B : Natural) return Natural;
   function GCD_Recursive(A, B : Natural) return Natural;

   function Diophantine_Equation_Iterative(A, B, C : Integer) return Solution;
   function Diophantine_Equation_Recursive(A, B, C : Integer) return Solution;
   
end Library;