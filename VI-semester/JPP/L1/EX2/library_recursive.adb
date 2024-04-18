package body Library_Recursive is

   function Factorial(N : UInt64) return UInt64 is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial(N - 1);
      end if;
   end Factorial;

   function GCD(A, B : UInt64) return UInt64 is
   begin
      if B = 0 then
         return A;
      else
         return GCD(B, A mod B);
      end if;
   end GCD;

   function Diophantine_Equation(A, B, C : Int64) return Solution is
      Result : Solution := (X => 0, Y => 0);
   begin
      if C = 0 then
         return Result;
      elsif A = 0 and B = 0 then
         return Result;
      elsif A = 0 then
         Result.Y := C / B;
         return Result;
      elsif B = 0 then
         Result.X := C / A;
         return Result;
      else
         declare
            Temp : Solution;
         begin
            Temp := Diophantine_Equation(B, A mod B, C);
            Result.X := Temp.Y;
            Result.Y := Temp.X - (A / B) * Temp.Y;
            return Result;
         end;
      end if;
   end Diophantine_Equation;

end Library_Recursive;