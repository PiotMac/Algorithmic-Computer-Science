package body Library is

    function Factorial_Iterative(N : Natural) return Positive is
      Result : Natural := 1;
   begin
      for I in 2 .. N loop
         Result := Result * I;
      end loop;

      return Result;
   end Factorial_Iterative;

   function GCD_Iterative(A, B : Natural) return Natural is
      A1, B1, Temp : Natural;
   begin
      A1 := A;
      B1 := B;
      while B1 /= 0 loop
         Temp := B1;
         B1 := A1 mod B1;
         A1 := Temp;
      end loop;

      return A1;
   end GCD_Iterative;

   function Diophantine_Equation_Iterative(A, B, C : Integer) return Solution is
      Old_R, R, Old_S, S, Old_T, T : Integer;
      Quotient, Temp : Integer;
      Result : Solution := (X => 0, Y => 0);
   begin
      Old_R := A;
      R := B;
      Old_S := 1;
      S := 0;
      Old_T := 0;
      T := 1;
      while R /= 0 loop
         Quotient := Old_R / R;
         Temp := Old_R;
         Old_R := R;
         R := Temp - Quotient * R;

         Temp := Old_S;
         Old_S := S;
         S := Temp - Quotient * S;

         Temp := Old_T;
         Old_T := T;
         T := Temp - Quotient * T;
      end loop;

      if C mod Old_R = 0 then
         Result.X := Old_S * (C / Old_R);
         Result.Y := Old_T * (C / Old_R);
      end if;

      return Result;
   end Diophantine_Equation_Iterative;

   function Factorial_Recursive(N : Natural) return Positive is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial_Recursive(N - 1);
      end if;
   end Factorial_Recursive;

   function GCD_Recursive(A, B : Natural) return Natural is
   begin
      if B = 0 then
         return A;
      else
         return GCD_Recursive(B, A mod B);
      end if;
   end GCD_Recursive;

   function Diophantine_Equation_Recursive(A, B, C : Integer) return Solution is
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
            Temp := Diophantine_Equation_Recursive(B, A mod B, C);
            Result.X := Temp.Y;
            Result.Y := Temp.X - (A / B) * Temp.Y;
            return Result;
         end;
      end if;
   end Diophantine_Equation_Recursive;

end Library;