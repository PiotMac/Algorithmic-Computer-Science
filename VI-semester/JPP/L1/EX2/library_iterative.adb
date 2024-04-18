package body Library_Iterative is
   function Factorial(N : UInt64) return UInt64 is
      Result : UInt64 := 1;
   begin
      for I in UInt64 range 2 .. N loop
         Result := Result * UInt64(I);
      end loop;

      return Result;
   end Factorial;

   function GCD(A, B : UInt64) return UInt64 is
      A1, B1, Temp : UInt64;
   begin
      A1 := A;
      B1 := B;
      while B1 /= 0 loop
         Temp := B1;
         B1 := A1 mod B1;
         A1 := Temp;
      end loop;

      return A1;
   end GCD;

   function Diophantine_Equation(A, B, C : Int64) return Solution is
      Old_R, R, Old_S, S, Old_T, T : Int64;
      Quotient, Temp : Int64;
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
   end Diophantine_Equation;

end Library_Iterative;