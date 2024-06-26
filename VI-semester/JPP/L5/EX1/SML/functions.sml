fun binomial n 0 = 1
  | binomial 0 k = 0
  | binomial n k = (binomial (n - 1) (k - 1) * n) div k

fun nextRow row = 
    let
        val row' = 0 :: row
        val row'' = row @ [0]
    in
        ListPair.map op+ (row', row'')
    end

fun pascal () =
    let
        fun rows [] = [[1]]
          | rows (r::rs) = let val next = nextRow r in r :: rows (next::rs) end
    in
        rows [[1]]
    end

fun binomial2 n k =
    let
        fun factorial 0 = IntInf.fromInt 1
          | factorial m = IntInf.* (factorial (m - 1), IntInf.fromInt m)

        fun binomialCoeff n k = 
            let
                val numerator = factorial n
                val denominator = IntInf.* (factorial k, factorial (n - k))
            in
                IntInf.div (numerator, denominator)
            end
    in
        binomialCoeff n k
    end


fun merge (xs, []) = xs
  | merge ([], ys) = ys
  | merge (x::xs, y::ys) = if x <= y then x :: merge (xs, y::ys) else y :: merge (x::xs, ys)

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs = 
    let
        val n = List.length xs div 2
        val ys = List.take(xs, n)
        val zs = List.drop(xs, n)
    in
        merge (mergesort ys, mergesort zs)
    end

fun extendedGCD 0 b = (b, 0, 1)
  | extendedGCD a b =
    let
        val (g, x, y) = extendedGCD (b mod a) a
    in
        (g, y - (b div a) * x, x)
    end

fun de (a, 0) = (1, 0, a)
  | de (a, b) =
    let
        val (x', y', gcd) = de (b, (a mod b))
        val (x, y) = (y', x' - y' * (a div b))
    in
        (x, y, gcd)
    end


fun primeFactors n = 
    let
        fun factor (n, p) =
            if p * p > n then if n > 1 then [n] else []
            else if n mod p = 0 then p :: factor (n div p, p)
            else factor (n, p + 1)
    in
        factor (n, 2)
    end

fun gcd (x, y) = 
    let
        fun gcd' 0 b = b
          | gcd' a b = gcd' (b mod a) a
    in
        gcd' x y
    end

fun totient n = 
    List.length (List.filter (fn x => gcd (x, n) = 1) (List.tabulate (n, fn i => i + 1)))

fun totient n = 
    List.length (List.filter (fn x => gcd (x, n) = 1) (List.tabulate (n, fn i => i + 1)))

fun intPow (x, 0) = 1
  | intPow (x, n) = x * intPow (x, n - 1)

fun group [] = []
  | group (x::xs) = 
    let
        val (prefix, suffix) = List.partition (fn y => y = x) xs
    in
        (x::prefix) :: group suffix
    end

fun totient2 n =
    let
        fun product (p, k) = (p - 1) * intPow (p, k - 1)
        val factors = primeFactors n
        val groupedFactors = group factors
        val factorCounts = List.map (fn xs => (List.hd xs, List.length xs)) groupedFactors
    in
        List.foldl (fn (pk, acc) => acc * product pk) 1 factorCounts
    end


fun isPrime p = 
    let
        val sqrtP = Real.floor (Math.sqrt (Real.fromInt p))
        fun checkDivisors d = d > sqrtP orelse (p mod d <> 0 andalso checkDivisors (d + 1))
    in
        checkDivisors 2
    end

fun primes n = 
    List.filter isPrime (List.tabulate (n - 1, fn i => i + 2))


fun listToString xs = "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"

fun main () =
    let
    	val _ = print ("\n\n\n")
        val _ = print ("Binomial coefficient for n = 30 and k = 10: " ^ Int.toString (binomial 30 10) ^ "\n")
        val _ = print ("Binomial coefficient (using Pascal's triangle) for n = 30 and k = 10: " ^ IntInf.toString (binomial2 30 10) ^ "\n")
        val _ = print ("Merge sort of [4, 2, 1, 3, 8, 10, 17, 9, 5]: " ^ listToString (mergesort [4, 2, 1, 3, 8, 10, 17, 9, 5]) ^ "\n")
        val _ = print (case (de (12345, 67890)) of (x, y, z) => "Extended GCD for 12345x + 67890y = z: " ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString z ^ "\n")
        val _ = print ("Prime factors of 12345678: " ^ listToString (primeFactors 12345678) ^ "\n")
        val _ = print ("Euler's Totient function for 100: " ^ Int.toString (totient 100) ^ "\n")
        val _ = print ("Euler's Totient function using prime factors for 100: " ^ Int.toString (totient2 100) ^ "\n")
        val _ = print ("Primes up to 100: " ^ listToString (primes 100))
        val _ = print ("\n\n\n")
    in
        ()
    end

val _ = main ()
