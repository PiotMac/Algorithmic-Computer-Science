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

fun measureTime f x =
  let
    val start_time = Time.now ()
    val _ = f x
    val end_time = Time.now ()
    val diff = Time.-(end_time, start_time)
    val seconds = Time.toReal diff  (* Convert time difference to real seconds *)
  in
    seconds
  end
  
fun main () =
  let
    fun loop i =
      if i > 2000 then ()
      else
        let
          (* val time1 = measureTime (fn x => binomial i 10) () *)
          (* val time2 = measureTime (fn x => binomial2 i 10) () *)
          val time1 = measureTime (fn x => totient i) ()
          val time2 = measureTime (fn x => totient2 i) ()
        in
          print (Int.toString i ^ ";" ^ Real.toString time1 ^ ";" ^ Real.toString time2 ^ "\n");
          loop (i + 1)
        end
  in
    print "\n\n\n";
    print "n;first;second\n";
    loop 1000;
    print "\n\n\n"
  end

val _ = main ()
