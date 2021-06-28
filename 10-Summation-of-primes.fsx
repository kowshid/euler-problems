(*
    Project Euler #10

    Summation of primes
    --------------------

    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    Find the sum of all the primes below two million.
*)

let isPrime (num: uint64) : bool =
    let squareRootOfN = uint64 (sqrt (double num))

    { 2UL .. squareRootOfN }
    |> Seq.forall (fun x -> num % x <> 0UL)

printf "Enter limit for prime numbers: "

let limit = System.UInt64.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let sumOfPrimes =
    { 2UL .. limit }
    |> Seq.filter isPrime
    |> Seq.sum

timer.Stop()

printfn "Summation of prime numbers below %d is %d" limit sumOfPrimes
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 142913828922
// Run Time: 1003.6851 milliseconds