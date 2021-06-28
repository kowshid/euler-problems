(*
    Project Euler #7

    10001st prime
    --------------------
    By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
    What is the 10001st prime number?
*)

let isPrime (num: uint) : bool =
    let squareRootOfN = uint (sqrt (double num))
    { 2u .. squareRootOfN }
    |> Seq.forall (fun x -> num % x <> 0u)

let calculatetNthPrime (index: int) : uint =
    Seq.initInfinite (fun i -> uint i + 2u)
    |> Seq.filter isPrime
    |> Seq.item (index - 1)

printf "Enter the index of Prime Number: "

let index = System.Int32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculatetNthPrime index

timer.Stop()

printfn "The %dth prime number is %d" index result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 104743
// Run Time: 45.8002 milliseconds