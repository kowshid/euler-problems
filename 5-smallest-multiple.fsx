(*
    Project Euler #5

    Largest palindrome product
    --------------------

    2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)

let rec calculateGcd (large: uint) (small: uint) : uint =
    match small with
    | 0u -> large
    | _  -> calculateGcd small (large % small)

let rec getSmallestMultiple (start: uint) (acc: uint) : uint =
    match start with
    | 1u -> acc
    | _  -> getSmallestMultiple (start - 1u) (start * acc / (calculateGcd acc start))

printf "Enter limit to find smallest positive number multiple: "

let limit = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = getSmallestMultiple limit 1u

timer.Stop()

printfn "Smallest positive number that is evenly divisible by all of the numbers from 1 to %d is %d" limit result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 232792560
// Run Time: 0.6332 milliseconds