(*
    Project Euler #1

    Multiples of 3 and 5
    --------------------

    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.
*)

let isDivisibleBy3or5 (num: uint) : bool =
    num % 3u = 0u || num % 5u = 0u

let calculateSumOfMultiplesOf3And5BelowNum (num: uint) : uint =
    { 3u .. num - 1u }
    |> Seq.fold
        (fun sum number ->
            if isDivisibleBy3or5 number then
                sum + number
            else
                sum
        )
        0u

printf "Enter N: "

let input = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculateSumOfMultiplesOf3And5BelowNum input

timer.Stop()

printfn "Sum of Multiples of 3 And 5 Below %d is %d" input result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// The answer is 233168
// Time needed 2.1918 milliseconds