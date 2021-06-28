(*
    Project Euler #21

    Amicable numbers
    --------------------
    Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
    If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

    For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

    Evaluate the sum of all the amicable numbers under 10000.
*)

let calculateSumOfDivisors (num: uint) : uint =
    let squareRoot = uint (sqrt (double num))

    { 2u .. squareRoot }
    |> Seq.fold
        (fun sum divisorToCheck ->
            if num % divisorToCheck = 0u then
                divisorToCheck + (num / divisorToCheck) + sum
            else
                sum
        )
        1u

let isAmicable (num: uint) : bool =
    let possibleAmicaleConjugate = calculateSumOfDivisors num

    calculateSumOfDivisors possibleAmicaleConjugate = num && possibleAmicaleConjugate <> num

let calculateSumOfAmicableNumber (limit: uint) : uint =
    { 1u .. limit - 1u }
    |> Seq.filter isAmicable
    |> Seq.sum

printf "Enter the limit to find amicable numbers: "

let limit = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculateSumOfAmicableNumber limit

timer.Stop()

printfn "The sum of all the amicable numbers under %d is %d" limit result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 31626
// Run Time: 20.8952 milliseconds