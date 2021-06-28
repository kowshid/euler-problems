(*
    Project Euler #23

    Non-abundant sums
    --------------------

    A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
    A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
    As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

    Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*)

let calculateSumOfDivisors (num: uint) : uint =
    let squareRoot = uint (sqrt (double num))

    seq {
        for divisorToCheck in 2u .. squareRoot do
            if num % divisorToCheck = 0u then
                yield divisorToCheck
                if num / divisorToCheck <> squareRoot then
                    yield (num / divisorToCheck)
    }
    |> Seq.sum
    |> (+) 1u

let isAbundant (num: uint) : bool =
    num < (calculateSumOfDivisors num)

let notSumOfTwoAbundantNum (num: uint) : bool =
    let rec checkIfNotSumOfTwoAbundantNum (numToCheck: uint) (subtractor: uint) : bool =
        match subtractor with
        | currSubtractor when currSubtractor >= numToCheck                                          -> true
        | currSubtractor when isAbundant currSubtractor && isAbundant (numToCheck - currSubtractor) -> false
        | _ ->
            checkIfNotSumOfTwoAbundantNum numToCheck (subtractor + 1u)

    checkIfNotSumOfTwoAbundantNum num 12u

let timer = System.Diagnostics.Stopwatch.StartNew()

let result =
    seq { 1u .. 28123u }
    |> Seq.filter notSumOfTwoAbundantNum
    |> Seq.sum

timer.Stop()

printfn "The sum of all the positive integers which cannot be written as the sum of two abundant numbers is %d" result
printfn "Time elapsed in seconds %f" timer.Elapsed.TotalSeconds

// Answer: 4179871
// Run Time: 8.540129 seconds