(*
    Project Euler #30

    Digit fifth powers
    --------------------
    Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4
    As 1 = 1^4 is not a sum it is not included.

    The sum of these numbers is 1634 + 8208 + 9474 = 19316.
    Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
*)

let extractDigitsOfNum (numToExtractDigits: uint) : List<uint> =
    let rec loop (num: uint) (acc: List<uint>) : List<uint> =
        match num with
        | 0u -> acc
        | _  -> loop (num / 10u) (num % 10u :: acc)

    loop numToExtractDigits List.Empty

let sumOfFifthPowerOfDigits (num: uint) : uint =
    let digits = extractDigitsOfNum num

    digits
    |> List.fold
        (fun sum digit ->
            sum + (pown digit 5)
        )
        0u

let canBeWrittenAsSumOfFifthPowerOfDigits (num: uint) : bool =
    num = sumOfFifthPowerOfDigits num

let timer = System.Diagnostics.Stopwatch.StartNew()

let result =
    { 10u .. 354294u }
    |> Seq.filter canBeWrittenAsSumOfFifthPowerOfDigits
    |> Seq.sum

timer.Stop()

printfn "The sum of all the numbers that can be written as the sum of fifth powers of their digits is %d" result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 443839
// Run Time: 30.746 milliseconds