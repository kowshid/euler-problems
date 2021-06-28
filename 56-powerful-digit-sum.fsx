(*
    Project Euler #56

    Powerful digit sum
    --------------------
    A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.
    Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
*)

let calculateDigitSum (numToCalculateDigitSum: bigint) : uint16 =
    let rec loop (num: bigint) (sum: uint16) : uint16 =
        if num = 0I then
            sum
        else
            loop (num / 10I) sum + uint16 (num % 10I)

    loop numToCalculateDigitSum 0us

let calculateMaxDigitSum (limit: bigint) : uint16 =
    seq {
        for baseNum in limit - 1I .. -1I .. 2I do
            for power in (int limit - 1) .. -1 .. 2 do
                yield calculateDigitSum (pown baseNum power)
    }
    |> Seq.max

printf "Enter the limit for both base and power: "

let basePowerLimit = bigint.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculateMaxDigitSum basePowerLimit

timer.Stop()

printfn "The maximum digital sum for numbers a^b, where a, b < %A is %d" basePowerLimit result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 972
// Run Time: 141.6606 milliseconds