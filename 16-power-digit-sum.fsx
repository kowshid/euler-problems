(*
    Project Euler #16

    Power digit sum
    --------------------

    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
    What is the sum of the digits of the number 2^1000?
*)
let calculateSumOfDigits (initialNum: bigint) : bigint =
    let rec calculateSum (num: bigint) (sum: bigint) : bigint =
        if num = 0I then
            sum
        else
            calculateSum (num / 10I) (sum + num % 10I)

    calculateSum initialNum 0I

let powerDigitSum (baseNum: bigint) (power: int) =
    pown baseNum power
    |> calculateSumOfDigits

printf "Enter the base number: "
let baseNum = bigint.Parse(System.Console.ReadLine())

printf "Enter the power: "
let power = System.Int32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = powerDigitSum baseNum power

timer.Stop()

printfn "The sum of the digits of the number %A^%d is %A" baseNum power result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 1366
// Run Time: 4.5798 milliseconds