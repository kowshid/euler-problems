(*
    Project Euler #48

    Self powers
    --------------------

    The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
    Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
*)

let lastNDigitsOfSelfPoweringNumberSum (digitCount: uint16) (numberLimit: bigint) : bigint =
    let modValue = pown 10I (int digitCount)

    seq { 1I .. numberLimit }
    |> Seq.map
        (fun selfPoweringNum ->
            pown selfPoweringNum (int selfPoweringNum) % modValue
        )
    |> Seq.fold
        (fun sum bigintToAdd ->
            (sum + bigintToAdd) % modValue
        )
        0I

printf "Enter how many last digit to be calculated: "
let digitCount = System.UInt16.Parse(System.Console.ReadLine())

printf "Enter the final self powering number of the series: "
let limit = bigint.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = lastNDigitsOfSelfPoweringNumberSum digitCount limit

timer.Stop()

printfn "the last %d digits of the series 1^1 + 2^2 + .. + %A^%A is %A" digitCount limit limit result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 9110846700
// Run Time: 21.2796 milliseconds