(*
    Project Euler #14

    Longest Collatz sequence
    --------------------

    The following iterative sequence is defined for the set of positive integers:
        n → n / 2 (n is even)
        n → 3n + 1 (n is odd)

    Using the rule above and starting with 13, we generate the following sequence:
    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
    It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

    Which starting number, under one million, produces the longest chain?

    NOTE: Once the chain starts the terms are allowed to go above one million.
*)

let calculateNextTerm (num: uint64) : uint64 =
    if num % 2UL = 1UL then
        3UL * num + 1UL
    else
        num / 2UL

let calculateCollatzSequenceLength (initialNum: uint64): uint64 * uint =
    let rec calculateSequenceLength (currNum: uint64) (length: uint) : uint =
        if currNum = 1UL then
            length
        else
            calculateSequenceLength (calculateNextTerm currNum) (length + 1u)

    (initialNum, calculateSequenceLength initialNum 1u)

printf "Enter limit to find collatz sequence lengths: "

let limit = System.UInt64.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let numberWithLongestCollatzChain =
    seq { 1UL .. limit - 1UL }
    |> Seq.map (fun numToCalculateCollatzSeqLen -> calculateCollatzSequenceLength numToCalculateCollatzSeqLen)
    |> Seq.maxBy (fun (_, length) -> length)
    |> fst

timer.Stop()

printfn "Number with longest Collatz chain under %d is %d" limit numberWithLongestCollatzChain
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 837799
// Run Time: 519.2298 milliseconds