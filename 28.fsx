(*
    Project Euler #28

    Even Fibonacci numbers
    --------------------
    Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
    21 22 23 24 25
    20  7  8  9 10
    19  6  1  2 11
    18  5  4  3 12
    17 16 15 14 13
    It can be verified that the sum of the numbers on the diagonals is 101.

    What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
*)

let sumOf4DiagonalNumsForGivenWidth (widthOfSpiral: uint) =
    let squreOfSide = widthOfSpiral * widthOfSpiral

    { 1u .. 3u }
    |> Seq.fold
        (fun sum multipleToSubstract ->
            sum + (squreOfSide - multipleToSubstract * (widthOfSpiral - 1u))
        )
        squreOfSide

let sumOfAllDiagonalNumbers (widthLimit: uint) : uint =
    { 3u .. 2u .. widthLimit }
    |> Seq.fold
        (fun sum spiralSideWidth ->
            sum + sumOf4DiagonalNumsForGivenWidth spiralSideWidth
        )
        1u

printf "Enter spiral side width: "

let spiralSideWidth = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = sumOfAllDiagonalNumbers spiralSideWidth

timer.Stop()

printfn "The sum of the numbers on the diagonals in a %d by %d spiral formed is %d" spiralSideWidth spiralSideWidth result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 669171001
// Run Time: 2.4821 milliseconds