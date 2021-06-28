(*
    Project Euler #6

    Sum square difference
    --------------------

    The sum of the squares of the first ten natural numbers is,
        1^2 + 2^2 + .. + 10^2 = 385
    The square of the sum of the first ten natural numbers is,
        (1 + 2 + .. + 10)^2 = 3025
    Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is .
        3025 - 385 = 2640
    Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)

let calculateSumOfSquares (limit: uint) : uint =
    { 1u .. limit }
    |> Seq.map (fun x -> x * x)
    |> Seq.sum

let calculateSquareOfSum (limit: uint) : uint =
    let totalSum = Seq.sum { 1u .. limit }
    (totalSum * totalSum)

printf "Enter limit to find difference between Square Of Sum and Sum Of Squares: "

let limit = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculateSquareOfSum limit - calculateSumOfSquares limit

timer.Stop()

printfn "The difference between the sum of the squares of the first one %d natural numbers and the square of the sum is %d" limit result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 25164150
// Run Time: 4.6064 milliseconds