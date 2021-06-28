(*
    Project Euler #3

    Largest prime factor
    --------------------
    The prime factors of 13195 are 5, 7, 13 and 29.
    What is the largest prime factor of the number 600851475143 ?
*)

let isPrime (num: uint64) : bool =
    let squareRootOfN = uint64 (sqrt (double num))

    { 2UL .. squareRootOfN }
    |> Seq.forall (fun x -> num % x <> 0UL)

let rec generateFactorsForNumber (num: uint64) (currFactorToCheck: uint64) (factorLimit: uint64) (acc: seq<uint64>) : seq<uint64> =
    let reminder = (num % currFactorToCheck)

    if currFactorToCheck > factorLimit then
        acc
    elif reminder = 0UL then
        let newAcc =
            seq {
                yield currFactorToCheck
                yield (num / currFactorToCheck)
                yield! acc
            }
        generateFactorsForNumber num (currFactorToCheck + 1UL) factorLimit newAcc
    else
        generateFactorsForNumber num (currFactorToCheck + 1UL) factorLimit acc

let maxPrimeFactor (num: uint64) : uint64 =
    let squareRootOfNum = uint64 (sqrt (double num))

    generateFactorsForNumber num 1UL squareRootOfNum Seq.empty
    |> Seq.filter isPrime
    |> Seq.max

printf "Enter number to find largest prime factor: "

let input = System.UInt64.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = maxPrimeFactor input

timer.Stop()

printfn "Maximum Prime Factor of %d is %d" input result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 6857
// Run Time: 8.4752 milliseconds