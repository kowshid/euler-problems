(*
    Project Euler #4

    Largest palindrome product
    --------------------

    A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
    Find the largest palindrome made from the product of two 3-digit numbers.
*)

let rec reverseNumber (current: uint) (acc: uint) : uint =
    if current = 0u then
        acc
    else
        reverseNumber (current / 10u) (acc * 10u + current % 10u)

let isPalindrome (num: uint) : bool =
    num = (reverseNumber num 0u)

let timer = System.Diagnostics.Stopwatch.StartNew()

let largestPalindromeOfTwo3DigitNumbersMultiple : uint =
    seq {
        for x in { 100u .. 999u } do
            for y in { 100u .. 999u } do
                if isPalindrome (x * y) then
                    yield (x * y)
    }
    |> Seq.max

timer.Stop()

printfn "The largest palindrome made from the product of two 3-digit numbers %d" largestPalindromeOfTwo3DigitNumbersMultiple
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 906609
// Run Time: 10.668 milliseconds