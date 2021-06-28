(*
    Project Euler #17

    Number letter counts
    --------------------
    If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
    If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

    NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
*)

let rec numToString (num: uint) : string =
    match num with
    | 1u    -> "one"
    | 2u    -> "two"
    | 3u    -> "three"
    | 4u    -> "four"
    | 5u    -> "five"
    | 6u    -> "six"
    | 7u    -> "seven"
    | 8u    -> "eight"
    | 9u    -> "nine"
    | 10u   -> "ten"
    | 11u   -> "eleven"
    | 12u   -> "twelve"
    | 13u   -> "thirteen"
    | 14u   -> "fourteen"
    | 15u   -> "fifteen"
    | 16u   -> "sixteen"
    | 17u   -> "seventeen"
    | 18u   -> "eighteen"
    | 19u   -> "nineteen"
    | x when x / 10u = 2u -> "twenty"  + numToString (x % 10u)
    | x when x / 10u = 3u -> "thirty"  + numToString (x % 10u)
    | x when x / 10u = 4u -> "forty"   + numToString (x % 10u)
    | x when x / 10u = 5u -> "fifty"   + numToString (x % 10u)
    | x when x / 10u = 6u -> "sixty"   + numToString (x % 10u)
    | x when x / 10u = 7u -> "seventy" + numToString (x % 10u)
    | x when x / 10u = 8u -> "eighty"  + numToString (x % 10u)
    | x when x / 10u = 9u -> "ninety"  + numToString (x % 10u)
    | x when x >= 1000u   -> numToString (x / 1000u) + "thousand" + numToString (x % 1000u)
    | x when x >= 100u    ->
        let prefix = numToString (x / 100u) + "hundred"
        if x % 100u = 0u then
            prefix
        else
            prefix + "and" + numToString (x % 100u)
    | _ -> ""

let letterCountForNumbersInWords (limit: uint) : int =
    seq { 1u .. limit }
    |> Seq.map (fun num -> numToString num)
    |> Seq.fold (+) ""
    |> String.length

printf "Enter the limit for converting numbers to words: "

let limit = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = letterCountForNumbersInWords limit

timer.Stop()

printfn "The letter count of the numbers from 1 to %d in words is %d" limit result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 21124
// Run Time: 8.6003 milliseconds