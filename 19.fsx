(*
    Project Euler #19

    Counting Sundays
    --------------------
    You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

    How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*)

let dayOfWeekExceptJanFeb (day: int) (month: int) (year: int) : int =
    let valueToAdd       = 13 * (month + 1) / 5
    let yearOfTheCentury = year % 100
    let century          = year / 100

    ((day + valueToAdd + yearOfTheCentury + yearOfTheCentury / 4 - 2 * century + century / 4) % 7 + 7) % 7

let dayOfWeekForJanFeb (day: int) (month: int) (year: int) : int =
    let valueToAdd       = 13 * (month + 13) / 5
    let yearOfTheCentury = (year - 1) % 100
    let century          = (year - 1) / 100

    ((day + valueToAdd + yearOfTheCentury + yearOfTheCentury / 4 - 2 * century + century / 4) % 7 + 7) % 7

let dayOfWeek (day: int) (month: int) (year: int) : int =
    match month with
    | 1 | 2 -> dayOfWeekForJanFeb day month year
    | _     -> dayOfWeekExceptJanFeb day month year

let countSundaysOnFirstOfMonth (startYear: int) (endYear: int) : int =
    seq {
        for year in startYear .. endYear do
            for month in 1 .. 12 do
                if (dayOfWeek 1 month year) = 1 then
                    yield (month, year)
    }
    |> Seq.length

printf "Enter start year: "
let startYear = System.Int32.Parse(System.Console.ReadLine())

printf "Enter end year: "
let endYear = System.Int32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = countSundaysOnFirstOfMonth startYear endYear

timer.Stop()

printfn "Sunday count on the first of the month from year %d to %d is %d" startYear endYear result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 171
// Run Time 4.4026 milliseconds