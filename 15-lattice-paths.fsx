(*
    Project Euler #15

    Lattice paths
    --------------------

    Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
    How many such routes are there through a 20×20 grid?
*)

let rec factorial (num: uint) : bigint =
    match num with
    | 0u -> bigint(1)
    | _  -> bigint(num) * factorial (num - 1u)

let calculateRoutes (rows: uint) (columns: uint) : bigint =
    factorial (rows + columns) / factorial (rows) / factorial (columns)

printf "Enter rows count: "
let rows = System.UInt32.Parse(System.Console.ReadLine())

printf "Enter columns count: "
let columns = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculateRoutes rows columns

timer.Stop()

printfn "Possible routes through a %d×%d grid is %A" rows columns result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 137846528820
// Run Time: 0.4715 milliseconds