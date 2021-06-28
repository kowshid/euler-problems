(*
    Project Euler #9

    Special Pythagorean triplet
    --------------------

    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b^2 = c^2
    For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
*)

type PythagoreanTriplet = {
    adjacent:   uint
    opposite:   uint
    hypotenuse: uint
} with

    member this.Perimeter : uint =
        this.adjacent + this.opposite + this.hypotenuse

    member this.ProductOfSides : uint =
        this.adjacent * this.opposite * this.hypotenuse

let isRightAngleTriangle (adjacent: uint) (opposite: uint) (hypotenuse: uint) : bool =
    hypotenuse * hypotenuse = adjacent * adjacent + opposite * opposite

let findTargetPythagoreanTriplet (target: uint) : PythagoreanTriplet =
    seq {
        for x in { 1u .. target - 3u } do
            for y in { x + 1u .. target - 3u } do
                for z in { y + 1u .. target - 3u } do
                    if isRightAngleTriangle x y z then
                        {
                            adjacent   = x
                            opposite   = y
                            hypotenuse = z
                        }
    }
    |> Seq.find (fun x -> x.Perimeter = target)

printf "Target Perimeter of Pythogorean Triplet: "

let target = System.UInt32.Parse(System.Console.ReadLine())

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = (findTargetPythagoreanTriplet target).ProductOfSides

timer.Stop()

printfn "The target pythogorean triplet having perimeter %d has a product of %d" target result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 31875000
// Run Time: 427.297 milliseconds