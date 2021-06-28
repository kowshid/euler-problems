(*
    Project Euler #22

    Names scores
    --------------------
    Using p022_names.txt, a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
    For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

    What is the total of all the name scores in the file?
*)

let calculateNameScore (position: uint) (name: string) : uint =
    let letterValueSum =
        name
        |> Seq.map (fun letter -> uint letter - uint 'A' + 1u)
        |> Seq.sum

    position * letterValueSum

let processInput (fileDir: string) : seq<string> =
    let separators = [|"\""; ","; "\n"; "\t"|]

    let input = System.IO.File.ReadAllText(fileDir).ToUpper()

    input.Split(separators, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.sort

let calculateNameScoreSum (fileDir: string) : uint =
    processInput fileDir
    |> Seq.mapi (fun index name -> calculateNameScore (uint (index + 1)) name)
    |> Seq.sum

printf "Enter the input file path: "

let inputFilePath = System.Console.ReadLine()

let timer = System.Diagnostics.Stopwatch.StartNew()

let result = calculateNameScoreSum inputFilePath

timer.Stop()

printfn "The total of all the name scores in the file %s is %d" inputFilePath result
printfn "Time elapsed in milliseconds %f" timer.Elapsed.TotalMilliseconds

// Answer: 871198282
// Run Time: 10.0526 milliseconds