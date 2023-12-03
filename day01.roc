app "day01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Path, pf.File, pf.Task]
    provides [main] to pf

parse : Str -> List (List Str)
parse = \input ->
    input |> Str.split "\n" |> List.map Str.graphemes

func1 = \input ->
    input
    |> parse
    |> List.map \line ->
        line
        |> List.walk { nums: [] } \state, elem ->
            when Str.toU64 elem is
                Ok num ->
                    { state & nums: List.append state.nums num }

                Err _ ->
                    { state & nums: state.nums } ## At this point we have a list of digits so we get the first and last digits in the list
        |> \state ->
            first =
                when List.first state.nums is
                    Ok num -> num
                    Err _ -> 0
            last =
                when List.last state.nums is
                    Ok num -> num
                    Err _ -> 0
            10 * first + last
    |> List.sum
    |> Num.toStr

func2 = \input ->
    input
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.replaceEach "zero" "z0o"
        |> Str.replaceEach "one" "o1e"
        |> Str.replaceEach "two" "t2o"
        |> Str.replaceEach "three" "t3e"
        |> Str.replaceEach "four" "f4r"
        |> Str.replaceEach "five" "f5e"
        |> Str.replaceEach "six" "s6x"
        |> Str.replaceEach "seven" "s7n"
        |> Str.replaceEach "eight" "e8t"
        |> Str.replaceEach "nine" "n9e"
        |> Str.graphemes
        |> List.walk { nums: [] } \state, elem ->
            when Str.toU64 elem is
                Ok num ->
                    { state & nums: List.append state.nums num }

                Err _ ->
                    { state & nums: state.nums } ## At this point we have a list of digits so we get the first and last digits in the list
        |> \state ->
            first =
                when List.first state.nums is
                    Ok num -> num
                    Err _ -> 0
            last =
                when List.last state.nums is
                    Ok num -> num
                    Err _ -> 0
            10 * first + last
    |> List.sum
    |> Num.toStr

testInput1 = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
testInput2 = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

main =
    task =
        input <- Path.fromStr "data/day01.txt" |> File.readUtf8 |> Task.await

        test1 = func1 testInput1
        part1 = func1 input

        test2 = func2 testInput2
        part2 = func2 input

        {} <- Task.await (Stdout.line "test1: \(test1)")
        {} <- Task.await (Stdout.line "part1: \(part1)")
        {} <- Task.await (Stdout.line "test2: \(test2)")
        {} <- Task.await (Stdout.line "part2: \(part2)")
        Stdout.write ""

    Task.onErr task \_ -> crash "Failed to read and parse input"

