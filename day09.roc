app "day09"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Path, pf.File, pf.Task]
    provides [main] to pf

unwrap : Result a * -> a
unwrap = \result ->
    when result is
        Ok x -> x
        Err _ -> crash "unwrap failed"

parse = \input ->
    lines =
        input
        |> Str.split "\n"
        |> List.dropIf Str.isEmpty

    array : List (List I64)
    array =
        lines
        |> List.map \line ->
            line
            |> Str.split " "
            |> List.dropIf Str.isEmpty
            |> List.map \numStr ->
                numStr
                |> Str.toI64
                |> unwrap

    array
    

func1 = \input ->
    array : List (List I64)
    array =
        input
        |> parse

    diff : List I64 -> List I64
    diff = \values ->
        fst = List.dropLast values 1
        snd = List.dropFirst values 1
        List.map2 snd fst Num.sub

    nextValue : List I64 -> I64
    nextValue = \values ->
        #dbg values
        if List.all values Num.isZero then
            0
        else
            last =
                values
                |> List.last
                |> unwrap
            next = last + nextValue (diff values)
            #dbg next
            next
        
    array
    |> List.map nextValue
    |> List.sum
    |> Num.toStr

func2 = \input ->
    array : List (List I64)
    array =
        input
        |> parse

    diff : List I64 -> List I64
    diff = \values ->
        fst = List.dropLast values 1
        snd = List.dropFirst values 1
        List.map2 snd fst Num.sub

    prevValue : List I64 -> I64
    prevValue = \values ->
        #dbg values
        if List.all values Num.isZero then
            0
        else
            first =
                values
                |> List.first
                |> unwrap
            prev = first - prevValue (diff values)
            #dbg prev
            prev
        
    array
    |> List.map prevValue
    |> List.sum
    |> Num.toStr

testInput1 = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

main =
    task =
        input <- Path.fromStr "data/day09.txt" |> File.readUtf8 |> Task.await
        testInput2 = testInput1

        test1 = func1 testInput1
        part1 = func1 input
        test2 = func2 testInput2
        part2 = func2 input

        #test1 = ""
        #part1 = ""
        #test2 = ""
        #part2 = ""

        expect test1 == "114"
        expect part1 == "1901217887"
        expect test2 == "2"
        expect part2 == "905"

        {} <- Task.await (Stdout.line "test1: \(test1)")
        {} <- Task.await (Stdout.line "part1: \(part1)")
        {} <- Task.await (Stdout.line "test2: \(test2)")
        {} <- Task.await (Stdout.line "part2: \(part2)")
        Stdout.write ""

    Task.onErr task \_ -> crash "Failed to read and parse input"

