app "day03"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Path, pf.File, pf.Task]
    provides [main] to pf

unwrap : Result a * -> a
unwrap = \result ->
    when result is
        Ok x -> x
        Err _ -> crash "unwrap failed"

## Replace last element in a list, if it is possible
setLast : List a, a -> Result (List a) [Overflow]
setLast = \xs, x ->
    len : Nat
    len = List.len xs

    index : Result Nat [Overflow]
    index = Num.subChecked len 1

    Result.map index \i ->
        List.set xs i x

padLine : List Str -> List Str
padLine = \line ->
    dbg
        line

    line
    |> List.prepend "."
    |> List.append "."

## Parse all the numbers into a list and create an index into the list keyed by coordinates
parseNumbers : Str -> { numbers : List Nat, index : Dict { x : Nat, y : Nat } Nat }
parseNumbers = \input ->
    initialState : { numbers: List Nat, index: Dict {x:Nat, y:Nat} Nat, x:Nat, y:Nat, prev:[Dot,Digit,Newline,Symbol] }
    initialState = { numbers: [], index: Dict.empty {}, x:0, y:0, prev:Dot }
    
    input
    |> Str.graphemes
    |> List.walkWithIndex initialState \state, elem, i ->
        dbg
            T state.x "," state.y ":" state.elem

        if Str.contains "0123456789" elem then
            thisDigit = (Str.toNat elem |> unwrap)
            when state.prev is
                Digit ->
                    { state &
                        # continuation of number
                        numbers: prevDigit = List.last state.numbers |> unwrap
                        state.numbers
                        |> setLast (10 * prevDigit + thisDigit)
                        |> unwrap,
                        index: state.index
                        |> Dict.insert { x: state.x, y: state.y } ((List.len state.numbers) - 1),
                        # modifiying last in numbers list so use length-1,
                        x: state.x + 1,
                        y: state.y,
                        prev: Digit,
                    }

                _ ->
                    { state &
                        # new number
                        numbers: state.numbers |> List.append thisDigit,
                        index: state.index
                        |> Dict.insert { x: state.x, y: state.y } (List.len state.numbers),
                        # appending to numbers list so use length
                        x: state.x + 1,
                        y: state.y,
                        prev: Digit,
                    }
        else if elem == "\n" then
            { state &
                x: 0,
                y: state.y + 1,
                x: 0,
                prev: Newline,
            }
        else if elem == "." then
            { state &
                x: state.x + 1,
                prev: Dot,
            }
        else
            { state &
                x: state.x + 1,
                prev: Symbol,
            }
    |> \parsed ->
        { numbers: parsed.numbers, index: parsed.index }

func1 = \input ->
    parsed =
        input
        |> parseNumbers

    numbers = parsed.numbers
    index = parsed.index

    ""

func2 = \_ -> ""

testInput1 = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
testInput2 = testInput1

main =
    task =
        input <- Path.fromStr "data/day03.txt" |> File.readUtf8 |> Task.await

        test1 = func1 testInput1
        # #part1 = func1 input
        part1 = ""

        test2 = func2 testInput2
        part2 = func2 input

        ##        expect test1 == "8"
        ##        expect part1 == "2913"
        ##        expect test2 == "2286"
        ##        expect part2 == "55593"

        {} <- Task.await (Stdout.line "test1: \(test1)")
        {} <- Task.await (Stdout.line "part1: \(part1)")
        {} <- Task.await (Stdout.line "test2: \(test2)")
        {} <- Task.await (Stdout.line "part2: \(part2)")
        Stdout.write ""

    Task.onErr task \_ -> crash "Failed to read and parse input"

