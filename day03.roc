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
replaceLast : List a, a -> Result (List a) err
replaceLast = \xs, x ->
    len = List.len xs
    Num.subChecked len 1
    |> Result.try \index ->
        Str.replace xs index x

padLine : List Str -> List Str
padLine = \line ->
    dbg line
    line
    |> List.prepend "."
    |> List.append "."

func1 = \input ->
    lines : List (List Str)
    lines =
        input
        |> Str.split "\n"
        |> List.map Str.graphemes

    width : Nat
    width = lines |> List.get 1 |> unwrap |> List.len

    height : Nat
    height = lines |> List.len

    dbg width
    dbg height

    linesPad : List (List Str)
    linesPad =
        lines
        |> List.prepend (List.repeat "." width)
        |> List.append (List.repeat "." width)
        |> List.map padLine

    Coord : {x: Nat, y: Nat}
    Numbers : { numbers: List Nat, index: Dict {x: Nat, y: Nat} Nat }

    ## Parse all the numbers into a list and create an index into the list keyed by coordinates
    parseNumbers : Str -> Numbers
    parseNumbers = \input ->
        input
        |> Str.graphemes
        |> List.walkWithIndex {numbers: [], index: Dict.empty {}, x:0, y:0, prev: Dot } \state, elem, i ->
            if Str.contains "0123456789" elem then
                thisDigit = (Str.toNat elem |> unwrap)
                when state.prev is
                    Digit -> { # continuation of number
                        numbers:
                            prevDigit = List.last state.numbers |> unwrap
                            state.numbers
                            |> replaceLast (10*prevDigit + thisDigit)
                            |> unwrap,
                        index:
                            state.index
                            |> Dict.insert {x:x, y:y} ((List.len state.numbers) - 1), # modifiying last in numbers list so use length-1
                        x: state.x + 1,
                        y: state.y,
                        prev: Digit }
                    _ -> { # new number
                            numbers: state.numbers |> List.append thisDigit },
                            index:
                                state.index
                                |> Dict.insert {x:x, y:y} (List.len state.numbers), # appending to numbers list so use length
                            x: state.x + 1,
                            y: state.y,
                            prev: Digit }
            else if elem == "\n" then
                { state & y: state.y + 1, x: 0, prev: Newline}
            else if elem == "." then
                { state & x: state.x + 1 }
            else
                { state & x: state.x + 1 }

        |> \parsed ->
            {numbers: parsed.numbers, index: parsed.index}

            

    ## Parse all the numbers into a list and create an index into the list ke
    ## Generate a graph which connects symbols to digits and digits to other digits
    next =
        lines
        |> List.walkWithIndex [] \outer, line, i ->

            prevLine = List.get linesPad (i+0) |> unwrap
            currLine = List.get linesPad (i+1) |> unwrap
            nextLine = List.get linesPad (i+2) |> unwrap

            thing = List.walkWithIndex line [] \inner, cell, j ->
                ## number cells around current cell
                dbg cell
                cell1 = List.get prevLine (j+0) |> unwrap
                cell2 = List.get prevLine (j+1) |> unwrap
                cell3 = List.get prevLine (j+2) |> unwrap
                cell4 = List.get currLine (j+0) |> unwrap
                cell5 = List.get currLine (j+1) |> unwrap ## Current cell
                cell6 = List.get currLine (j+2) |> unwrap
                cell7 = List.get nextLine (j+0) |> unwrap
                cell8 = List.get nextLine (j+1) |> unwrap
                cell9 = List.get nextLine (j+2) |> unwrap

                this =
                    if (cell9 == "*") then
                        "*"
                    else
                        "."

                dbg this
                List.append inner this

            dbg thing
            List.append outer thing

    next
    |> List.map (\l -> Str.joinWith l "")
    |> Str.joinWith "\n"

func2 = \_ -> ""

testInput1 = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
testInput2 = testInput1

main =
    task =
        input <- Path.fromStr "data/day03.txt" |> File.readUtf8 |> Task.await

        test1 = func1 testInput1
        ##part1 = func1 input
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

