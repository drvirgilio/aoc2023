app "day02"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Path, pf.File, pf.Task]
    provides [main] to pf

unwrap : Result a * -> a
unwrap = \result ->
    when result is
        Ok x -> x
        Err _ -> crash "unwrap failed"

Game : { id: U64, dice: List {red: U64, green: U64, blue: U64} }

parse : Str -> List Game
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        split : List Str
        split = line
            |> Str.replaceEach "Game " ""
            |> Str.replaceEach " " ""
            |> Str.replaceEach "blue" "b"
            |> Str.replaceEach "red" "r"
            |> Str.replaceEach "green" "g"
            |> Str.split ":"

        ##dbg split

        id : U64
        id =
            List.first split
            |> Result.try Str.toU64
            |> unwrap
            
        second : Str
        second = when (List.last split) is
            Ok val -> val
            Err _ -> crash "oops!@#!@"

        dice : List {red: U64, green: U64, blue: U64}
        dice =
            second
            |> Str.split ";"
            |> List.walk [] \outerState, outerElem ->
                Str.split outerElem ","
                |> List.walk {red: 0, green: 0, blue: 0} \innerState, innerElem ->
                    if Str.endsWith innerElem "r" then
                        innerElem
                        |> Str.splitFirst "r"
                        |> unwrap
                        |> .before
                        |> Str.toU64
                        |> unwrap
                        |> \num -> {innerState & red: num}
                    else if Str.endsWith innerElem "g" then
                        innerElem
                        |> Str.splitFirst "g"
                        |> unwrap
                        |> .before
                        |> Str.toU64
                        |> unwrap
                        |> \num -> {innerState & green: num}
                    else if Str.endsWith innerElem "b" then
                        innerElem
                        |> Str.splitFirst "b"
                        |> unwrap
                        |> .before
                        |> Str.toU64
                        |> unwrap
                        |> \num -> {innerState & blue: num}
                    else
                        crash "string doesn't end in r, g, or b"
                |> \val -> List.append outerState val
        {id : id, dice: dice}

isPossible : Game -> Bool
isPossible = \game ->
    dice = game.dice
    List.walk dice Bool.true \state, elem ->
        r = elem.red <= 12
        g = elem.green <= 13
        b = elem.blue <= 14
        state && r && g && b

func1 = \input ->
    input
    |> parse
    |> List.keepIf isPossible
    |> List.map .id
    |> List.sum
    |> Num.toStr

power : Game -> U64
power = \game ->
    dice = game.dice
    d = List.walk dice {r:0, g:0, b:0} \state, elem ->
        r = Num.max state.r elem.red
        g = Num.max state.g elem.green
        b = Num.max state.b elem.blue
        {r:r, g:g, b:b}
    d.r * d.g * d.b

func2 = \input ->
    input
    |> parse
    |> List.map power
    |> List.sum
    |> Num.toStr


testInput1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"
testInput2 = testInput1

main =
    task =
        input <- Path.fromStr "data/day02.txt" |> File.readUtf8 |> Task.await

        test1 = func1 testInput1
        part1 = func1 input

        test2 = func2 testInput2
        part2 = func2 input

        expect test1 == "8"
        expect part1 == "2913"
        expect test2 == "2286"
        expect part2 == "55593"


        {} <- Task.await (Stdout.line "test1: \(test1)")
        {} <- Task.await (Stdout.line "part1: \(part1)")
        {} <- Task.await (Stdout.line "test2: \(test2)")
        {} <- Task.await (Stdout.line "part2: \(part2)")
        Stdout.write ""

    Task.onErr task \_ -> crash "Failed to read and parse input"
    
