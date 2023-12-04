app "day04"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Path, pf.File, pf.Task]
    provides [main] to pf

unwrap : Result a * -> a
unwrap = \result ->
    when result is
        Ok x -> x
        Err _ -> crash "unwrap failed"

Card : {id: Nat, wins: List Nat, haves: List Nat}

parse : Str -> List Card
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        id : Nat
        id =
            line
            |> Str.replaceFirst "Card " ""
            |> Str.split ":"
            |> List.first
            |> unwrap
            |> Str.trim
            |> Str.toNat
            |> unwrap

        wins : List Nat
        wins =
            line
            |> Str.split ":"
            |> List.last
            |> unwrap
            |> Str.split "|"
            |> List.first
            |> unwrap
            |> Str.split " "
            |> List.dropIf Str.isEmpty
            |> List.map \s ->
                Str.toNat s |> unwrap
            |> List.sortAsc

        haves : List Nat
        haves = 
            line
            |> Str.split ":"
            |> List.last
            |> unwrap
            |> Str.split "|"
            |> List.last
            |> unwrap
            |> Str.split " "
            |> List.dropIf Str.isEmpty
            |> List.map \s ->
                Str.toNat s |> unwrap
            |> List.sortAsc

        card = {id: id, wins: wins, haves: haves}
        card

countMatches : Card -> Nat
countMatches = \card ->
    winSet = Set.fromList card.wins
    haveSet = Set.fromList card.haves
    matchSet = Set.intersection winSet haveSet
    Set.len matchSet

func1 = \input ->
    cards = parse input
    counts = List.map cards countMatches
    points = List.map counts \n ->
        if n==0 then
            0
        else
            Num.powInt 2 (n-1)
    List.sum points
    |> Num.toStr

func2 = \input ->
    cards : List Card
    cards = parse input

    counts : List Nat
    counts = List.map cards countMatches

    points : List Nat
    points = List.walkBackwards counts [] \state, count ->
        point =
            state
            |> List.takeFirst count
            |> List.sum
            |> Num.add 1
        List.prepend state point

    Num.toStr (List.sum points)

testInput1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"
testInput2 = testInput1

main =
    task =
        input <- Path.fromStr "data/day04.txt" |> File.readUtf8 |> Task.await

        test1 = func1 testInput1
        part1 = func1 input
        test2 = func2 testInput2
        part2 = func2 input

        ##test1 = ""
        ##part1 = ""
        ##test2 = ""
        ##part2 = ""

        expect test1 == "13"
        expect part1 == "21158"
        expect test2 == "30"
        expect part2 == "6050769"

        {} <- Task.await (Stdout.line "test1: \(test1)")
        {} <- Task.await (Stdout.line "part1: \(part1)")
        {} <- Task.await (Stdout.line "test2: \(test2)")
        {} <- Task.await (Stdout.line "part2: \(part2)")
        Stdout.write ""

    Task.onErr task \_ -> crash "Failed to read and parse input"

