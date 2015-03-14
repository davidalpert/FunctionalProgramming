namespace FSTests

open NUnit.Framework
open FsUnit

module ConsListTests =

    // F# has linked immutable lists
    // as a native data structure.
    let MkList seq =
        seq
        |> List.ofSeq
        |> List.rev
        |> List.fold (fun lst t -> t :: lst) []

    let TestAsEnumerable xs =
        let expected = xs |> Seq.ofList
        let list = MkList xs
        printfn "%A" list
        let result = list |> Seq.ofList
        result |> should equal expected

    [<Test>]
    let ``Test As Enumerable #1``()=
        TestAsEnumerable []

    [<Test>]
    let ``Test As Enumerable #2``()=
        TestAsEnumerable [1;2;3;]

    [<Test>]
    let ``Test As Enumerable #3``()=
        TestAsEnumerable [1;2;3;4;5;6;]

    let TestConcat fst snd =
        let expected = List.concat [fst; snd;]
        let list1 = MkList fst
        let list2 = MkList snd
        let result = List.concat [list1; list2;]
        result |> should equal expected

    [<Test>]
    let ``Test Concat #1``()=
        TestConcat [] []

    [<Test>]
    let ``Test Concat #2``()=
        TestConcat [] [1;2;3;]

    [<Test>]
    let ``Test Concat #3``()=
        TestConcat [1;2;3;] []

    [<Test>]
    let ``Test Concat #4``()=
        TestConcat [1;2;3;] [4;5;6;]

    [<Test>]
    let ``Test As Enumerable``()=
        let expected = [1; 2; 3;] |> Seq.ofList
        let actual = 1 :: (2 :: (3 :: [])) |> Seq.ofList

        actual |> should equal expected

