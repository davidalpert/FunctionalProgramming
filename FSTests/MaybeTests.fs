namespace FSTests

open NUnit.Framework
open FsUnit

module MaybeTests =

    // F# comes with a built-in Option<'a> type:
    //
    //    type Option<'a> =      // use a generic definition
    //    | Some of 'a           // valid value
    //    | None                 // missing *)
    //
    // This is a discriminated union that can be
    // either Some(value) or None.
    //
    // A function that returns 'Some of int' or 'None' will be
    // inferred to have a return type of 'int option':
    //
    let SafeDivide i =
        match i%2 with
        | 0 -> Some(i/2)
        | _ -> None

    // In conjunction with discriminated unions
    // F# provides some powerful pattern matching
    // with active paterns that can match inside
    // expressions without the need for Linq syntax
    // or the IEnumerable monad.
    //
    [<Test>]
    let ``Test Select Just``()=
        let result = match Some 5 with
                     | Some(n) when n = 5 -> true
                     | _                  -> false

        result |> should equal true

    // Because SafeDivide returns an Option we have to match the
    // result to extract the int before we can call it again.
    [<Test>]
    let ``Test Select Many Nothing Result``()=
        let expected = Option<int>.None
        let a = SafeDivide 10
        let b = match a with
                | Some(i) -> SafeDivide i
                | _       -> None
        let result = b

        result |> should equal expected

    // If, on the other hand, we define SafeDivide to operate
    // on an 'int option', then we can call it on its result
    // directly:
    let SafeDivide2 o =
        match o with
        | Some(i) when i%2 = 0 -> Some(i/2)
        | _                    -> None

    [<Test>]
    let ``Test Select Many Nothing Result #2``()=
        let expected = Option<int>.None
        let a = SafeDivide2 (Some 10)
        let b = SafeDivide2 a
        let result = b

        result |> should equal expected

    // For brevity, in idiomatic F# this can be rewritten
    // without the intermediate values by piping the output
    // of the first call directly as input to the second:
    [<Test>]
    let ``Test Select Many Nothing Result #3``()=
        let expected = None
        let result = SafeDivide2 (Some 10)
                     |> SafeDivide2

        result |> should equal expected

    // Even cleaner, and without the need for that
    // SafeDivide meta function, the Option<'a> type
    // includes map and bind functions that dereference
    // the option value for us, depending on whether
    // the function we want to apply returns an
    // Option (bind) or not (map):
    [<Test>]
    let ``Test Select Many Nothing Result #4``()=
        let expected = None
        let result = SafeDivide 10
                     |> Option.bind SafeDivide

        result |> should equal expected

    [<Test>]
    let ``Test Select Many Just Result``()=
        let expected = Some 5
        let result = SafeDivide 20
                     |> Option.bind SafeDivide

        result |> should equal expected

    // Option.map operates on Option instances like
    // the Linq Select method operates on the IMaybe
    // monad of the FunctionalProgramming library:
    [<Test>]
    let ``Test Equality Just``()=
        let expected = Some 5
        let result = Some 4 |> Option.map (fun n -> n + 1)

        result |> should equal expected

    [<Test>]
    let ``Test Inequality Just``()=
        let expected = Some 4
        let result = Some 4 |> Option.map (fun n -> n + 1)

        result |> should not' (equal expected)

    // The nothing case can be expressed with similar
    // syntax as Maybe.Nothing<int>():
    [<Test>]
    let ``Test Just Nothing Inequality``()=
        let expected = Some 4
        let result = Option<int>.None

        result |> should not' (equal expected)

    // And with F# type inference, declaring the equivalent
    // of a Maybe.Nothing<int>() or Option<int>.None is often
    // trivial:
    [<Test>]
    let ``Test Just Nothing Inequality (with type inference)``()=
        let expected = Some 4
        let result = None

        result |> should not' (equal expected)
