namespace FSTests

open System.Collections.Generic
open NUnit.Framework
open FsUnit

module DictionaryExtensionsTests =

    [<Test>]
    let ``Test Missing Key (as tuple)``()=
        let data = new Dictionary<string,string>()
        data.Add("a", "1")
        data.Add("b", "2")
        data.Add("d", "4") // no "c"!

        let expected = None
        let actual = data.TryGetValue("c")

        // In F#, calling a tryget w/o the final reference param
        // returns a tuple of (bSuccess, result)
        let (success,value) = actual

        success |> should equal false


    // In F#, it's pretty easy to define a helper function
    // (the complicated syntax of the dict param is a type
    // annotation to explicitly tell the F# compiler the
    // type of dic) to wrap the dictorary access and
    // return an Option:
    let getAsOption key (dict:Dictionary<_,_>) =
        match dict.TryGetValue(key) with
        | (true,value) -> Some value
        | (false,_)    -> None

    [<Test>]
    let ``Test Missing Key (as option)``()=
        let data = new Dictionary<string,string>()
        data.Add("a", "1")
        data.Add("b", "2")
        data.Add("d", "4") // no "c"!

        let expected = None
        let actual = data |> getAsOption "c"

        actual |> should equal expected

