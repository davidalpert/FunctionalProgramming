module MaybeTests

open NUnit.Framework
open FsUnit

[<Test>]
let ``sample fsunit test``()=
    false |> should equal false
