module Client.Tests

open Fable.Mocha

open Index
open Shared

let all =
    testList "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Shared.Tests.shared
#endif
        ]

[<EntryPoint>]
let main _ = Mocha.runTests all