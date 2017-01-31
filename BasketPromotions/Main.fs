module BasketPromotions

open Expecto

open Domain

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
