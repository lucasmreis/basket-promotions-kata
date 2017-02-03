module Tests

open Expecto

open Domain

[<Tests>]
let tests =
    testList "basket promotions" [
        testProperty "total of single line" <| fun (quantity : Qty ) (price : Price) ->
            let prod = { sku = "a" ; price = price }
            let event = AddToBasket(prod, quantity)
            let basket = update empty event

            let lineTotal = basket.lines.Head.lineTotal
            let basketTotal = basket.total

            Expect.equal lineTotal basketTotal "must be the same as basket total"

        testProperty "adding product multiple times downto the basket" <| fun (N : uint16 ) ->
            let prod = { sku = "sku" ; price = 10 }
            let event = AddToBasket(prod, 1)
            let basket =
                [1..(int N + 1)]
                |> List.map (fun _ -> event)
                |> List.fold update empty

            Expect.equal basket.lines.Length 1 "must have one line"

        testProperty "adding multiple products to the basket" <| fun (N : uint16 ) ->
            let prod (num : int) = { sku = "sku" + num.ToString() ; price = 10 }
            let event (num : int) = AddToBasket(prod num, 1)
            let basket =
                [1..(int N + 1)]
                |> List.map event
                |> List.fold update empty

            Expect.equal basket.lines.Length (int N + 1) "must have N lines"
    ]
