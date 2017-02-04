module Tests

open Expecto

open Domain

[<Tests>]
let tests =
    testList "basket promotions" [
        testProperty "total of single line" <| fun (quantity : Qty ) (price : Price) ->
            let prod = { sku = "a" ; price = price ; promotion = None }
            let event = AddToBasket(prod, quantity)
            let basket = update empty event

            let lineTotal = basket.lines.Head.lineTotal
            let basketTotal = basket.total

            Expect.equal lineTotal basketTotal "must be the same as basket total"

        testProperty "adding product multiple times downto the basket" <| fun (N : Qty ) ->
            let prod = { sku = "sku" ; price = 10 ; promotion = None }
            let event = AddToBasket(prod, 1us) // notation for uint16
            let basket =
                [1..(int N + 1)]
                |> List.map (fun _ -> event)
                |> List.fold update empty

            Expect.equal basket.lines.Length 1 "must have one line"

        testProperty "adding multiple products to the basket" <| fun (N : Qty ) ->
            let prod (num : int) = { sku = "sku" + num.ToString() ; price = 10 ; promotion = None }
            let event (num : int) = AddToBasket(prod num, 1us)
            let basket =
                [1..(int N + 1)]
                |> List.map event
                |> List.fold update empty

            Expect.equal basket.lines.Length (int N + 1) "must have N lines"

        testProperty "promoted line total" <| fun (N : Qty) ->
            let promoQty = N + 2us
            let promotion = { promoQty = promoQty ; promoPrice = 7 }
            let promoted = promotedTotal promoQty 10 promotion

            let notPromoQty = N + 1us
            let notPromoted = promotedTotal notPromoQty 10 promotion

            let promotedExpected = 7
            let notPromotedExpected = notPromoQty * 10

            Expect.equal promoted promotedExpected "same price as promotion"
            Expect.equal notPromoted notPromotedExpected "multiplied by regular price"
    ]
