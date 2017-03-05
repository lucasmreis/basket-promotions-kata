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

        testProperty "adding product multiple times down to the basket" <| fun (N : Qty ) ->
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
            let price = 10
            let promotedPrice = 7

            let promoQty = N + 2us
            let promotion = { promoQty = promoQty ; promoPrice = promotedPrice }
            let promoted = promotedTotal promoQty price promotion

            let notPromoQty = N + 1us
            let notPromoted = promotedTotal notPromoQty price promotion

            let promotedExpected = Promoted(promoQty * price, promoQty * price - promotedPrice , promotedPrice)
            let notPromotedExpected = NotPromoted(notPromoQty * price)

            Expect.equal promoted promotedExpected "same price as promotion"
            Expect.equal notPromoted notPromotedExpected "multiplied by regular price"

        testProperty "not promoted products added to not promoted" <| fun (N : Qty) ->
            let initial = {
                lines = [{ productSku = "a" ; quantity = 3us ; lineTotal = NotPromoted 30 }]
                total = NotPromoted 30
            }
            let prod = { sku = "sku" ; price = 10 ; promotion = None }
            let event = AddToBasket(prod, 1us)

            let basket =
                [1..(int N + 1)]
                |> List.map (fun _ -> event)
                |> List.fold update initial

            let isNotPromoted =
                match basket.total with
                | NotPromoted _ -> true
                | Promoted _ -> false

            Expect.isTrue isNotPromoted "should stay not promoted"

        testProperty "not promoted products added to promoted" <| fun (N : Qty) ->
            let initial = {
                lines = [{ productSku = "a" ; quantity = 3us ; lineTotal = Promoted(30, 11, 19) }]
                total = Promoted(30, 11, 19)
            }
            let prod = { sku = "sku" ; price = 10 ; promotion = None }
            let event = AddToBasket(prod, 1us)

            let basket =
                [1..(int N + 1)]
                |> List.map (fun _ -> event)
                |> List.fold update initial

            let isPromoted =
                match basket.total with
                | NotPromoted _ -> false
                | Promoted(_, discount, _) ->
                    Expect.equal discount 11 "discount is the same"
                    true

            Expect.isTrue isPromoted "should stay promoted"
    ]
