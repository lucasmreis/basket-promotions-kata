module Domain

type Sku = string
type Price = int
type Qty = uint16

type Promotion = {
    promoQty: Qty
    promoPrice: Price
}

type Product = {
    sku: Sku
    price: Price
    promotion: Promotion option
}

type Event =
    | AddToBasket of Product * Qty

 // READ MODELS

type ReadTotal =
    | NotPromoted of Price
    | Promoted of original: Price * discount: Price * final: Price

type Line = {
    productSku: Sku
    quantity: Qty
    lineTotal: ReadTotal
}

type Basket = {
    lines: Line list
    total: ReadTotal
}

let empty = { lines = [] ; total = NotPromoted 0 }

// super cool custom operator!
let (*) (qty : Qty) (price : Price) : Price =
    int qty * price

let promotedFinalTotal quantity price promotion =
    let promotedQty = quantity / promotion.promoQty
    let promotedTotal = promotedQty * promotion.promoPrice

    let notPromotedQty = quantity % promotion.promoQty
    let notPromotedTotal = notPromotedQty * price

    promotedTotal + notPromotedTotal

let promotedTotal quantity price promotion =
    let final = promotedFinalTotal quantity price promotion
    let original = quantity * price

    if final <> original
    then Promoted(original, original - final, final)
    else NotPromoted(final)

let lineTotal quantity product =
    match product.promotion with
    | None -> NotPromoted(quantity * product.price)
    | Some promotion -> promotedTotal quantity product.price promotion

let buildLine product quantity = {
    productSku = product.sku
    quantity = quantity
    lineTotal = lineTotal quantity product
}

let sumTotals t1 t2 =
    match t1, t2 with
    | NotPromoted v1, NotPromoted v2 -> NotPromoted(v1 + v2)
    | NotPromoted v, Promoted(o, d, f) -> Promoted(v + o, d, v + f)
    | Promoted(o, d, f), NotPromoted v -> Promoted(v + o, d, v + f)
    | Promoted(o1, d1, f1), Promoted(o2, d2, f2) -> Promoted(o1 + o2, d1 + d2, f1 + f2)

let basketTotal lines =
    lines
    |> List.map (fun l -> l.lineTotal)
    |> List.fold sumTotals (NotPromoted 0)

let addToBasket product quantity basket =
    let transformLine line =
        if line.productSku = product.sku
        then buildLine product (line.quantity + quantity)
        else line

    let transformedLines =
        basket.lines
        |> List.map transformLine

    let productAlreadyInBasket =
        transformedLines <> basket.lines

    let lines =
        if productAlreadyInBasket
        then transformedLines
        else (buildLine product quantity)::basket.lines

    { basket with lines = lines ; total = basketTotal lines }

let update basket event =
    match event with
    | AddToBasket(product, quantity) ->
        addToBasket product quantity basket
