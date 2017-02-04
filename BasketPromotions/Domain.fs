module Domain

type Sku = string
type Price = int
type Qty = uint16

let createQty (n : int) : Qty =
    if n < 0 then (uint16 0) else (uint16 n)

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

 // VIEW MODELS

type Line = {
    productSku: Sku
    quantity: Qty
    lineTotal: Price
}

type Basket = {
    lines: Line list
    total: Price
}

let empty = { lines = [] ; total = 0 }

// super cool custom operator!
let (*) (qty : Qty) (price : Price) : Price =
    int qty * price

let promotedTotal quantity price promotion =
    let promotedQty = quantity / promotion.promoQty
    let promotedTotal = promotedQty * promotion.promoPrice

    let notPromotedQty = quantity % promotion.promoQty
    let notPromotedTotal = notPromotedQty * price

    promotedTotal + notPromotedTotal

let lineTotal quantity product =
    match product.promotion with
    | None -> quantity * product.price
    | Some promotion -> promotedTotal quantity product.price promotion

let buildLine product quantity = {
    productSku = product.sku
    quantity = quantity
    lineTotal = lineTotal quantity product
}

let basketTotal lines =
    lines
    |> List.map (fun l -> l.lineTotal)
    |> List.sum

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
