module Domain

type Sku = string
type Price = int
type Qty = uint16

let createQty (n : int) =
    if n < 0 then (uint16 0) else (uint16 n)

type Promotion = {
    promoQuantity: Qty
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

let qtyTimesPrice (quantity : Qty) (price : Price) =
    let (result : Price) = int quantity * price
    result

let promotionTotal promotion price quantity =
    let promotionedQty = quantity / promotion.promoQuantity
    let regularQty = quantity % promotion.promoQuantity
    qtyTimesPrice regularQty price + qtyTimesPrice promotionedQty promotion.promoPrice

let lineTotal quantity product =
    match product.promotion with
    | None -> qtyTimesPrice quantity product.price
    | Some promo -> promotionTotal promo product.price quantity

let buildLine product quantity = {
    productSku = product.sku
    quantity = quantity
    lineTotal = qtyTimesPrice quantity product.price
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
