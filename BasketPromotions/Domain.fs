module Domain

type Sku = string
type Price = int
type Qty = int

type Product = {
    sku: Sku
    price: Price
}

type Event =
    | AddToBasket of Product * Qty

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

let buildLine product quantity = {
    productSku = product.sku
    quantity = quantity
    lineTotal = quantity * product.price
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
