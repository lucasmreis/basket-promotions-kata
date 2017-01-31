module Domain

// DOMAIN
type Qty = Qty of int
type Price = Price of int
type Sku = Sku of string

let zero = Price 0

let opQty op q1 q2 =
    match q1, q2 with
    | Qty v1, Qty v2 -> Qty (op v1  v2)

let sumQty = opQty (+)

let minusQty = opQty (-)

let divideQty = opQty (/)

let modQty = opQty (%)

let lgQty quantity value =
    match quantity, value with
    | Qty q, i -> q > i

let opPrice op p1 p2 =
    match p1, p2 with
    | Price v1, Price v2 -> Price (op v1 v2)

let sumPrice = opPrice (+)

let minusPrice = opPrice (-)

let mult q p =
    match q, p with
    | Qty q, Price v -> Price (v * q)

type Promotion = {
    quantity: Qty
    price: Price
}

type Product = {
    sku: Sku
    price: Price
    promotion: Promotion option
}

type Event =
    | AddedToBasket of Product * Qty

// READ MODEL

type ViewTotal =
    | Simple of Price
    | Promotioned of before: Price * after: Price * discount: Price

type PromotionSuggestion = {
    quantityToAdd: Qty
    possibleAddedDiscount: Price
}

type Line = {
    sku: Sku
    quantity: Qty
    total: ViewTotal
    promotionSuggestion: PromotionSuggestion option
}

type Basket = {
    lines: Line list
    total: ViewTotal
}

let findLine sku basket =
    List.tryFind (fun l -> l.sku = sku) basket.lines

let promotionedPrice product quantity =
    match product.promotion with
    | Some promo ->
        let promoQty = divideQty quantity promo.quantity
        let regularQty = modQty quantity promo.quantity

        let beforePrice = mult quantity product.price
        let afterPrice = sumPrice (mult promoQty promo.price) (mult regularQty product.price)
        let discount = minusPrice afterPrice beforePrice

        if beforePrice <> afterPrice
        then Promotioned(beforePrice, afterPrice, discount)
        else Simple afterPrice

    | None -> Simple (mult quantity product.price)

let suggestion product quantity =
    match product.promotion with
    | Some promo ->
        let promoQty = divideQty quantity promo.quantity
        let regularQty = modQty quantity promo.quantity

        if lgQty regularQty 0
        then Some({ quantityToAdd = minusQty promo.quantity regularQty ; possibleAddedDiscount = minusPrice promo.price (mult promo.quantity product.price) })
        else None

    | None -> None

let sumTwoTotals t1 t2 =
    match t1, t2 with
    | Simple v1, Simple v2 -> Simple (sumPrice v1 v2)
    | Simple v, Promotioned (b, a, d) -> Promotioned (sumPrice b v, sumPrice a v, d)
    | Promotioned (b, a, d), Simple v -> Promotioned (sumPrice b v, sumPrice a v, d)
    | Promotioned (b1, a1, d1), Promotioned (b2, a2, d2) ->
        let before = sumPrice b1 b2
        let after = sumPrice a1 a2
        Promotioned (before, after, minusPrice after before)

let sumTotal (lines: Line list) =
    lines
    |> List.map (fun l -> l.total)
    |> List.fold sumTwoTotals (Simple zero)

let addToBasket (product: Product) (quantity: Qty) basket =
    let existingLine = findLine product.sku basket

    let line =
        match existingLine with
        | Some l ->
            let newQuantity = sumQty l.quantity quantity
            {
                l with
                    quantity = newQuantity
                    total = promotionedPrice product newQuantity
                    promotionSuggestion = suggestion product newQuantity }
        | None ->
            {
                sku = product.sku
                quantity = quantity
                total = promotionedPrice product quantity
                promotionSuggestion = suggestion product quantity }

    let lines =
        match existingLine with
        | Some _ -> basket.lines |> List.map (fun l -> if l.sku = line.sku then line else l)
        | None -> line::basket.lines

    let total = sumTotal lines

    { basket with lines = lines ; total = total }

let update basket event =
    let newBasket =
        match event with
        | AddedToBasket(p, q) -> addToBasket p q basket
    printfn "NEW BASKET: %A\n\n\n--------------------------------\n\n" newBasket
    newBasket

// Product + Event AddToBasket + ViewBasket com lines e total > addToBasket

// BasketPromotion

// Qty ; Price + Sku para single case DU

// promotionSuggestion
