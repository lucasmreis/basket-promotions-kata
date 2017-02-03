#load "Domain.fs"

open Domain

let productA = {
    sku = "a"
    price = 1
    promotion = None
}

let productB = {
    sku = "b"
    price = 2
    promotion = Some {
        promoQty = createQty 3
        promoPrice = 5
    }
}

let events = [
  AddToBasket(productA, createQty 5)
  AddToBasket(productB, createQty 7)
  AddToBasket(productA, createQty 4)
]

let myBasket = List.fold update empty events
