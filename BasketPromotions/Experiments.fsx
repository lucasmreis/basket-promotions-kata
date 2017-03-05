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
        promoQty = 3us
        promoPrice = 5
    }
}

let events = [
  AddToBasket(productA, 5us)
  AddToBasket(productB, 7us)
  AddToBasket(productA, 4us)
  AddToBasket(productB, 1us)
]

let myBasket = List.fold update empty events
