
# calc Whitholdding taxe based on net Dividend
calcPaidTax <- function(db= cashMvmt) {
    
    db <- db[grep("Dividend", Type), ]
    db <- db[Ccy == "GBP", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0)]
    db <- db[Ccy == "USD", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0.3 / 0.7)]
    db <- db[Ccy == "CHF", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0.35 / 0.65)]
    db <- db[Ccy == "EUR", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0.35 / 0.65)]
    db <- db[Ccy == "DKK", ':=' (Type=  "Wh_Tax", 
                                 Amount=  -Amount * 0.37 / 0.63)]
    db <- db[Ccy == "SEK", ':=' (Type=  "Wh_Tax", 
                                 Amount=  -Amount * 0.3 / 0.7)]
    db <- db[Ccy == "NOK", ':=' (Type=  "Wh_Tax", 
                                 Amount=  -Amount * 0.25 / 0.75)]

}
