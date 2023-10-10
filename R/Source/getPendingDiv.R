
# extract from NAV data the (ex)Dividend announced but not yet paid
getPendingDiv <- function(db= nav) {
    
    db <- db[Cat =="CPON", .(Type= "PendingDividend",
                             Amount= sum(Amount)), by= .(Date, Port, Ccy, Name)]
    
    ptax <- calcPaidTax(db)[,-7]
    
    db <- rbind(ptax, db)
    
    # remove Wh calc as dividned is accounted net of expected tax
    db[Type == "Wh_Tax", Amount:=0]
    
    db[Type == "Wh_Tax", Type:="PendingWh_Tax"]
}
