
# extract Accrued Fees from NAV datas
getAccruedFees <- function(db= nav) {
    
    db <- db[Cat == "TRES",]
    db <- db[!grep('DC|BA|CD100' , Code), ] #, 1, 2) %in% c("DC", "BA", "CD100"), ]
    db <- db[, .(Type= "AccruedFees",
                 Amount=sum(Amount)), by= .(Date, Port, Ccy)]
} 
