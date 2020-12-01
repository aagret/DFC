
# create recap sheet of all cash movements in the fund
getAllCash <- function(db= db1, ...){
    
    argg <- c(as.list(environment()), list(...))
    
    db <- setDT(ldply(argg))
    db <- dcast(db, Date + Ccy ~Type ,
                #fun.aggregate = sum,
                value.var= "Amount")
    
    setkey(db, Date)
    
    locf <- c("Cash", "AccruedFees", "PendingDividend", "PendingWh_Tax" )
    db[, (locf):= lapply(.SD, na.locf, na.rm=FALSE),
       by= Ccy, 
       .SDcols= locf]
    
    db[is.na(db)] <- 0
    
    cum <- c("NetDividend", "PaidFees", "Wh_Tax")
    db[, (cum):= lapply(.SD, function(x) cumsum(x)),
       by= Ccy,
       .SDcols= cum]

    db[, CashInNav:=  Cash + AccruedFees + Margin, by= Ccy]
    db[, TotTax:=     Wh_Tax, by= Ccy]
    db[, TotFees:=    AccruedFees + PaidFees, by= Ccy]
    db[, OffsetCash:= CashInNav - TotTax - TotFees,   by= Ccy]

    # carry over previous values
    dts <- unique(db$Date)
    
    for (dt in 2:length(dts)) {
        
        now  <- db[Date==dts[dt]]
        prev <- db[Date==dts[dt-1]]
        
        missing <- prev[!Ccy %in% now$Ccy]
        missing$Date <- dts[dt]
        
        db <- rbind(db, missing)
        setkey(db, Date)
    }
    
    return(db)
    
}
