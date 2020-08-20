
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
    
    
    # nonCum <- c("NetDividend", "PaidFees",
    #             "PaidInterest", "SubRed")
    # 
    # db[, (nonCum):= lapply(.SD, function(x){ x[is.na(x)] <-0; x }), by= Ccy, .SDcols= nonCum]
    # 
    # cum <- colnames(db)[!colnames(db) %in% nonCum]
    # db[, (cum):= lapply(.SD, function(x) {na.locf(x, na.rm=FALSE)}), by= Ccy, .SDcols= cum]
    # 
    # 
    # db[is.na(db)] <- 0
    # 
    db[, CashInNav:=  Cash + AccruedFees + PendingDividend, by= Ccy]
    db[, TotTax:=     PendingWh_Tax + Wh_Tax, by= Ccy]
    db[, TotFees:=    AccruedFees + PaidFees, by= Ccy]
    db[, OffsetCash:= CashInNav - TotTax - TotFees,   by= Ccy]
    # 
    # # calc all taxes
    # db[, Tax:= na.locf( PendingWh_Tax), by= .(Ccy)]
    # db[, Tax:= diff(c(0, PendingWh_Tax)), by= .(Ccy)]
    # db[, Tax:= min(0, Tax), by= .(Date, Ccy)]
    # db[, Tax:= cumsum(Tax), by= Ccy]
    
    # # calc all Fees
    # db[, Fees:= diff(c(0, AccruedFees)), by= Ccy]
    # db[, Fees:= min(0, Fees), by= .(Ccy, Date)]
    # db[, Fees:= cumsum(Fees), by= Ccy]
    # 
    # db[, Fees2:= Fees+PaidFees, by=Ccy]
    # db[, Fees3:= Fees+ shift(PaidFees, 2), by=Ccy]
    # 
    # # calc offset cash
    # db[, OffsetCash:= Cash + Margin + PendingDividend - Tax - Fees + AccruedFees + PendingDividend , by= Ccy]
    
    
    return(db)
    
}
