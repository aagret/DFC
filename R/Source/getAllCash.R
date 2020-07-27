
# create recap sheet of all cash movements in the fund
getAllCash <- function(db= db1, ...){
    
    argg <- c(as.list(environment()), list(...))
    
    db <- setDT(ldply(argg))
    db <- dcast(db, Date + Ccy ~Type ,
                #fun.aggregate = sum,
                value.var= "Amount")
    
    setkey(db, Date)
    
    nonCum <- c("NetDividend", "PaidFees", "PendingDividend", "PaidInterest", 
                "SubRed",  "Wh_Tax")
    
    db[, (nonCum):= lapply(.SD, function(x){ x[is.na(x)] <-0; x }), .SDcols= nonCum]
    
    cum <- colnames(db)[!colnames(db) %in% nonCum]
    db[, (cum):= lapply(.SD, function(x) {na.locf(x, na.rm=FALSE)}), by= Ccy, .SDcols= cum]
    
    
    db[is.na(db)] <- 0
    
    # calc all taxes
    db[, Tax:= na.locf( PendingWh_Tax), by= .(Ccy)]
    db[, Tax:= diff(c(0, PendingWh_Tax)), by= .(Ccy)]
    db[, Tax:= min(0, Tax), by= .(Date, Ccy)]
    db[, Tax:= cumsum(Tax), by= Ccy]
    
    # calc all Fees
    db[, Fees:= diff(c(0, AccruedFees)), by= Ccy]
    db[, Fees:= min(0, Fees), by= .(Ccy, Date)]
    db[, Fees:= cumsum(Fees), by= Ccy]
    
    # calc offset cash
    db[, OffsetCash:= Cash - Tax - Fees + AccruedFees + PendingDividend + Margin, by= Ccy]
    
    
    return(db)
    
}
