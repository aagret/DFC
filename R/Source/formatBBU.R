
# format data for Bloomberg BBU uplad
formatBBU <- function(db1= secPos, db2= allCash) {
    
    db1 <- db1[, .(Port, Date, Ticker, Amount), ]
    db1[, Price:=numeric()]
    
    db2 <- melt(db2[, .(Date, Ccy, TotFees, TotTax, OffsetCash)], 
                id.vars= c("Date", "Ccy"), 
                mesure.vars= c( "Fees", "Tax", "OffsetCash"),
                variable.factor= FALSE)
    db2 <- db2[value !=0]
    
    db2[, Price:= numeric()]
    db2[, Port:=  "DF EQUITY"]
    
    db2[variable == "OffsetCash", variable:= paste(Ccy, "Curncy", sep=" ")]
    
    db2[variable == "TotFees", ':=' (variable= paste(".FEE_", Ccy, " LX", sep=""),
                                  Price= -value,
                                  value= -1)]
    
    
    db2[variable == "TotTax", ':=' (variable= paste(".TAX_", Ccy, " LX", sep=""),
                                 Price= -value,
                                 value= -1)]
    
    db2[, Ccy:=NULL]
    colnames(db2)[2:3] <- c("Ticker", "Amount")
    
    db <- rbind(db1, db2)
    
}
