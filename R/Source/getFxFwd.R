
# retrieve cash Balances from KBL files
getFxFwd <- function(fileList= fileList) {
    
    fileSelect <- fileList[grepl("SolCptCsh", fileList)]
    
    db <- getData(fileSelect, c(1, 3))[grep("K0124100", ClientID)]
    db <- db[AccountType %in% c("FWD"), .(ClientID, ValueDate, 
                                               Accountcurrency, BalanceatValueDate)]
    
    colnames(db) <- c("Port", "Date", "Ccy", "Amount")
    
    db[, ':=' (Port=  "DF EQUITY",
               Date= as.Date(Date, "%Y-%m-%d"),
               Type= "Cash",
               Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    setDT(db, Date)
    
    db <- db[, sum(Amount), by= .(Port, Date, Type, Ccy)]
    
    colnames(db)[5] <- "Amount"
    
    return(db)
    
}
