
# retrieve cash Balances from KBL files
getCashPos <- function(fileList= fileList) {
    
    fileSelect <- fileList[grepl("SolCptCsh", fileList)]
    
    db <- getData(fileSelect, c(1, 3))[grep("K0124100", ClientID)]
    db <- db[AccountType %in% c("CC", "IV", "FWD"), .(ClientID, ValueDate, 
                                               Accountcurrency, get('FuturePositionD+2') )] #BalanceatValueDate get('FuturePositionD+2')
    
    colnames(db) <- c("Port", "Date", "Ccy", "Amount")
    
    db[, ':=' (Port=  "DF Equity",
               Date= as.Date(Date, "%Y-%m-%d"),
               Type= "Cash",
               Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    db <- db[Date > as.Date("2020-07-15"), ]
    

    ## READ new historical file from 2018-11-02 to 2020-07-15
    fileSelect <- fileList[grepl("HistoCashPositions", fileList)]
    
    
    histo <- fread(fileSelect,select = c(1:4))
    
    colnames(histo) <- c("Port", "Date", "Ccy", "Amount")
    
    histo[, ':=' (Date= as.Date(Date, "%d.%m.%Y"),
                  Type= "Cash"), ]
    #,
     #             Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    db <- rbind(histo,db)
    
    db <- db[, sum(Amount), by= .(Date, Type, Ccy)]
    colnames(db)[4] <- "Amount"
    
    return(db)
    
}
