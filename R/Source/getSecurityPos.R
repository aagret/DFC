

# retrieve security positions from KBL files
getSecurityPos <- function(fileList= fileList) {
    
    fileSelect <- fileList[grepl("SolCptTit", fileList)]
    
    db <- getData(fileSelect, 1)[grep("K0124100", ClientID),
                                 .(ClientID, ValueDate, Balance,
                                   KBLCodeidentifyingSecurityused,
                                   ISINCode, DescriptionofSecurity, 
                                   CurrencyCode)][Balance != "0,00",]
    
    #if no Isin use Kb number
    db[ISINCode == "", ISINCode:= KBLCodeidentifyingSecurityused]
    db$KBLCodeidentifyingSecurityused <- NULL
    
    colnames(db) <- c("Port", "Date", "Amount", "Isin", "Description", "Ccy")
    
    db[, ':=' (Port=  "DF EQUITY",
               Date= as.Date(Date, "%Y-%m-%d"),
               Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    db <- db[Isin != "SE0012455681", ]
    
    db <- addTicker(db)
    
}
