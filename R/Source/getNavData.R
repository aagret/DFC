
# extract datas from administrator weekly NAV file
getNavData <- function(fileList= fileList) {
    
    # get all positions from NAV
    fileSelect <- fileList[grepl("ffpos1", fileList)]
    
    db <- ldply(fileSelect, fread, quote="")
    db <- setDT(db[, c(8:11, 16, 22, 36, 46:54)])
    
    # format Nav data
    colnames(db) <- c("Date", "Code", "Cat", "Name", "Ccy", "Isin", "Type", "EurAMount",
                      "Amount", "Position", "Cost", "EurCost", "Accrued","EurAccrued",
                      "PrepaidInterest", "EurPrepaidInterest")
    
    db[, ':=' (Port=  "DF Equity",
               Date= as.Date(Date, format= "%d/%m/%Y"))]
    
    setkey(db, Date, Ccy)
    
}
