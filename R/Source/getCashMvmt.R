
#retrieve cash movements from KBL files
getCashMvmt <- function(fileList= fileList) {
    
    
    ## READ new historical file from 2018-11-02 to 2020-07-15
    fileSelect <- fileList[grepl("HistoCashTransactions", fileList)]
    
    
    db <- fread(fileSelect, skip = 1, fill=TRUE)[,c(2,6:11,13)]
    
    colnames(db) <- c("Port", "Ccy", "Type", "Ref", "Name", 
                      "Value", "Date", "Amount")
    
    db[, ':=' (Port=  "DF EQUITY",
               Value= as.Date(Value, "%d.%m.%Y"),
               Date=  as.Date(Date,  "%d.%m.%Y")), ]
    
    
    
    
    ##read daily KBL files
    fileSelect <- fileList[grepl("CshStmt_JRN", fileList)]
    kbl <- getData(fileSelect)[grep("EQUITY FUND", ClientName), 
                             c(2, 6:11, 13)]
    
    colnames(kbl) <- c("Port", "Ccy", "Type", "Ref", "Name", 
                      "Value", "Date", "Amount")

    kbl[, ':=' (Port=  "DF EQUITY",
               Value= as.Date(Value, "%Y-%m-%d"),
               Date=  as.Date(Date,  "%Y-%m-%d"),
               Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    
    # remove old kbl datas inclueded in db histo
    
    kbl <- kbl[Date > as.Date("2020-07-15"), ]
    
    # merge recent and histo datas
    db <- rbind(db,kbl)
    
    db <- db[!Type %in% c("OEM", "OER", "OCS", "ORG", "OSD", "ODC", "OPF", "OEA",
                          "BOC","CUS", "EMC", "MAF", "DCC", "DTC",
                          "MTM", "ICC", "TRI"),]
    
    db[Type == "IPC", Type:="PaidInterest"]
    db[Type == "OCD", Type:="NetDividend"]
    db[grep("Redemption", Name), Type:= "SubRed"]
    db[grep("from|FROM|SCR", Name), Type:= "SubRed"]
    db <- db[!Type %in% c("PaidInterest", "NetDividend", "SubRed"), 
             Type:= "PaidFees"]
    
    setDT(db, key= c("Date", "Ccy"))
    
    db <- db[, sum(Amount), by= .(Date, Port, Ccy, Type)]
    colnames(db)[5] <- "Amount"
    
    # add whitholdding tax datas
    tax <- calcPaidTax(db)
    
    db <- rbind(tax,db)
    
}

# 
# 
# fileSelect <- fileList[grepl("Copy", fileList)]
# 
# db1 <- fread(fileSelect, skip = 1, fill=TRUE)
# db1 <- db1[,c(2,6:11,13)]
# 
# colnames(db1) <- c("Port", "Ccy", "Type", "Ref", "Name", 
#                   "Value", "Date", "Amount")
# 
# db1[, ':=' (Port=  "DF EQUITY",
#            Value= as.Date(Value, "%d.%m.%Y"),
#            Date=  as.Date(Date,  "%d.%m.%Y")#,
#            #Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))
#            ), ]
# 
# db1 <- db1[!Type %in% c("OEM", "OER", "OCS", "ORG", "OSD", "ODC", "OPF", "OEA",
#                       "BOC","CUS", "EMC", "MAF", "DCC", "DTC",
#                       "MTM", "ICC", "TRI"),]
# 
# db1[Type == "IPC", Type:="PaidInterest"]
# db1[Type == "OCD", Type:="NetDividend"]
# db1[grep("Redemption", Name), Type:= "SubRed"]
# db1[grep("from|FROM|SCR", Name), Type:= "SubRed"]
# db1 <- db1[!Type %in% c("PaidInterest", "NetDividend", "SubRed"), 
#          Type:= "PaidFees"]
# 
# setDT(db1, key= c("Date", "Ccy"))
# 
# db1 <- db1[, sum(Amount), by= .(Date, Port, Ccy, Type)]
# colnames(db1)[5] <- "Amount"
