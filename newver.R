# extract datas from weekly NAV files and daily KBL files
# format and export to Bloomberg for PORT analysis

library(data.table)
library(plyr)
library(zoo)


#load functions
source("/home/Alexandre/r-projects/DFE/R/scriptFunctions.R")


calcPaidTax <- function(tax= nav) {
  
  # load securities database
  tickers <- read.csv("/home/Alexandre/r-projects/DFE/Config/tickers.csv",
                      header = FALSE)
  
  colnames(tickers) <- c("Isin", "Ticker","Name")
  
  # select only dividends data
#  tax <- tax[grepl("Dividend", Type), ]
  
  # check if data names match tickers names and add Isin codes
  for (i in 1:length(tickers$Name)) {
    
    tax[grepl(substr(tickers$Name[i],1,6),toupper(Name)), Isin:=tickers$Isin[i]]
    
  }
  
  # usual tax rates per country/currency
  tax <- tax[Ccy == "GBP", ':=' (Type=  "Wh_Tax", 
                               Amount= - Amount * 0)]
  tax <- tax[Ccy == "USD", ':=' (Type=  "Wh_Tax", 
                               Amount= - Amount * 0.3 / 0.7)]
  tax <- tax[Ccy == "CHF", ':=' (Type=  "Wh_Tax", 
                               Amount= - Amount * 0.35 / 0.65)]
  tax <- tax[Ccy == "DKK", ':=' (Type=  "Wh_Tax", 
                               Amount=  -Amount * 0.27 / 0.73)]
  tax <- tax[Ccy == "SEK", ':=' (Type=  "Wh_Tax", 
                               Amount=  -Amount * 0.3 / 0.7)]
  tax <- tax[Ccy == "NOK", ':=' (Type=  "Wh_Tax", 
                               Amount=  -Amount * 0.25 / 0.75)]
  
  # usual tax rates in Europe per country
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "DE", ':=' (Type=  "Wh_Tax", 
                                                          Amount= - Amount * 0.26375 / 0.73625)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "FI", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.20 / 0.80)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "FR", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.265 / 0.735)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "IE", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.25 / 0.75)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "IT", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.26 / 0.75)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "NL", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.15 / 0.85)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "SE", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.30 / 0.70)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "AT", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.275 / 0.725)]
  tax <- tax[Ccy == "EUR" & substr(Isin,1,2) == "ES", ':=' (Type=  "Wh_Tax",
                                                          Amount= - Amount * 0.195 / 0.805)]
  
  return(tax[,-"Isin"])
  
}

getCashMvmt <- function(fileList= fileList) {
  
  ## READ new historical file from 2018-11-02 to 2020-07-15
  fileSelect <- fileList[grepl("HistoCashTransactions", fileList)]
  
  db <- fread(fileSelect, skip = 1, fill=TRUE)[,c(2,6:11,13)]
  
  colnames(db) <- c("Port", "Ccy", "Type", "Ref", "Name", 
                    "Value", "Date", "Amount")
  
  db[, ':=' (Port=  "DF Equity",
             Value= as.Date(Value, "%d.%m.%Y"),
             Date=  as.Date(Date,  "%d.%m.%Y")), ]

  
  ##read daily KBL files
  fileSelect <- fileList[grepl("CshStmt_JRN", fileList)]

  kbl <- setDT(ldply(fileSelect, formatCsv, 1))
  kbl <- kbl[grep("EQUITY FUND", ClientName), 
             c("ClientName", 
               "TransactionCurrency", "TransactionType",
               "TransactionReference", "TransactionName",
               "ValueDate", "AccountingDate", "MovementAmount",
               "CorporateActionParticipation", "CustomerNetAmount")]
  
  colnames(kbl) <- c("Port", "Ccy", "Type", "Ref", "Name", 
                     "Value", "Date", "Amount", "Dividend", "Tax")
  
  kbl[, ':=' (Port=  "DF Equity",
              Value= as.Date(Value, "%Y-%m-%d"),
              Date=  as.Date(Date,  "%Y-%m-%d"),
              Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount))),
              Dividend=as.numeric(gsub(",", ".", gsub("\\.", "", Dividend)))
              )]
                                 
  # remove old kbl datas inclueded in db histo
  kbl <- kbl[Date > as.Date("2020-07-15"), ]
  
  # merge recent and histo datas
  db <- rbindlist(list(db,kbl), fill=TRUE)
  
  #remove unused accounting data
  db <- db[!Type %in% c("BOC", "MTM", "ODC", "ICC", "CUS", "OPF", "OEA", "TRI"), ]
 
  # reclass Types
  db[Type %in% c("IPC", "IDC"),
     Type:="1.2 PaidInterest"]
  
  db[Type %in% c("OCD", "OCS", "OER"),
     Type:="3.1 NetDividend"]
  
  db[grep("Pch|Sle|pch|sle", Name),
     Type:= "2.2 Trade"]
  
  db[Type %in% c("EMC", "BPC", "OCR", "EPC", "MAF", "DTC", "DSC", "DCC"),
     Type:= "2.2 Trade"]

  db[grep("Redemption|Subscription|Transfer|BONIF|Sub|TFR|SCR|TRF|Trf", Name),
     Type:= "2.1 SubRed"]

  db[Type %in% c("VIR", "DDG", "OEM", "OSD", "ORG", "OAS", "OSP", "OES", "FEE"),
     Type:= "1.3 PaidFees"]
  
  db[grep("4", Type),
     Type:= "1.3 PaidFees"] 
  
  db[grep("Commission", Name),
     Type:= "1.3 PaidFees"] 
  
  db <- db[Type != "FUC", ]
  
  setDT(db, key= c("Date", "Ccy"))
  
  #sum Amount by Type
  db <- db[, sum(Amount), by= .(Date, Port, Ccy, Type, Name)]
  
  #colnames(db)[6] <- "Amount"
  colnames(db)[6] <- "Amount"
  
  return(db)
  
}

# retrieve cash Balances from KBL files
getCashPos <- function(fileList= fileList) {
  
  fileSelect <- fileList[grepl("SolCptCsh", fileList)]
  
  #db <- getData(fileSelect, c(1, 3))[grep("K0124100", ClientID)]
  db <- setDT(ldply(fileSelect, formatCsv, c(1,3)))
  db <- db[grep("K0124100", ClientID)]
  
  db <- db[AccountType %in% c("CC", "IV", "FWD", "75"), .(ClientID, ValueDate, AccountType,
                                                    Accountcurrency, BalanceatValueDate )] #BalanceatValueDate get('FuturePositionD+2')
  
  colnames(db) <- c("Port", "Date", "Type", "Ccy", "Amount")
  
  db[, ':=' (Port=  "DF Equity",
             Date= as.Date(Date, "%Y-%m-%d"),
             Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
  
  db[Type == "IV", Type:= "1.1 AccruedInterest"]
  db[Type == "75", Type:= "5.1 PendingNetDividend"]
  db[Type == "FWD",Type:= "2.3 FxFwd"]
  db[Type == "CC", Type:= "1.0 CurrentAccount"]
  
  db <- db[Date > as.Date("2020-07-15"), ]
  
  
  ## READ new historical file from 2018-11-02 to 2020-07-15
  fileSelect <- fileList[grepl("HistoCashPositions", fileList)]
  
  
  histo <- fread(fileSelect,select = c(1:4))
  
  colnames(histo) <- c("Port", "Date", "Ccy", "Amount")
  
  histo[, ':=' (Date= as.Date(Date, "%d.%m.%Y"),
                Type= "1.0 CurrentAccount"), ]
  #,
  #             Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
  
  db <- rbind(histo,db)
  
  db <- db[, sum(Amount), by= .(Date, Type, Ccy)]
  
  colnames(db)[4] <- "Amount"
  db$Port <- "DF Equity"
  db$Name <- "Cash"

  return(db)
  
}

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
  
  #remove unused data
  db <- db[!Cat %in% c("VMOB", "CAT", "FUTU", "CPON"), ]
  db <- db[!grep("AC|BA|DC|CD|EF|CM", Code), ]
  
  #db[grep("BA|DC|CD|EF|CM", Code), Type:= "Cash"]
  db[Cat == "TRES" & Type == "AD1", Type:= "5.2 PendingFees"]
  
  db[, ':=' (Port=  "DF Equity",
             Date= as.Date(Date, format= "%d/%m/%Y"))]
  
  setkey(db, Date, Ccy)
  
  return(db[, .(Date, Type, Ccy, Amount, Port, Name)])
  
}


# get kbl downloaded files
setwd("/home/Alexandre/kbl-downloads/")
fileList <- list.files()

cashMvmt <- getCashMvmt(fileList)

cashPos  <- getCashPos(fileList)

# get NAV datas
setwd("/home/Alexandre/DFE_Nav_csv")
fileList <- list.files()

nav <- getNavData(fileList)

paidWhTax <- calcPaidTax(cashMvmt[Type== "3.1 NetDividend", ] )
paidWhTax[, Type:= "3.2 PaidWhTax"]

# merge and compleete data
data <- rbind(cashMvmt, cashPos, nav, paidWhTax)

data <- dcast(data, Date+Port+Ccy ~Type, value.var = "Amount", fun.aggregate = sum, fill = NA)

locf <- c("1.0 CurrentAccount", "1.1 AccruedInterest", "5.2 PendingFees", "2.3 FxFwd")
data[, (locf):= lapply(.SD, na.locf, na.rm=FALSE),
      by= Ccy, 
      .SDcols= locf]

data[is.na(data)] <- 0

data[, `1.3 PaidFees`:= cumsum(`1.3 PaidFees`), by= Ccy]
data[, `3.2 PaidWhTax`:= cumsum(`3.2 PaidWhTax`), by= Ccy]

data[, CashInNav:= `1.0 CurrentAccount` + `1.1 AccruedInterest` + 
        `5.2 PendingFees` + `5.1 PendingNetDividend`, by= Ccy]

data[, TotalFees:= `5.2 PendingFees` + `1.3 PaidFees`, by= Ccy]
data[, TotalTaxes:= `3.2 PaidWhTax`, by= Ccy]
data[, OffsetCash:= CashInNav - TotalFees - TotalTaxes, by= Ccy]

