# extract datas from weekly NAV files and daily KBL files
# format and export to Bloomberg for PORT analysis

library(data.table)
library(plyr)
library(zoo)


#load functions
source("~/R-Projects/DFE/R/scriptFunctions.R")


# get kbl downloaded files
setwd("/home/artha/kbl")
fileList <- list.files()

cashMvmt <- getCashMvmt(fileList)
cashMvmt <- unique(cashMvmt)

cashPos  <- getCashPos(fileList)

# get NAV datas
setwd("/home/Alexandre/DFE_Nav_csv")
fileList <- list.files()

nav <- getNavData(fileList)

margin <- nav[grep("AC", Code), .(Port, Date, Type, Ccy, Amount) ]
margin$Type <- "Margin"

accruedFees <- getAccruedFees(nav)

pendingDiv <- getPendingDiv(nav)
pendingDiv <- unique(pendingDiv)

# get details of all cash movements
allCash <- getAllCash(cashPos, cashMvmt, accruedFees, pendingDiv, margin)


###############################################

#get pos from trades
setwd("/home/Alexandre/DFEquity_Trades")
library(openxlsx)

posFromTrade <- read.xlsx("DFE_Trades.xlsx","Sheet1",
                          #stringsAsFactors=FALSE,
                          skipEmptyRows = TRUE)

setDT(posFromTrade)

posFromTrade <- posFromTrade[, .(Portfolio.Name, Execution.Date, Type, Security.ID, Quantity)]
colnames(posFromTrade) <- c("Port", "Date", "Type", "Ticker", "Quantity")

posFromTrade[, Date:=as.Date(as.character(Date), "%Y%m%d")]
posFromTrade[Type == "Sell", Quantity:= -Quantity]

posFromTrade[, Amount:= cumsum(Quantity), by= .(Ticker)]

posFromTrade <- posFromTrade[, .(Port, Date, Ticker, Amount), ]

setkey(posFromTrade, Date, Ticker)

#pos <- posFromTrade[Date == min(Date), ]

dts <- sort(unique(c(allCash$Date, posFromTrade$Date)))
pos <- posFromTrade[Date == dts[1], ]

for(dt in dts[-1]) {
#for(dt in unique(posFromTrade$Date)[-1]) {
  
    new <- posFromTrade[Date == as.Date(dt), ]
    
    old <- pos[Date == max(pos[Date < as.Date(dt), Date]) & !Ticker %in% new$Ticker, ]
  
    old$Date <- as.Date(dt)
    
    new <- rbind(new , old)
    
    

 #   new <- posFromTrade[Date == as.Date(dt), ]

    #pos[Ticker %in% new$Ticker, Amount:=0, by=.(Date, Ticker)]
        
    pos <- rbind(pos,new)
    
    #pos[unique(pos[,.(Date, Ticker)]), mult= "last"]
}

pos <- pos[Amount != 0,]



############################################################

# format file for Bloomberg BBU
uploadBBU <- formatBBU(pos, allCash)


#uploadBBU$Port <- "DF TEST"

# save file

fwrite(uploadBBU, file="/home/artha/R-Projects/DFE/upload/positionsDFE.csv")


