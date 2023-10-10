
#get pos from trades
setwd("/home/Alexandre/DFEquity_Trades")
library(openxlsx)

posFromTrade <- read.xlsx("DFE_Trades.xlsx","Sheet1",
                          #stringsAsFactors=FALSE,
                          skipEmptyRows = TRUE)

setDT(posFromTrade)

posFromTrade <- posFromTrade[, .(Portfolio.Name, Execution.Date, Type, Security.ID, CCY, Quantity)]
colnames(posFromTrade) <- c("Port", "Date", "Type", "Ticker", "Ccy", "Quantity")

posFromTrade[, Date:=as.Date(as.character(Date), format="%Y%m%d")]
posFromTrade[Type == "Sell", Quantity:= -Quantity]

setkey(posFromTrade, Date, Ticker)

posFromTrade[, Amount:= Quantity,]
posFromTrade[, Amount:= cumsum(Amount*1000)/1000, by= Ticker]

# posFromTrade[, Amount:= cumsum(Quantity), by= .(Ticker)]

posFromTrade <- posFromTrade[, .(Port, Date, Ticker, Ccy, Amount), ]

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

setkey(uploadBBU, Date)


## get oneRiver price

OneRiverPrices <- read.xlsx("/home/Alexandre/02_ressources/Publications/FundsPerformances/fundsPerformance_2023_landscape.xlsx",
                            sheet = "Estimated",
                            cols= c(14,16),
                            #stringsAsFactors=FALSE,
                            skipEmptyRows = TRUE)
#OneRiverPrices$Ccy <- "USD"
setDT(OneRiverPrices)

OneRiverPrices[, Date := as.Date(Date, format="%Y%m%d")]
setkey(OneRiverPrices, Date)

db <- OneRiverPrices[uploadBBU[Ticker == ".ONE  KY Equity"]]
db$i.Price <- NULL
db2 <- na.locf(db)

x <- uploadBBU[Ticker != ".ONE  KY Equity"]
uploadBBU <- rbind(x,db)
x <- uploadBBU[Ticker == ".ONE  KY Equity"]
x <- na.locf(x)
y <- uploadBBU[Ticker != ".ONE  KY Equity"]
uploadBBU <- rbind(x,y)
#uploadBBU$Port <- "DF TEST"

# save file

fwrite(uploadBBU, file="/home/Alexandre/r-projects/DFE/upload/positionsDFE.csv")

# archive histo NAV

source("/home/Alexandre/r-projects/DFE/R/Source/archiveNAV.R")

privateSecurities <- uploadBBU[substr(uploadBBU$Ticker,1,1) == "."]

first <- nchar(privateSecurities$Ticker)-8
privateSecurities$Country <- substr(privateSecurities$Ticker,first,first+2)
privateSecurities$Privilege <- "Firm"
privateSecurities$SecType <- "Common Stock"

fwrite(privateSecurities, file="/home/Alexandre/r-projects/DFE/upload/privateSecurities.csv")


