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

cashPos  <- getCashPos(fileList)

secPos   <- getSecurityPos(fileList)


# get NAV datas
setwd("/home/Alexandre/DFE_Nav_csv")
fileList <- list.files()

nav <- getNavData(fileList)

accruedFees <- getAccruedFees(nav)

pendingDiv <- getPendingDiv(nav)

# get details of all cash movements
allCash <- getAllCash(cashPos, cashMvmt, accruedFees, pendingDiv)

# format file for Bloomberg BBU
uploadBBU <- formatBBU(secPos, allCash)


# add positions before auto upload 
oldPos <- fread("/home/artha/R-Projects/DFE/Config/positionsBeforeAutoUpload.csv")[,-c(1, 7)]
oldPos[,  Date:= as.Date(Date, format="%Y-%m-%d")]

uploadBBU  <- rbind(uploadBBU, oldPos)

# save file
fwrite(uploadBBU, file="/home/artha/R-Projects/DFE/upload/positionsDFE.csv")

