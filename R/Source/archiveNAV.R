
library(data.table)

search <- "ffnav1"
dir <- "/home/Alexandre/DFE_Nav_csv/"

files <- list.files(dir, search)

db <- do.call(rbind, lapply(files, function(x) read.csv(paste0(dir, x), stringsAsFactors = FALSE)))
db <- db[,c(3,6,7,5)]
setDT(db)

colnames(db) <- c("Date", "Shs", "NAV", "TotAsset")
db[, Date:= as.Date(Date, "%d/%m/%Y")]

fwrite(db, "/home/Alexandre/r-projects/DFE/upload/histoNAV.csv")
