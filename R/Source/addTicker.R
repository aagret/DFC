
# add Bloomberg Tickers (need separate data file for Isin/ticker relation)
addTicker <- function(db= secPos) {
    
    tickers <- read.csv("/home/Alexandre/r-projects/DFE/Config/tickers.csv", header= FALSE, stringsAsFactors = FALSE)
    colnames(tickers) <- c("Isin", "Ticker","Name")
    
    setDT(tickers, key= c("Isin", "Ticker"))
    setkey(db, Isin)
    
    #check if new Isin !TODO!
    new <- unique(db$Isin[!db$Isin %in% tickers$Isin])
    
    if(length(new) != 0)  {
        new <-list(new)
        
        system("echo 'Subject: DFE alert\rnew security in the fund' | sendmail aagret@arthafinance.biz")
        
    }
    
    db <- tickers[db]
    
}
