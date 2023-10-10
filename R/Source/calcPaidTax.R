
# calc Whitholdding taxe based on net Dividend
calcPaidTax <- function(db= cashMvmt) {
    
    tickers <- read.csv("/home/Alexandre/r-projects/DFE/Config/tickers.csv",header = FALSE)
    colnames(tickers) <- c("Isin", "Ticker","Name")
    
    db <- db[grepl("Dividend", Type), ]
    
   # db$Isin <- ""
    
    # if ("Name" %in% colnames(db)) {   
    
        for (i in 1:length(tickers$Name)) {
        
            db[grepl(substr(tickers$Name[i],1,4),toupper(Name)), Isin:=tickers$Isin[i]]
        }
    
    #}
    
    
    db <- db[Ccy == "GBP", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0)]
    db <- db[Ccy == "USD", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0.3 / 0.7)]
    db <- db[Ccy == "CHF", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0.35 / 0.65)]
    db <- db[Ccy == "DKK", ':=' (Type=  "Wh_Tax", 
                                 Amount=  -Amount * 0.27 / 0.73)]
    db <- db[Ccy == "SEK", ':=' (Type=  "Wh_Tax", 
                                 Amount=  -Amount * 0.3 / 0.7)]
    db <- db[Ccy == "NOK", ':=' (Type=  "Wh_Tax", 
                                 Amount=  -Amount * 0.25 / 0.75)]
    
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "DE", ':=' (Type=  "Wh_Tax", 
                                 Amount= - Amount * 0.26375 / 0.73625)]
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "FI", ':=' (Type=  "Wh_Tax",
                                Amount= - Amount * 0.20 / 0.80)]
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "FR", ':=' (Type=  "Wh_Tax",
                                Amount= - Amount * 0.265 / 0.735)]
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "IE", ':=' (Type=  "Wh_Tax",
                                Amount= - Amount * 0.25 / 0.75)]
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "IT", ':=' (Type=  "Wh_Tax",
                                Amount= - Amount * 0.26 / 0.75)]
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "NL", ':=' (Type=  "Wh_Tax",
                                Amount= - Amount * 0.15 / 0.85)]
    db <- db[Ccy == "EUR" & substr(Isin,1,2) == "SE", ':=' (Type=  "Wh_Tax",
                                Amount= - Amount * 0.30 / 0.70)]
}

