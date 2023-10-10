
# extract data from file
getData <- function(file= fileSelect, drop= 1) {
    
    db <- ldply(file, formatCsv, drop)
    setDT(db)
    
}
