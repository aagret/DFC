
# format file according to KBL file structure
formatCsv <- function(file= file, drop= drop) {
    
    db <- gsub("\t", "", readLines(file))
    db <- gsub('^\\"|\\"$',"", db)
    
    db <- strsplit(db[-drop], ";")
    db <- do.call("rbind.data.frame", c(db, stringsAsFactors=FALSE))
    colnames(db) <- gsub(" ", "", as.character(unlist(db[1, ])))
    db  <- db[-1, ]
    
}
