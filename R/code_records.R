
setClass(
  "code_records",
  representation(
  ),
  contains = "pmx_list"
)

#' 
#' Create a list of code records.
#' 
#' @return an empty list of code records  
#' @export
CodeRecords <- function() {
  return(new("code_records"))
}

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("code_records", "character"), definition=function(object, file) {
  code <- NULL
  for (record in object@list) {
    code <- c(code, paste0("[", record %>% getName(), "]"))
    code <- c(code, record@code)
    code <- c(code, "") # write.table will add a new line
  }
  write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})

#_______________________________________________________________________________
#----                                read.model                             ----
#_______________________________________________________________________________

#' Read model file.
#' 
#' @param file path to records
#' @return records object
#' @export
read.model <- function(file) {
  allLines <- read.table(file=file, sep="@")[,1]
  records <- new("code_records", list=list())
  prevRecordIndex <- 1
  
  for (index in seq_along(allLines)) {
    line <- allLines[index]
    if (isRecordDelimiter(line)) {
      recordDelimiter <- getRecordDelimiter(line)
      record <- new(paste0(tolower(recordDelimiter), "_record"), code=character())
      records@list <- c(records@list, record)
      if (length(records@list) > 1) {
        records@list[[length(records@list)-1]]@code <- allLines[(prevRecordIndex + 1):(index-1)]
      }
      prevRecordIndex <- index
    }
  }
  # Filling last record
  records@list[[length(records@list)]]@code <- allLines[(prevRecordIndex + 1):length(allLines)]
  return(records)
}

isRecordDelimiter <- function(line) {
  return(grepl("^s*\\[.*\\]s*$", line))
}

getRecordDelimiter <- function(line) {
  return(gsub("\\[(.*)\\]","\\1", line))
}


