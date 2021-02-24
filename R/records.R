
setClass(
  "records",
  representation(
    list = "list"
  )
)

#_______________________________________________________________________________
#----                              getRecord                                ----
#_______________________________________________________________________________

#' Get record.
#' 
#' @param object generic object
#' @param recordType parameter type: theta
#' @return record object
#' @export
getRecord <- function(object, recordType) {
  stop("No default function is provided")
}

setGeneric("getRecord", function(object, recordType) {
  standardGeneric("getRecord")
})

setMethod("getRecord", signature=c("records", "character"), definition=function(object, recordType) {
  record <- object@list %>% purrr::keep(~(.x %>% getName()==recordType))
  if (length(record) >= 1) {
    record <- record[[1]]
  }
  return(record)
})

#_______________________________________________________________________________
#----                              addRecord                                ----
#_______________________________________________________________________________

#' Add record.
#' 
#' @param object generic object
#' @param record record to be added
#' @export
addRecord <- function(object, record) {
  stop("No default function is provided")
}

setGeneric("addRecord", function(object, record) {
  standardGeneric("addRecord")
})

setMethod("addRecord", signature=c("records", "record"), definition=function(object, record) {
  object@list <- c(object@list, record)
  return(object)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("records", "character"), definition=function(object, file) {
  code <- NULL
  for (record in object@list) {
    code <- c(code, paste0("[", record %>% getName(), "]"))
    code <- c(code, record@code)
    code <- c(code, "") # write.table will add a new line
  }
  write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})

