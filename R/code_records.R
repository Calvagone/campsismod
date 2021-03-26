
setClass(
  "code_records",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="code_record")
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
#----                          getCompartments                              ----
#_______________________________________________________________________________

#' Get all compartments from model.
#' 
#' @param object generic object
#' @return a list of compartments
#' @export
getCompartments <- function(object) {
  stop("No default function is provided")
}

setGeneric("getCompartments", function(object) {
  standardGeneric("getCompartments")
})

setMethod("getCompartments", signature=c("code_records"), definition=function(object) {
  desRecord <- object %>% getByName("DES")
  retValue <- Compartments()
  
  if (length(desRecord) == 0) {
    return(retValue)
  }
  code <- desRecord@code
  for (index in seq_along(code)) {
    line <- code[index]
    if (isODE(line)) {
      name <- getODEName(line)
      if (startsWith(name, prefix="A_")) {
        name <- gsub("^A_", "", name)
        if (name == as.character(index)) {
          name <- NA
        }
      }
      compartment <- Compartment(index=index, name=name)
      retValue <- retValue %>% add(compartment)
    }
  }
  return(retValue)
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

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________


setMethod("show", signature=c("code_records"), definition=function(object) {
  for (record in object@list) {
    show(record)
    print("")
  }
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

setMethod("sort", signature=c("code_records"), definition=function(x, decreasing=FALSE, ...) {
  names <- x@list %>% purrr::map_chr(~.x %>% getName())

  # Reorder
  names <- factor(names, levels=c("PK", "PRED", "DES", "ERROR"), labels=c("PK", "PRED", "DES", "ERROR"))
  order <- order(names)
  
  # Apply result to original list
  x@list <- x@list[order]
  return(x)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("code_records", "character"), definition=function(object, file) {
  # First sort code records
  object <- object %>% sort()
  
  # Write code record
  code <- NULL
  for (record in object@list) {
    code <- c(code, paste0("[", record %>% getName(), "]"))
    code <- c(code, record@code)
    code <- c(code, "") # write.table will add a new line
  }
  write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})


