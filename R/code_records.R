
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
#' Only for model instantiation. Not exported.
#' 
#' @param model model
#' @return compartments object
getCompartments <- function(model) {
  assertthat::assert_that(is(model, "pmx_model"), msg="model is not a PMX model")
  desRecord <- model@model %>% getByName("DES")
  retValue <- Compartments()
  if (length(desRecord) == 0) {
    return(retValue)
  }
  code <- desRecord@code
  odeCounter <- 0
  
  for (index in seq_along(code)) {
    line <- code[index]
    if (isODE(line)) {
      odeCounter <- odeCounter + 1
      name <- getODEName(line)
      if (startsWith(name, prefix="A_")) {
        name <- gsub("^A_", "", name)
        if (name == as.character(odeCounter)) {
          name <- NA
        }
      }
      compartment <- Compartment(index=odeCounter, name=name)
      retValue <- retValue %>% add(compartment)
    }
  }
  return(retValue)
}

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
  records <- CodeRecords()
  
  # Reading all records
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
  
  # Reading DES record
  desRecord <- records %>% getByName("DES")
  
  
  return(records)
}

#' Extract all compartment characteristics from the DES record.
#' 
#' @param record DES record
#' @return a list
extractCharacteristicsFromDesRecord <- function(record) {
  
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
    cat("\n")
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

setMethod("write", signature=c("code_records", "character"), definition=function(object, file, ...) {
  # First sort code records
  object <- object %>% sort()
  
  # The model is needed to get the characteristics
  model <- processExtraArg(args=list(...), name="model")
  if (is.null(model)) {
    warning("model not provided, compartment characteristics will not be persisted")
  } else {
    characteristics <- model@compartments@characteristics
  }
  
  # Adding characteristics to DES record
  desRecord <- object %>% getByName("DES")
  if (!is.null(desRecord)) {
    for (characteristic in characteristics@list) {
      desRecord@code <- c(desRecord@code, characteristic %>% toString())
    }
    object <- object %>% replace(desRecord)
  }

  # Write code record
  code <- NULL
  for (record in object@list) {
    code <- c(code, paste0("[", record %>% getName(), "]"))
    code <- c(code, record@code)
    code <- c(code, "") # write.table will add a new line
  }
  write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})


