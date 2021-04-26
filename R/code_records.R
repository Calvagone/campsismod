
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

#' Detect all compartments from model code records.
#' Only for model instantiation. Not exported.
#'
#' @param records code records
#' @return a list of 2 args: first argument is the DES record without characteristics,
#' second argument is the compartments object
#' 
getCompartments <- function(records) {
  assertthat::assert_that(is(records, "code_records"), msg="records class is not 'code_records'")
  desRecord <- records %>% getByName("DES")
  compartments <- Compartments()
  if (length(desRecord) == 0) {
    return(list(desRecord, compartments))
  }
  code <- desRecord@code
  odeCounter <- 0
  updatedRecord <- new("des_record")
  
  for (index in seq_along(code)) {
    line <- code[index]
    if (isODE(line)) {
      odeCounter <- odeCounter + 1
      name <- extractTextBetweenBrackets(line)
      if (startsWith(name, prefix="A_")) {
        name <- gsub("^A_", "", name)
        if (name == as.character(odeCounter)) {
          name <- NA
        }
      } else {
        stop(paste0("Compartment ", name, " does not start with 'A_'"))
      }
      compartment <- Compartment(index=odeCounter, name=name)
      compartments <- compartments %>% add(compartment)
      updatedRecord@code <- updatedRecord@code %>% append(line)
    
    } else if (isBioavailibility(line)) {
      compartments <- addCharacteristic(line, Bioavailability(0, rhs=""), compartments)
      
    } else if (isLagTime(line)) {
      compartments <- addCharacteristic(line, LagTime(0, rhs=""), compartments)
    
    } else if (isInfusionDuration(line)) {
      compartments <- addCharacteristic(line, InfusionDuration(0, rhs="", rate=FALSE), compartments)
    
    } else if (isRate(line)) {
      compartments <- addCharacteristic(line, InfusionDuration(0, rhs="", rate=TRUE), compartments)
    
    } else if (isInitialCondition(line)) {
      compartments <- addInitialCondition(line, InitialCondition(0, rhs=""), compartments)
        
    } else {
      updatedRecord@code <- c(updatedRecord@code, line)
    }
  }
  return(list(updatedRecord, compartments))
}

#' Add characteristic to compartments object.
#'
#' @param line line record
#' @param emptyCharacteristic empty characteristic, to be completed
#' @param compartments compartments object
#' @return updated compartments object
#' 
addCharacteristic <- function(line, emptyCharacteristic, compartments) {
  cmtName <- extractTextBetweenBrackets(line)
  compartment <- compartments %>% getByName(cmtName)
  
  if (length(compartment) == 0) {
    stop(paste0("Characteristic compartment undefined: '", cmtName, "'"))
  }
  characteristic <- emptyCharacteristic
  characteristic@compartment <- compartment@index
  characteristic@rhs <- extractRhs(line)
  
  return(compartments %>% add(characteristic))
}

#' Add initial condition to compartments object.
#'
#' @param line line record
#' @param emptyCondition empty condition, to be completed
#' @param compartments compartments object
#' @return updated compartments object
#' 
addInitialCondition <- function(line, emptyCondition, compartments) {
  cmtName <- getInitialConditionCmt(line)
  compartment <- compartments %>% getByName(cmtName)
  
  if (length(compartment) == 0) {
    stop(paste0("Initial condition compartment undefined: '", cmtName, "'"))
  }
  condition <- emptyCondition
  condition@compartment <- compartment@index
  condition@rhs <- extractRhs(line)
  
  return(compartments %>% add(condition))
}

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

setMethod("getEquation", signature=c("code_records", "character"), definition=function(object, lhs) {
  for (record in object@list) {
    equation <- record %>% getEquation(lhs)
    if (!is.null(equation)) {
      return(equation)
    }
  }
  return(NULL)
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

isRecordDelimiter <- function(line) {
  return(grepl("^s*\\[.*\\]s*$", line))
}

getRecordDelimiter <- function(line) {
  return(gsub("\\[(.*)\\]","\\1", line))
}

#_______________________________________________________________________________
#----                           removeEquation                              ----
#_______________________________________________________________________________

setMethod("removeEquation", signature=c("code_records", "character"), definition=function(object, lhs) {
  copy <- object
  for (record in object@list) {
    copy <- copy %>% replace(record %>% removeEquation(lhs))
  }
  return(copy)
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

setMethod("replaceEquation", signature=c("code_records", "character", "character"), definition=function(object, lhs, rhs) {
  copy <- object
  for (record in object@list) {
    copy <- copy %>% replace(record %>% replaceEquation(lhs, rhs))
  }
  return(copy)
})

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
    warning("model not provided, compartment characteristics and initial conditions will be lost")
  } else {
    characteristics <- model@compartments@characteristics
    initial_conditions <- model@compartments@initial_conditions
  }
  
  # Adding characteristics to DES record
  desRecord <- object %>% getByName("DES")
  if (!is.null(desRecord) && !is.null(model)) {
    for (characteristic in characteristics@list) {
      desRecord@code <- desRecord@code %>% append(characteristic %>% toString(model=model))
    }
    for (initial_condition in initial_conditions@list) {
      desRecord@code <- desRecord@code %>% append(initial_condition %>% toString(model=model))
    }
    object <- object %>% replace(desRecord)
  }

  # Write code record
  code <- NULL
  for (record in object@list) {
    code <- code %>% append(paste0("[", record %>% getName(), "]"))
    code <- code %>% append(record@code)
    code <- code %>% append("") # write.table will add a new line
  }
  write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})


