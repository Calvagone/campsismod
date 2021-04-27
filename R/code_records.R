
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

#' Detect all compartments names from the code records.
#' Only for model instantiation. Not exported.
#'
#' @param records code records
#' @return a list of compartments
#' 
getCompartments <- function(records) {
  assertthat::assert_that(is(records, "code_records"), msg="records class is not 'code_records'")
  odeRecord <- records %>% getByName("ODE")
  compartments <- Compartments()
  if (length(odeRecord) == 0) {
    return(list(odeRecord, compartments))
  }
  code <- odeRecord@code
  odeCounter <- 0
  updatedOdeRecord <- OdeRecord()
  
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
      updatedOdeRecord@code <- updatedOdeRecord@code %>% append(line)

    } else {
      updatedOdeRecord@code <- c(updatedOdeRecord@code, line)
    }
  }
  return(compartments)
}

#' Add properties to compartments object.
#'
#' @param compartments compartments object
#' @param records all records
#' @param name record name to look at
#' @param init empty characteristic, to be completed
#' @return updated compartments object
#' 
addProperties <- function(compartments, records, name, init) {
  record <- records %>% getByName(name)
  if (record %>% length() == 0) {
    return(compartments)
  }
  for (line in record@code) {
    cmtName <- extractLhs(line) %>% trim()
    if (cmtName %>% length() == 0) {
      next
    }
    compartment <- compartments %>% getByName(cmtName)
    
    if (length(compartment) == 0) {
      stop(paste0("Compartment undefined: '", cmtName, "' in record ", record %>% getName()))
    }
    characteristic <- init
    characteristic@compartment <- compartment@index
    characteristic@rhs <- extractRhs(line)
    
    compartments <- compartments %>% add(characteristic)
  }
  return(compartments)
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
#----                       removeTransientRecords                          ----
#_______________________________________________________________________________

removeTransientRecords <- function(object) {
  records <- CodeRecords()
  for (record in object@list) {
    if (!record@transient) {
      records <- records %>% add(record)
    }
  }
  return(records)
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
  levels <- c("MAIN", "ODE", "F", "LAG", "DURATION", "RATE", "INIT", "ERROR")
  names <- factor(names, levels=levels, labels=levels)
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
  
  # Adding characteristics to ODE record
  odeRecord <- object %>% getByName("ODE")
  if (!is.null(odeRecord) && !is.null(model)) {
    for (characteristic in characteristics@list) {
      odeRecord@code <- odeRecord@code %>% append(characteristic %>% toString(model=model))
    }
    for (initial_condition in initial_conditions@list) {
      odeRecord@code <- odeRecord@code %>% append(initial_condition %>% toString(model=model))
    }
    object <- object %>% replace(odeRecord)
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


