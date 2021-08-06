
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
#----                                add                                    ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature=c("code_records", "code_records"), definition=function(object, x) {
  return(object %>% appendCodeRecords(x))
})

#' Append code records
#' 
#' @param records1 base set of code records
#' @param records2 extra set of code records to be appended
#' @return the resulting set of code records
#' 
appendCodeRecords <- function(records1, records2) {
  for (record in (records2)@list) {
    baseRecord <- records1 %>% getByName(record %>% getName())
    if (baseRecord %>% length() == 0) {
      records1 <- records1 %>% add(record)
    } else {
      baseRecord <- baseRecord %>% add(record)
      records1 <- records1 %>% replace(baseRecord)
    }
  }
  return(records1 %>% sort())
}

#_______________________________________________________________________________
#----                            addEquation                                ----
#_______________________________________________________________________________

#' @rdname addEquation
setMethod("addEquation", signature=c("code_records", "character", "character"), definition=function(object, lhs, rhs, before=NULL, after=NULL) {
  
  if (is.null(before) && is.null(after)) {
    # Default use MAIN record and append
    mainRecord <- object %>% getByName("MAIN")
    mainRecord <- mainRecord %>% addEquation(lhs=lhs, rhs=rhs, before=before, after=after)
    object <- object %>% replace(mainRecord)
  } else {
    for(record in object@list) {
      beforeOrAfter <- ifelse(is.null(before), after, before)
      if (record %>% hasEquation(beforeOrAfter)) {
        record <- record %>% addEquation(lhs=lhs, rhs=rhs, before=before, after=after)
        object <- object %>% replace(record)
        break
      }
    }
  }
  
  return(object)
})

#_______________________________________________________________________________
#----                         addPropertiesRecords                           ----
#_______________________________________________________________________________

addPropertiesRecords <- function(records, model) {
  properties <- model@compartments@properties
  
  for (name in getRecordNames()) {
    record <- new(tolower(paste0(name, "_record")))
    if (!is(record, "properties_record")) {
      next
    }
    subProperties <- properties %>% select(name) 
    if (subProperties %>% length() == 0) {
      next
    }
    for (subProperty in subProperties@list) {
      compartment <- model@compartments %>% getByIndex(Compartment(index=subProperty@compartment))
      equation <- Equation(compartment %>% getName(), subProperty@rhs, comment=subProperty@comment)
      record <- record %>% add(equation)
    }
    records <- records %>% add(record)
  }
  return(records %>% sort())
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
  if (odeRecord %>% length() == 0) {
    return(compartments)
  }
  odeCounter <- 0
  for (statement in odeRecord@statements@list) {
    if (is(statement, "ode")) {
      odeCounter <- odeCounter + 1
      name <- statement@lhs
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
  for (equation in record@statements@list) {
    if (!is(equation, "equation")) {
      stop("Properties record may only contain equations at this stage")
    }
    cmtName <- equation@lhs
    compartment <- compartments %>% getByName(cmtName)
    if (length(compartment) == 0) {
      stop(paste0("Compartment undefined: '", cmtName, "' in record ", record %>% getName()))
    }
    property <- init
    property@compartment <- compartment@index
    property@rhs <- equation@rhs
    property@comment <- equation@comment 
    compartments <- compartments %>% add(property)
  }
  return(compartments)
}

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

#' @rdname getEquation
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
#----                             getRecordNames                            ----
#_______________________________________________________________________________

getRecordNames <- function() {
  return(c("MAIN", "ODE", "F", "LAG", "DURATION", "RATE", "INIT", "ERROR"))
}

#_______________________________________________________________________________
#----                            hasEquation                                ----
#_______________________________________________________________________________

#' @rdname hasEquation
setMethod("hasEquation", signature=c("code_records", "character"), definition=function(object, lhs) {
  for (record in object@list) {
    if (record %>% hasEquation(lhs=lhs)) {
      return(TRUE)
    }
  }
  return(FALSE)
})

#_______________________________________________________________________________
#----                                read.model                             ----
#_______________________________________________________________________________

#' Remove all trailing line breaks.
#' 
#' @param x character vector
#' @return a character vector
removeTrailingLineBreaks <- function(x) {
  lenX <- x %>% length()
  if (lenX > 0) {
    res <- rle(isEmptyLine(x))
    lastValue <- res$values[res$values %>% length()]
    if (lastValue) {
      len <- res$lengths[res$values %>% length()]
      x <- x[-((lenX-len+1):lenX)]
    }
  }
  return(x)
}

addContentToRecord <- function(record, content) {
  # In all cases, we remove trailing line breaks
  content <- content %>% removeTrailingLineBreaks()
  
  if (is(record, "properties_record")) {
    record@statements <- parseProperties(content)
  } else if (is(record, "statements_record")) {
    record@statements <- parseStatements(content)
  } else {
    stop("Record must be either a 'properties_record' or a 'statements_record'")
  }
  return(record)
}

#' Read model file.
#' 
#' @param file path to records
#' @return records object
#' @export
read.model <- function(file) {
  allLines <- readLines(con=file)
  records <- CodeRecords()
  
  # Read all records
  lastLineIndexInPrevRecord <- 1
  for (index in seq_along(allLines)) {
    line <- allLines[index]
    if (isRecordDelimiter(line)) {
      recordDelimiter <- getRecordDelimiter(line)
      
      # Create empty record and add it to list
      record <- new(paste0(tolower(recordDelimiter), "_record"))
      records@list <- c(records@list, record)
      
      # Add lines to previous record
      if (records %>% length() > 1) {
        content <- allLines[(lastLineIndexInPrevRecord + 1):(index-1)]
        prevRecordIndex <- records %>% length() - 1
        records@list[[prevRecordIndex]] <-
          addContentToRecord(records@list[[prevRecordIndex]], content)
      }
      lastLineIndexInPrevRecord <- index
    }
  }
  # Filling in with lines of last record
  content <- allLines[(lastLineIndexInPrevRecord + 1):length(allLines)]
  lastRecordIndex <- records %>% length()
  records@list[[lastRecordIndex]] <-
    addContentToRecord(records@list[[lastRecordIndex]], content)
  
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

#' @rdname removeEquation
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

#' @rdname replaceEquation
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

#' @rdname sort
setMethod("sort", signature=c("code_records"), definition=function(x, decreasing=FALSE, ...) {
  names <- x@list %>% purrr::map_chr(~.x %>% getName())

  # Reorder
  names <- factor(names, levels=getRecordNames(), labels=getRecordNames())
  order <- order(names)
  
  # Apply result to original list
  x@list <- x@list[order]
  return(x)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' @rdname write
setMethod("write", signature=c("code_records", "character"), definition=function(object, file, ...) {
  # The model is needed to get the compartment properties
  model <- processExtraArg(args=list(...), name="model")
  if (is.null(model)) {
    warning("model not provided, compartment properties will be lost")
    object <- object %>% sort()
  } else {
    # Add transient records and sort
    object <- object %>% addPropertiesRecords(model)
  }

  # Write code record
  code <- NULL
  for (record in object@list) {
    code <- code %>% append(paste0("[", record %>% getName(), "]"))
    for (statement in record@statements@list) {
      code <- code %>% append(statement %>% toString())
    }
    code <- code %>% append("") # write.table will add a new line
  }
  write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})


