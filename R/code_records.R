#_______________________________________________________________________________
#----                         code_records class                            ----
#_______________________________________________________________________________

setClass(
  "code_records",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="code_record"),
  validity = function(object) {
    hasUnknownStatements <-
      object@list %>% purrr::map_lgl(~.x@statements@list %>%
                                       purrr::map_lgl(~is(.x, "unknown_statement")) %>% any()) %>% any()
    if (hasUnknownStatements) {
      warning(
        "Model code contains unknown statements. Conversion to rxode2 and mrgsolve may lead to errors."
      )
    }
    return(TRUE)
  }
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
#' @keywords internal
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

findRecordByPosition <- function(object, pos) {
  if (is.null(pos)) {
    stop("pos is null")
  }
  for (record in object@list) {
    if (pos@by_index) {
      # Return first record
      return(record)
    } else if (pos@by_element) {
      # Element here should be a model statement
      # Check if current record contains the given element
      if (record %>% find(pos@element) %>% length() > 0) {
        return(record)
      }
    } else {
      stop("pos either by index or by element")
    }
  }
  stop("No code record found for specified position 'pos'")
}

#' @param pos position where x needs to be added in list
#' @rdname add
setMethod("add", signature=c("code_records", "model_statement"), definition=function(object, x, pos=NULL) {
  if (is.null(pos)) {
    if (is(x, "ode")) {
      recordName <- "ODE"
    } else {
      recordName <- "MAIN"
    }
    record <- object %>% getByName(recordName)
    if (isS4(record)) {
      # Existing record
      record <- record %>% add(x)
      object <- object %>% replace(record)
    } else {
      # New record
      record <- new(paste0(recordName %>% tolower(), "_record"))
      record <- record %>% add(x)
      object <- object %>% add(record) %>% sort()
    }
  } else {
    
    if (pos@by_element && is(pos@element, "code_record")) {
      # Special case (position by code record element)
      record <- object %>% find(pos@element)
      newRecordNeeded <- is.null(record)
      if (newRecordNeeded) {
        record <- pos@element
      }
      if (pos@after) {
        record <- record %>% add(x) # x is appended to the end
      } else {
        record <- record %>% add(x, Position(1, after=FALSE)) # x added at the beginning
      }
      if (newRecordNeeded) {
        object <- object %>% add(record)
      } else {
        object <- object %>% replace(record)
      }
      
    } else {
      # Usual case (position by index or model statement element)
      record <- object %>% findRecordByPosition(pos)
      record <- record %>% add(x, pos=pos)
      object <- object %>% replace(record)
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
      compartment <- model@compartments %>% find(Compartment(index=subProperty@compartment))
      equation <- Equation(compartment %>% toString(), subProperty@rhs, comment=subProperty@comment)
      record <- record %>% add(equation)
    }
    records <- records %>% add(record)
  }
  return(records %>% sort())
}

#_______________________________________________________________________________
#----                               delete                                  ----
#_______________________________________________________________________________

#' @rdname delete
setMethod("delete", signature=c("code_records", "model_statement"), definition=function(object, x) {
  copy <- object
  for (record in object@list) {
    if (!is.null(record %>% find(x))) {
      copy <- copy %>% replace(record %>% delete(x))
    }
  }
  return(copy)
})

#_______________________________________________________________________________
#----                                find                                   ----
#_______________________________________________________________________________

#' @rdname find
setMethod("find", signature=c("code_records", "model_statement"), definition=function(object, x) {
  copy <- object
  for (record in object@list) {
    statement <- record %>% find(x)
    if (!is.null(statement)) {
      return(statement)
    }
  }
  return(NULL)
})

#_______________________________________________________________________________
#----                          getCompartments                              ----
#_______________________________________________________________________________

#' Add ODE compartment to compartments object.
#'
#' @param compartments compartments object
#' @param ode ODE
#' @return a compartments object
#' @importFrom purrr discard map_chr
#' @keywords internal
#' 
addODECompartment <- function(compartments, ode) {
  if (!is(ode, "ode")) {
    stop("ode is not an ODE")
  }
  name <- ode@lhs
  
  cmtIndex <- compartments %>% length() + 1
  if (startsWith(name, prefix="A_")) {
    name <- gsub("^A_", "", name)
    if (name == as.character(cmtIndex)) {
      name <- NA
    }
  } else {
    stop(paste0("Compartment ", name, " does not start with 'A_'"))
  }
  compartment <- Compartment(index=cmtIndex, name=name)
  names <- compartments@list %>% purrr::map_chr(~.x@name) %>% purrr::discard(is.na)
  
  # Add compartment only if not there yet
  if (is.na(name) || !(name %in% names)) {
    compartments <- compartments %>% add(compartment)
  }
  return(compartments)
}

#' Detect all compartments names from the code records.
#' Only for model instantiation. Not exported.
#'
#' @param records code records
#' @return a list of compartments
#' @keywords internal
#' 
getCompartments <- function(records) {
  assertthat::assert_that(is(records, "code_records"), msg="records class is not 'code_records'")
  odeRecord <- records %>% getByName("ODE")
  compartments <- Compartments()
  if (odeRecord %>% length() == 0) {
    return(compartments)
  }
  for (statement in odeRecord@statements@list) {
    if (is(statement, "ode")) {
      compartments <- compartments %>% addODECompartment(ode=statement)
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
#' @keywords internal
#' 
addProperties <- function(compartments, records, name, init) {
  record <- records %>% getByName(name)
  if (record %>% length() == 0) {
    return(compartments)
  }
  # Filter on equations (line breaks and comments are accepted in properties record)
  for (equation in record@statements@list %>% purrr::keep(~is(.x, "equation"))) {
    cmtName <- equation@lhs
    compartment <- compartments@list %>% purrr::detect(~.x %>% toString() == cmtName)
    if (is.null(compartment)) {
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
#----                             getRecordNames                            ----
#_______________________________________________________________________________

getRecordNames <- function() {
  return(c("MAIN", "ODE", "F", "LAG", "DURATION", "RATE", "INIT", "ERROR"))
}

#_______________________________________________________________________________
#----                                read.model                             ----
#_______________________________________________________________________________

#' Remove all trailing line breaks.
#' 
#' @param x character vector
#' @return a character vector
#' @keywords internal
#' 
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
    record@statements <- parseStatements(content)
    # Because properties records are transient and will not be part of the final model
    # We validate here the content
    methods::validObject(record, complete=TRUE)
  } else if (is(record, "statements_record")) {
    record@statements <- parseStatements(content)
  } else {
    stop("Record must be either a 'properties_record' or a 'statements_record'")
  }
  return(record)
}

#' Read model file.
#' 
#' @param file path to file 'model.campsis'
#' @param text model file as text, character (single or multiple lines)
#' @return records object
#' @export
read.model <- function(file=NULL, text=NULL) {
  if (!is.null(file)) {
    allLines <- readLines(con=file, warn=FALSE)
  } else if (!is.null(text)) {
    if (text %>% length() == 1) {
      allLines <- strsplit(text, split="(\r\n)|(\r)|(\n)")[[1]]
    } else {
      allLines <- text
    }
  } else {
    stop("Please provide argument 'file' or 'text'")
  }
  
  records <- CodeRecords()

  # Read all records
  lastLineIndexInPrevRecord <- 1
  for (index in seq_along(allLines)) {
    line <- allLines[index]
    if (isStrictRecordDelimiter(line)) {
      # Extract record delimiter
      recordDelimiter <- getRecordDelimiter(line)
      
      # Extract a possible comment
      comment <- as.character(NA)
      if (hasComment(line)) {
        comment <- extractRhs(line, split="#") %>% trim()
      }
      
      # Create empty record and add it to list
      record <-
        tryCatch({
          new(paste0(tolower(recordDelimiter), "_record"), comment=comment)
        },
        error = function(cond) {
          stop(paste0("Record delimiter '", recordDelimiter, "' is unknown"))
        })
      records@list <- c(records@list, record)
      
      # Add lines to previous record
      if (records %>% length() > 1) {
        content <- allLines[(lastLineIndexInPrevRecord + 1):(index-1)]
        prevRecordIndex <- records %>% length() - 1
        records@list[[prevRecordIndex]] <-
          addContentToRecord(records@list[[prevRecordIndex]], content)
      } else  {
        # If no last record is present, check if content is detected:
        # i.e. anything but blank line(s) or CAMPSIS comment(s) before the first
        # record delimiter
        # If content is detected, throw an error
        if (lastLineIndexInPrevRecord==1 && index > 1) {
          content <- allLines[1:(index-1)]
          if (any(!grepl("^((\\s*)|(\\s*#.*))$", x=content))) {
            stop("Missing record delimiter at beginning of model")
          }
        }
      }
      lastLineIndexInPrevRecord <- index
    } else if (isRecordDelimiter(line)) {
      stop(paste0("Record delimiter '", line, "' is not valid"))
    }
  }
  # Filling in with lines of last record
  content <- allLines[(lastLineIndexInPrevRecord + 1):length(allLines)]
  lastRecordIndex <- records %>% length()
  if (lastRecordIndex==0) {
    stop("No record delimiter found in model")
  }
  records@list[[lastRecordIndex]] <-
    addContentToRecord(records@list[[lastRecordIndex]], content)
  
  return(records)
}

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature=c("code_records", "model_statement"), definition=function(object, x) {
  copy <- object
  for (record in object@list) {
    if (record %>% find(x) %>% length() > 0) {
      copy <- copy %>% replace(record %>% replace(x))
    }
  }
  return(copy)
})

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

#' @rdname replaceAll
setMethod("replaceAll", signature=c("code_records", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@list <- object@list %>% purrr::map(~.x %>% replaceAll(pattern=pattern, replacement=replacement, ...))
  return(object)
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
#' @importFrom utils write.table
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
    # Add record delimiter
    code <- code %>% append(writeRecordDelimiter(record))
    
    # Add all statements
    for (statement in record@statements@list) {
      code <- code %>% append(statement %>% toString())
    }
    code <- code %>% append("") # write.table will add a new line
  }
  utils::write.table(x=code, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
})


