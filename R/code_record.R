#_______________________________________________________________________________
#----                     code_record class (ABSTRACT)                      ----
#_______________________________________________________________________________

checkNonODERecord <- function(object) {
  hasODE <- object@statements@list %>% purrr::map_lgl(~is(.x, "ode")) %>% any()
  errors <- character(0)
  if (hasODE) {
    errors <- errors %>% append("ODE detected in non ODE record")
  }
  return(errors)
}

#' 
#' Code record class. See this code record as an abstract class.
#' 2 implementations are possible:
#' - properties record (lag, duration, rate & bioavailability properties)
#' - statements record (main, ode & error records)
#' 
#' @slot comment a comment, single character value
#' @slot statements model statements
#' 
setClass(
  "code_record",
  representation(
    comment = "character",
    statements = "model_statements"
  ),
  contains = "pmx_element",
  prototype = prototype(statements=ModelStatements(), comment=as.character(NA)),
  validity = function(object) {
    return(TRUE)
  }
)

#_______________________________________________________________________________
#----                      statements_record class                           ----
#_______________________________________________________________________________

#' 
#' Properties record class.
#' 
setClass(
  "properties_record",
  representation(
  ),
  contains = "code_record",
  validity = function(object) {
    hasODE <- object@statements@list %>% purrr::map_lgl(~is(.x, "ode")) %>% any()
    hasUnknownStatement <- object@statements@list %>% purrr::map_lgl(~is(.x, "unknown_statement")) %>% any()
    hasIfStatement <- object@statements@list %>% purrr::map_lgl(~is(.x, "if_statement")) %>% any()
    errors <- character(0)
    if (hasODE) {
      errors <- errors %>% append("ODE detected in properties record")
    }
    if (hasUnknownStatement) {
      errors <- errors %>% append("Unknown statement detected in properties record")
    }
    if (hasIfStatement) {
      errors <- errors %>% append("IF-statement detected in properties record")
    }
    return(errors)
  }
)

#_______________________________________________________________________________
#----                      statements_record class                          ----
#_______________________________________________________________________________

#' 
#' Statements record class.
#' 
#' @export
setClass(
  "statements_record",
  representation(
  ),
  contains = "code_record"
)

#_______________________________________________________________________________
#----                           MAIN record                                 ----
#_______________________________________________________________________________

#' 
#' Main record class.
#' 
#' @export
setClass(
  "main_record",
  representation(
  ),
  contains = "statements_record",
  validity = checkNonODERecord
)

#' 
#' Create MAIN code record.
#' 
#' @param code code record
#' @export
MainRecord <- function(code=character()) {
  return(new("main_record", statements=parseStatements(code)))
}

#_______________________________________________________________________________
#----                            ODE record                                 ----
#_______________________________________________________________________________

#' 
#' ODE record class.
#' 
#' @export
setClass(
  "ode_record",
  representation(
  ),
  contains = "statements_record"
)

#' 
#' Create ODE code record.
#' 
#' @param code code record
#' @return an ODE code record
#' @export
OdeRecord <- function(code=character()) {
  return(new("ode_record", statements=parseStatements(code)))
}

#_______________________________________________________________________________
#----                              F record                                 ----
#_______________________________________________________________________________

#' 
#' Bioavailability record class.
#' 
#' @export
setClass(
  "f_record",
  representation(
  ),
  contains = "properties_record"
)

#_______________________________________________________________________________
#----                              LAG record                               ----
#_______________________________________________________________________________

#' 
#' Lag record class.
#' 
#' @export
setClass(
  "lag_record",
  representation(
  ),
  contains = "properties_record"
)

#_______________________________________________________________________________
#----                           DURATION record                             ----
#_______________________________________________________________________________

#' 
#' (Infusion)-duration record class.
#' 
#' @export
setClass(
  "duration_record",
  representation(
  ),
  contains = "properties_record"
)

#_______________________________________________________________________________
#----                             RATE record                               ----
#_______________________________________________________________________________

#' 
#' (Infusion)-rate record class.
#' 
#' @export
setClass(
  "rate_record",
  representation(
  ),
  contains = "properties_record"
)

#_______________________________________________________________________________
#----                             INIT record                               ----
#_______________________________________________________________________________

#' 
#' Init record class.
#' 
#' @export
setClass(
  "init_record",
  representation(
  ),
  contains = "properties_record"
)

#_______________________________________________________________________________
#----                           ERROR record                                ----
#_______________________________________________________________________________


#' 
#' Error record class.
#' 
#' @export
setClass(
  "error_record",
  representation(
  ),
  contains = "statements_record",
  validity = checkNonODERecord
)

#' 
#' Create ERROR code record.
#' 
#' @param code code record
#' @return an ERROR code record
#' @export
ErrorRecord <- function(code=character()) {
  return(new("error_record", statements=parseStatements(code)))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

#' @param pos position where x needs to be added in list
#' @rdname add
setMethod("add", signature=c("code_record", "model_statement"), definition=function(object, x, pos=NULL) {
  object@statements <- object@statements %>% add(x, pos=pos)
  return(object)
})

#' @rdname add
setMethod("add", signature=c("code_record", "code_record"), definition=function(object, x) {
  object@statements <- object@statements %>% add(x@statements)
  return(object)
})

#_______________________________________________________________________________
#----                                contains                               ----
#_______________________________________________________________________________

#' @rdname contains
setMethod("contains", signature=c("statements_record", "model_statement"), definition=function(object, x) {
  return(!is.null(object@statements %>% find(x)))
})

#_______________________________________________________________________________
#----                               delete                                  ----
#_______________________________________________________________________________

#' @rdname delete
setMethod("delete", signature=c("statements_record", "model_statement"), definition=function(object, x) {
  object@statements <- object@statements %>% delete(x)
  return(object)
})

#_______________________________________________________________________________
#----                                find                                   ----
#_______________________________________________________________________________

#' @rdname find
setMethod("find", signature=c("statements_record", "model_statement"), definition=function(object, x) {
  return(object@statements %>% find(x))
})

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature=c("main_record"), definition=function(x) {
  return("MAIN")
})

#' @rdname getName
setMethod("getName", signature=c("ode_record"), definition=function(x) {
  return("ODE")
})

#' @rdname getName
setMethod("getName", signature=c("f_record"), definition=function(x) {
  return("F")
})

#' @rdname getName
setMethod("getName", signature=c("lag_record"), definition=function(x) {
  return("LAG")
})

#' @rdname getName
setMethod("getName", signature=c("duration_record"), definition=function(x) {
  return("DURATION")
})

#' @rdname getName
setMethod("getName", signature=c("rate_record"), definition=function(x) {
  return("RATE")
})

#' @rdname getName
setMethod("getName", signature=c("init_record"), definition=function(x) {
  return("INIT")
})

#' @rdname getName
setMethod("getName", signature=c("error_record"), definition=function(x) {
  return("ERROR")
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' @rdname length
setMethod("length", signature=c("statements_record"), definition=function(x) {
  return(x@statements %>% length())
})

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature=c("statements_record", "model_statement"), definition=function(object, x) {
  object@statements <- object@statements %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

#' @rdname replaceAll
setMethod("replaceAll", signature=c("code_record", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@statements@list <- object@statements@list %>% purrr::map(~.x %>% replaceAll(pattern=pattern, replacement=replacement, ...))
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("code_record"), definition=function(object) {
  cat(writeRecordDelimiter(object), "\n", sep="")
  show(object@statements)
})

#' 
#' Write record delimiter line.
#' 
#' @param object code record
#' @return a record delimiter line
#' @keywords internal
writeRecordDelimiter <- function(object) {
  recordDelimiter <- paste0("[", object %>% getName(), "]")
  recordDelimiter <- recordDelimiter %>% appendComment(object=object, dest="campsis")
  return(recordDelimiter)
}
