#_______________________________________________________________________________
#----                       code_record class                               ----
#_______________________________________________________________________________

checkCodeRecord <- function(object) {
  check1 <- expectZeroOrMore(object, "code")
  check2 <- expectOne(object, "transient")
  return(c(check1, check2))
}

setClass(
  "code_record",
  representation(
    code = "character",
    transient = "logical"
  ),
  contains = "pmx_element",
  prototype = prototype(transient=FALSE),
  validity = checkCodeRecord
)

#_______________________________________________________________________________
#----                      statements_record class                           ----
#_______________________________________________________________________________

#' 
#' Statement record class.
#' 
#' @slot statements model statements
setClass(
  "statements_record",
  representation(
    statements = "model_statements"
  ),
  prototype=prototype(statements=ModelStatements()),
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
  contains = "statements_record"
)

#' 
#' Create MAIN code record.
#' 
#' @param code code record
#' @export
MainRecord <- function(code=character()) {
  return(new("main_record", code=code))
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
#' @export
OdeRecord <- function(code=character()) {
  return(new("ode_record", code=code))
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
  contains = "code_record",
  prototype = prototype(transient=TRUE)
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
  contains = "code_record",
  prototype = prototype(transient=TRUE)
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
  contains = "code_record",
  prototype = prototype(transient=TRUE)
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
  contains = "code_record",
  prototype = prototype(transient=TRUE)
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
  contains = "code_record",
  prototype = prototype(transient=TRUE)
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
  contains = "statements_record"
)

#' 
#' Create ERROR code record.
#' 
#' @param code code record
#' @export
ErrorRecord <- function(code=character()) {
  return(new("error_record", code=code))
}

#_______________________________________________________________________________
#----                            addEquation                                ----
#_______________________________________________________________________________

#' 
#' Get equation index.
#' 
#' @param object code record
#' @param lhs left-hand-side variable to search for
#' @return index in this record or -1 if not found 
#' @export
getEquationIndex <- function(object, lhs) {
  index <- object@statements %>% indexOf(Equation(lhs, ""))
  if (index %>% length() == 0) {
    return(-1)
  } else {
    return(index)
  }
}

#' @param before index or variable, may be used to insert an equation at a specific position, before this index (in record) or variable
#' @param after index or variable, may be used to insert an equation at a specific position, after this index (in record) or variable
#' @rdname addEquation
setMethod("addEquation", signature=c("code_record", "character", "character"), definition=function(object, lhs, rhs, before=NULL, after=NULL) {
  if (!is.null(before)) {
    index <- ifelse(is.numeric(before), before, getEquationIndex(object, before)) - 1
  } else if(!is.null(after)) {
    index <- ifelse(is.numeric(after), after, getEquationIndex(object, after))
  } else {
    index <- NULL
  }
  if (isODE(paste0(lhs, "="))) {
    eq <- Ode(extractTextBetweenBrackets(lhs), rhs)
  } else {
    eq <- Equation(lhs, rhs)
  }
  if (is.null(index)) {
    object@statements@list <- object@statements@list %>% append(eq)
  } else {
    object@statements@list <- object@statements@list %>% append(eq, after=index)
  }

  return(object)
})

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

#' @rdname getEquation
setMethod("getEquation", signature=c("code_record", "character"), definition=function(object, lhs) {
  index <- getEquationIndex(object, lhs)
  if (index == -1) {
    return(NULL)
  } else {
    eq <- object@statements %>% getByIndex(index)
    return(eq@rhs)
  }
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
#----                            hasEquation                                ----
#_______________________________________________________________________________

#' @rdname hasEquation
setMethod("hasEquation", signature=c("code_record", "character"), definition=function(object, lhs) {
  index <- getEquationIndex(object, lhs)
  if (index == -1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' @rdname length
setMethod("length", signature=c("statements_record"), definition=function(x) {
  return(x@statements %>% length())
})

#_______________________________________________________________________________
#----                           removeEquation                              ----
#_______________________________________________________________________________

#' @rdname removeEquation
setMethod("removeEquation", signature=c("code_record", "character"), definition=function(object, lhs) {
  index <- getEquationIndex(object, lhs)
  if (index != -1) {
    object@statements@list <- object@statements@list[-index]
  }
  return(object)
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

#' @rdname replaceEquation
setMethod("replaceEquation", signature=c("code_record", "character", "character"), definition=function(object, lhs, rhs) {
  index <- getEquationIndex(object, lhs)
  if (index != -1) {
    eq <- Equation(lhs, rhs)
    object@statements <- object@statements %>% replace(eq)
  }
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________


setMethod("show", signature=c("code_record"), definition=function(object) {
  cat("[", object %>% getName(), "]\n", sep="")
  if (is(object, "statements_record")) {
    show(object@statements)
  } else {
    cat(object@code, sep="\n")
  }
})
