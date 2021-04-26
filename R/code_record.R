#_______________________________________________________________________________
#----                       code_record class                               ----
#_______________________________________________________________________________

checkCodeRecord <- function(object) {
  return(expectZeroOrMore(object, "code"))
}

setClass(
  "code_record",
  representation(
    code = "character"
  ),
  contains = "pmx_element",
  validity = checkCodeRecord
)

#_______________________________________________________________________________
#----                            PK record                                  ----
#_______________________________________________________________________________

#' @export
setClass(
  "pk_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create PK code record.
#' 
#' @param code code record
#' @export
PkRecord <- function(code=character()) {
  return(new("pk_record", code=code))
}

#_______________________________________________________________________________
#----                           PRED record                                 ----
#_______________________________________________________________________________

#' @export
setClass(
  "pred_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create PRED code record.
#' 
#' @param code code record
#' @export
PredRecord <- function(code=character()) {
  return(new("pred_record", code=code))
}

#_______________________________________________________________________________
#----                            DES record                                 ----
#_______________________________________________________________________________

#' @export
setClass(
  "des_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create DES code record.
#' 
#' @param code code record
#' @export
DesRecord <- function(code=character()) {
  return(new("des_record", code=code))
}

#_______________________________________________________________________________
#----                           ERROR record                                ----
#_______________________________________________________________________________


#' @export
setClass(
  "error_record",
  representation(
  ),
  contains = "code_record"
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
#----                              add                                      ----
#_______________________________________________________________________________

#' 
#' Get equation index.
#' 
#' @param object code record
#' @param lhs left-hand-side variable to search for
#' @return index in this record or -1 if not found 
#' @export
getEquationIndex <- function(object, lhs) {
  for (lineIndex in seq_along(object@code)) {
    line <- object@code[lineIndex]
    if (isEquation(line) && extractLhs(line) %>% trim()==lhs) {
      return(lineIndex)
    }
  }
  return(-1)
}

setMethod("add", signature=c("code_record", "character"), definition=function(object, x, before=NULL, after=NULL) {
  if (!is.null(before)) {
    index <- ifelse(is.numeric(before), before, getEquationIndex(object, before)) - 1
  } else if(!is.null(after)) {
    index <- ifelse(is.numeric(after), after, getEquationIndex(object, after))
  } else {
    index <- NULL
  }
  
  if (is.null(index)) {
    object@code <- object@code %>% append(x)
  } else {
    object@code <- object@code %>% append(x, after=index)
  }

  return(object)
})

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

setMethod("getEquation", signature=c("code_record", "character"), definition=function(object, lhs) {
  index <- getEquationIndex(object, lhs)
  if (index == -1) {
    return(NULL)
  } else {
    return(object@code[index])
  }
})

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("pk_record"), definition=function(x) {
  return("PK")
})

setMethod("getName", signature=c("pred_record"), definition=function(x) {
  return("PRED")
})

setMethod("getName", signature=c("des_record"), definition=function(x) {
  return("DES")
})

setMethod("getName", signature=c("error_record"), definition=function(x) {
  return("ERROR")
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("code_record"), definition=function(x) {
  return(length(x@code))
})

#_______________________________________________________________________________
#----                           removeEquation                              ----
#_______________________________________________________________________________

setMethod("removeEquation", signature=c("code_record", "character"), definition=function(object, lhs) {
  index <- getEquationIndex(object, lhs)
  if (index != -1) {
    object@code <- object@code[-index]
  }
  return(object)
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

setMethod("replaceEquation", signature=c("code_record", "character", "character"), definition=function(object, lhs, rhs) {
  index <- getEquationIndex(object, lhs)
  if (index != -1) {
    line <- object@code[index]
    object@code[index] <- paste0(extractLhs(line), "=", rhs)
  }
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________


setMethod("show", signature=c("code_record"), definition=function(object) {
  cat("[", object %>% getName(), "]\n", sep="")
  cat(object@code, sep="\n")
})
