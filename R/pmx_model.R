
setClass(
  "pmx_model",
  representation(
    code = "character",       # Mandatory
    parameters = "parameters" # Mandatory    
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Export function.
#' 
#' @param object generic object
#' @param dest destination
#' @return specific object depending on given destination
#' @export
export <- function(object, dest) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest) {
  standardGeneric("export")
})

setMethod("export", signature=c("pmx_model", "character"), definition=function(object, dest) {
  if (dest=="RxODE") {
    return(
      new(
        "rxode_model",
        code = object@code,
        theta = rxodeParams(object),
        omega = rxodeMatrix(object, type = "omega"),
        sigma = rxodeMatrix(object, type = "sigma")
      )
    )
  } else {
    stop("Only RxODE is supported for now")
  }
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write generic object.
#' 
#' @param object generic object
#' @param file path of the output dir or ZIP filename
#' @param ... extra arguments
#' @export
write <- function(object, file, ...) {
  stop("No default function is provided")
}

setGeneric("write", function(object, file, ...) {
  standardGeneric("write")
})

setMethod("write", signature=c("pmx_model", "character"), definition=function(object, file, zip=TRUE) {
  code <- object@code
  parameters <- object@parameters
  theta <- parameters %>% filter(type="theta")
  omega <- parameters %>% filter(type="omega")
  sigma <- parameters %>% filter(type="sigma")
  
  if (zip) {
    
  } else {
    if (dir.exists(file)) {
      # do nothing
    } else {
      dir.create(file)
    }
    theta %>% write(file=file.path(file, "theta.csv"))
    omega %>% write(file=file.path(file, "omega.csv"))
    sigma %>% write(file=file.path(file, "sigma.csv"))
    write.table(x=code, file=file.path(file, "model.mod"), row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
})

s4ToDataframe <- function(object) {
  names <- slotNames(object)
  list <- lapply(names, function(name) slot(object, name))
  as.data.frame(setNames(list, names))
}

parametersToDataframe <- function(object) {
  retValue <- purrr::map_df(object@list, .f=s4ToDataframe)
  return(retValue)
}

setMethod("write", signature=c("parameters", "character"), definition=function(object, file) {
  df <- parametersToDataframe(object)
  write.csv(df, file=file, row.names=FALSE)
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Read generic object.
#' 
#' @param file path where to read the file
#' @param ... extra arguments
#' @export
read <- function(file, ...) {
  stop("No default function is provided")
}

setGeneric("read", function(file, ...) {
  standardGeneric("read")
})

setMethod("read", signature=c("character"), definition=function(file, ...) {
  return("TODO")
})

dataframeToParameter <- function(row, type) {
  param <- NULL
  
  if (type=="theta") {
    param <- new("theta", name=row$name, index=row$index, suffix=row$suffix, fix=row$fix, value=row$value)
  } else if(type=="omega" | type=="sigma") {
    param <- new("omega", name=row$name, index=row$index, index2=row$index2, suffix=row$suffix, fix=row$fix, value=row$value)
  } else {
    stop(paste0("type must be one of: theta, omega or sigma"))
  }
  return(param)
}

setMethod("read", signature=c("character"), definition=function(file, type) {
  df <- read.csv(file=file) %>% dplyr::mutate(ROWID=dplyr::row_number())
  list <- df %>% plyr::dlply(.variables="ROWID", .fun=dataframeToParameter, type=type)
  attributes(list) <- NULL
  retValue <- new("parameters", list=list)
})
