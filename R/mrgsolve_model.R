
setClass(
  "mrgsolve_model",
  representation(
    param = "character",
    cmt = "character",
    main = "character",
    ode = "character",
    omega = "character",
    sigma = "character",
    table = "character"
    
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("pmx_model", "mrgsolve_type"), definition=function(object, dest, ...) {
  return(
    new(
      "mrgsolve_model",
      param = mrgsolveParam(object),
      cmt = mrgsolveCompartment(object),
      main = mrgsolveMain(object),
      ode = mrgsolveOde(object),
      omega = mrgsolveMatrix(object, type="omega"),
      sigma = mrgsolveMatrix(object, type="sigma"),
      table = mrgsolveTable(object)
    )
  )
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

setMethod("toString", signature=c("mrgsolve_model"), definition=function(object, ...) {
  cpp <- object@param
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@cmt)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@omega)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@sigma)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@main)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@ode)
  cpp <- cpp %>% append("")
  cpp <- cpp %>% append(object@table)
  cpp <- paste0(cpp, collapse="\n")
  return(cpp)
})

