
setClass(
  "mrgsolve_model",
  representation(
    param = "character",
    cmt = "character",
    main = "character",
    ode = "character",
    omega = "character",
    sigma = "character"
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
      sigma = mrgsolveMatrix(object, type="sigma")
    )
  )
})