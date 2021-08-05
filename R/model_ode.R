
#_______________________________________________________________________________
#----                             ode class                                 ----
#_______________________________________________________________________________

validateOde <- function(object) {
  return(expectOne(object, c("lhs", "rhs")))
}

#' 
#' ODE class. Any statement in the form d/dt(A) = B.
#' 
#' @export
setClass(
  "ode",
  representation(
  ),
  contains = "equation",
  validity = validateOde
)