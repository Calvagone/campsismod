
setClass(
  "rxode_model",
  representation(
    code = "character",
    theta = "numeric",
    omega = "matrix",
    sigma = "matrix"
  )
)