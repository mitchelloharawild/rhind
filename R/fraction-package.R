#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import vctrs
#' @importFrom cli cli_abort
#' @importFrom rlang caller_env
#' @importFrom stats median quantile
#' @useDynLib fraction, .registration = TRUE, .fixes = "C_"
## usethis namespace: end
NULL

.onLoad <- function(...) {
  s3_register("pillar::pillar_shaft", "fraction")
  # match() and %in% (mtfrm() was added in R 4.2.0)
  if (exists("mtfrm", baseenv(), inherits = FALSE)) {
    registerS3method("mtfrm", "fraction", mtfrm.fraction, envir = baseenv())
  }
}
