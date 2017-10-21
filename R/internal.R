#' Validate zoom data
#'
#' This function checks that zoom data are specified in a correct format.
#'
#' @param x \code{\link{character}} object.
#'
#' @return \code{\link{logical}} is it valid?
#'
#' @examples
#' # valid inputs
#' is_valid_zoom("10")
#' is_valid_zoom("1-10")
#'
#' # invalid inputs
#' is_valid_zoom("A")
#' is_valid_zoom("10.1")
#' is_valid_zoom("1-10.1")
#'
#' @noRd
is_valid_zoom <- function(x) {
  assertthat::assert_that(assertthat::is.string(x))
  if (grepl("-", x))
   x <- strsplit(x, "-")[[1]]
  for (i in seq_along(x)) {
    if (suppressWarnings(is.na(as.integer(x[i]))))
      return(FALSE)
    if (as.character(suppressWarnings(as.integer(x[i]))) != x[i])
      return(FALSE)
  }
  return(TRUE)
}
