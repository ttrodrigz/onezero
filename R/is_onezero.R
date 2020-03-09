#' Check if data is binary.
#'
#' Does a check to see if the data contains only 1's, 0's or NA's.
#'
#' @param x A vector.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @examples
#'
#' is_onezero(c(1, 0, NA))
#' is_onezero(c(1, 0, -1))
#'
#' dplyr::select_if(pets, is_onezero)
#'
#' @export
is_onezero <- function(x) {

    if (!is.numeric(x)) {
        return(FALSE)
    }

    accept <- c(1, 0, NA)

    !any(!x %in% accept)

}

