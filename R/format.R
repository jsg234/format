#' Round to number of significant figures with trailing zeroes
#'
#' This function rounds values to a number of sig figs but preserves trailing zeroes.
#' @param x some numeric value
#' @param digits number of significant figures
#' @keywords formatting
#' @export
#' @examples
#' fmt(0.0103, 2)
#'
fmt <- function(x, digits) {
  signif(x, digits) %>%
    purrr::map_chr(sprintf, fmt = paste0('%#.', digits,'g'))
}
#' Format p-values
#'
#' This function rounds p-values
#' @param x some numeric value
#' @param digits number of significant figures
#' @keywords formatting
#' @export
#' @examples
#' fmt(0.0103, 2)
#'
 pval <- function(x, digits) {
   cutoff <- 10^(-digits)
   if_else(x > cutoff,
           round(x, digits) %>%
             as.character(),
           glue('< {cutoff}') %>%
             as.character())
 }
