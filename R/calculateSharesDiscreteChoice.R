#' Calculate shares based on discrete choice model.
#'
#' Function works for the use of generic preferences as well as for inconvenience costs.
#' If no preferences are provided the function sets them to one which is equivalent to
#' pure description by inconvenience costs.
#'
#' @author Johanna Hoppe
#' @param totPrice total price of an option in a branch of the decision tree
#' @param lambda exponent that determines the price sensitivity of the decision model
#' @param pref optional use of generic preference fators
#' @returns share of option in a branch of the decision tree
#' @import data.table
#' @export

calculateSharesDiscreteChoice <- function(totPrice, lambda, pref = NULL) {

  if (!is.null(pref)) {
    if (sum(totPrice) == 0) {
      share <- 1 # e.g. for active modes with totPrice of zero on FV level
    } else {
      share <- pref * totPrice^lambda / (sum(pref * totPrice^lambda))
    }
  } else {
    share <- totPrice^lambda / (sum(totPrice^lambda))
  }

  return(share)
}
