#' @param x An uncertain value, in one of three possible forms:
#' (1) an object of class `"sup"`, created with [as.sup()],
#' (2) a numeric vector of length two containing a value as the first
#' entry and its uncertainty as the second entry, or
#' (3) a numeric vector of length 1, in which case the uncertainty
#' is assumed to be zero.
#'
#' @return A two-element vector of class `"sup"`, which
#' contains result of the computation as the first entry,
#' and the uncertainty as the second.  (This class definition
#' means that printing the item will produce a a well-formated
#' display.)

