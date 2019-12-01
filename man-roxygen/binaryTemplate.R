#' @section A note on operator precedence:
#' The sup package does not follow conventional rules for operator
#' precedence. For example, in normal algebra, `1+2*3` is `7`,
#' because the multiplication operator takes precedence over the
#' addition operator. By contrast, the sup operators work from left
#' to right, so that e.g. (in a case with zero uncertainty)
#' the value of `1 %+% 2 %*% 3` is `9`, because
#' the sum is done *before* the multiplication. The use of parentheses
#' is highly encouraged to avoid confusion, e.g. write
#' `1 %+% (2 %*% 3)` to mimic conventional algebra.
#'
#' @param A An uncertain value, in one of three possible forms:
#' (1) an object of class `"sup"`, created with [as.sup()],
#' (2) a numeric vector of length two containing a value as the first
#' entry and its uncertainty as the second entry, or
#' (3) a numeric vector of length 1, in which case the uncertainty
#' is assumed to be zero.
#'
#' @param B analogous to A.
#'
#' @return A two-element vector of class `"sup"`, which
#' contains result of the computation as the first entry,
#' and the uncertainty as the second.  (This class definition
#' means that printing the item will produce a a well-formated
#' display.)

