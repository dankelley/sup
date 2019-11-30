#' sup: A Package for Simple Error Propagation
#'
#' The sup package provides functions for handling simple
#' error propagaation calculations. This is mainly handled
#' with binary operators such as `%+$` for addition.
#'
#' @docType package
#'
#' @name sup
NULL


#' Create a sup object.
#'
#' @param v either a vector of length 2, in which case it is returned
#' unchanged, or a vector of length 1, in which case `u` (which defaults
#' to zero) is taken to be the uncertainty.
#'
#' @param u a numerical value indicating the uncertainty in `v` (ignored
#' unless length(u) is 1).
#'
#' @return An object of class `sup`.
#'
#' @examples
#' sup(10)
#' sup(10, 1)
#' sup(c(10, 1))
#'
#' @export
sup <- function(v, u=0)
{
    rval <- if (1 == length(v)) c(v, u) else if (2 == length(v)) v else stop("v must be of length 1 or 2")
    class(rval) <- "sup"
    rval
}

#' Add two uncertain items
#'
#' @template supTemplate
#'
#' @examples
#' c(10, 1) %+% c(2, 0.5)
#'
#' @export
`%+%` <- function(A, B)
{
    A <- sup(A)
    B <- sup(B)
    value <- A[1] + B[1]
    uncertainty <- sqrt(A[2]^2 + B[2]^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Subtract two uncertain items
#'
#' @template supTemplate
#'
#' @examples
#' c(10, 1) %-% c(2, 0.5)
#'
#' @export
`%-%` <- function(A, B)
{
    A <- sup(A)
    B <- sup(B)
    value <- A[1] - B[1]
    uncertainty <- sqrt(A[2]^2 + B[2]^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Multiply two uncertain items
#'
#' @template supTemplate
#'
#' @examples
#' c(10, 1) %*% c(2, 0.5)
#' @export
`%*%` <- function(A, B)
{
    A <- sup(A)
    B <- sup(B)
    value <- A[1] * B[1]
    uncertainty <- value * sqrt((A[2]/A[1])^2+(B[2]/B[1])^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Divide two uncertain items
#'
#' @template supTemplate
#'
#' @examples
#' c(10, 1) %/% c(2, 0.5)
#' @export
`%/%` <- function(A, B)
{
    A <- sup(A)
    B <- sup(B)
    value <- A[1] / B[1]
    uncertainty <- value * sqrt((A[2]/A[1])^2+(B[2]/B[1])^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Print an uncertain value
#' @param x a `up`-class object to be printed
#' @param ... ignored.
#' @export
#'
#' @examples
#' print(sup(10, 1))
#' sup(10, 1)
#' c(10, 1) %*% c(100, 5)
print.sup <- function(x, ...)
{
    cat(x[1], "+-", x[2], "\n")
}

