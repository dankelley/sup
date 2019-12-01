#' sup: A Package for Simple Error Propagation
#'
#' The sup package provides functions for handling simple
#' arithmetic operations for scalar quantities that are subject
#' to uncertainty. This is handled with binary operators:
#' `%+%` for addition, `%-%` for subtraction, `%*%` for multiplication
#' and `%/%` for division. These operators can work with vectors
#' of length 1 (which are interpreted as scalars without uncertainties),
#' with vectors of length 2 (in which case the second element is taken
#' as the uncertainty) or with objects created with [as.sup()], e.g.
#'```
#' 1 %*% 3                       # 3 +- 0
#' c(1, 0.1) %*% c(3, 0.3)       # 3 +- 0.4242641
#' c(1, 0.1) %*% as.sup(3, 0.3)  # 3 +- 0.4242641
#'```
#'
#' @section A note on left-to-right operator precedence:
#'
#' It is important to note that the sup operators work from
#' left to right, which is unlike the normal state of affairs
#' in R.  Thus, for example,
#'```
#' c(1, 0) %+% c(2, 0) %*% c(3, 0)
#'```
#' evaluates to `9 +- 0`, because the addition is done before
#' the multiplication, whereas conventonal R precedence
#' would suggest `7 +- 0`.  Parentheses are the solution
#' to this problem, e.g. writing
#'```
#' c(1, 0) %+% (c(2, 0) %*% c(3, 0))
#'```
#' in this case.
#'
#' @docType package
#'
#' @name sup
NULL


#' Create a sup object.
#'
#' @param v Either a vector of length 2, in which case it is returned
#' unchanged, or a vector of length 1, in which case `u` (which defaults
#' to zero) is taken to be the uncertainty. If missing, then `x`
#' must be supplied.
#'
#' @param u A numerical value indicating the uncertainty in `v` (ignored
#' unless `length(u)` is 1, and ignored if `x` is supplied).
#'
#' @param x A numerical vector of values. If this is given,
#' then the other arguments are ignored, and the return
#' value consists of the mean and the standard deviation
#' of x.
#'
#' @return An object of class `sup`.
#'
#' @examples
#' as.sup(10)       # 10 +- 0
#' as.sup(10, 1)    # 10 +- 1
#' as.sup(c(10, 1)) # 10 +- 1
#' as.sup(x=1:10)   # 5.5 +- 3.02765
#'
#' @importFrom stats sd
#' @export
as.sup <- function(v, u=0, x)
{
    if (!missing(x)) {
        rval <- c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
    } else {
        rval <- if (1 == length(v)) c(v, u) else if (2 == length(v)) v else stop("v must be of length 1 or 2")
    }
    class(rval) <- "sup"
    rval
}

#' Add two uncertain items
#'
#' @template binaryTemplate
#'
#' @examples
#' c(10, 1) %+% c(2, 0.5)
#'
#' @export
`%+%` <- function(A, B)
{
    A <- as.sup(A)
    B <- as.sup(B)
    value <- A[1] + B[1]
    uncertainty <- sqrt(A[2]^2 + B[2]^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Subtract two uncertain items
#'
#' @template binaryTemplate
#'
#' @examples
#' c(10, 1) %-% c(2, 0.5)
#'
#' @export
`%-%` <- function(A, B)
{
    A <- as.sup(A)
    B <- as.sup(B)
    value <- A[1] - B[1]
    uncertainty <- sqrt(A[2]^2 + B[2]^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Multiply two uncertain items
#'
#' @template binaryTemplate
#'
#' @examples
#' c(10, 1) %*% c(2, 0.5)
#' @export
`%*%` <- function(A, B)
{
    A <- as.sup(A)
    B <- as.sup(B)
    value <- A[1] * B[1]
    uncertainty <- value * sqrt((A[2]/A[1])^2+(B[2]/B[1])^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Divide two uncertain items
#'
#' @template binaryTemplate
#'
#' @examples
#' c(10, 1) %/% c(2, 0.5)
#' @export
`%/%` <- function(A, B)
{
    A <- as.sup(A)
    B <- as.sup(B)
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
#' print(as.sup(10, 1))
#' as.sup(10, 1)
#' c(10, 1) %*% c(100, 5)
print.sup <- function(x, ...)
{
    cat(x[1], "+-", x[2], "\n")
}

#' Sine function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' sin(as.sup(pi/2, pi/8))
sin.sup <- function(x)
{
    x <- as.sup(x)
    value <- sin(x[1])
    uncertainty <- x[2] * cos(x[1])
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Cosine function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' cos(as.sup(pi/2, pi/8))
cos.sup <- function(x)
{
    x <- as.sup(x)
    value <- cos(x[1])
    uncertainty <- x[2] * sin(x[1])
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Tangent function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' tan(as.sup(pi/2, pi/8))
tan.sup <- function(x)
{
    x <- as.sup(x)
    value <- tan(x[1])
    uncertainty <- x[2] / cos(x[1])^2
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Exponential function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' exp(as.sup(2, 0.2))
exp.sup <- function(x)
{
    x <- as.sup(x)
    value <- exp(x[1])
    uncertainty <- x[2] * value
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Square-root function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' sqrt(as.sup(2, 0.2))
sqrt.sup <- function(x)
{
    x <- as.sup(x)
    value <- sqrt(x[1])
    uncertainty <- x[2] / (2 * value)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Logarithm function
#'
#' @template unaryTemplate
#' @param base Numerical value giving the base of he logarithm,
#' with the default yielding the natural logarithm.
#'
#' @export
#'
#' @examples
#' log(as.sup(2, 0.2))
log.sup <- function(x, base=exp(1))
{
    x <- as.sup(x)
    value <- log(x[1])
    uncertainty <- x[2] / (value * log(base))
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Hyperbolic cosine function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' cosh(as.sup(2, 0.2))
cosh.sup <- function(x)
{
    x <- as.sup(x)
    value <- cosh(x[1])
    uncertainty <- x[2] * sinh(value)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Hyperbolic sine function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' sinh(as.sup(2, 0.2))
sinh.sup <- function(x)
{
    x <- as.sup(x)
    value <- sinh(x[1])
    uncertainty <- x[2] * cosh(value)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

#' Hyperbolic tangent function
#'
#' @template unaryTemplate
#'
#' @export
#'
#' @examples
#' tanh(as.sup(2, 0.2))
tanh.sup <- function(x)
{
    x <- as.sup(x)
    value <- tanh(x[1])
    uncertainty <- x[2] * (1 - tanh(value)^2)
    rval <- c(value, uncertainty)
    class(rval) <- "sup"
    rval
}

