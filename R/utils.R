# Util files for the package
# by JJAV 20220425
############################

#' Linear sum within a data.frame
#'
#' Perform a sum with the linear combination between the variables
#' and the coefficients
#'
#' @param .data the data.frame with the variables
#' @param vars the variables  to select from the data.frame
#' @param coeffs the coefficients to apply
#' @return a vector with the sum of each variable value multiply by the coefficient.
#' @export
linsum <- function(.data, vars , coeffs) {
  stopifnot("vars and coefficients must be of the same lenght" = length(vars) == length(coeffs))
  stopifnot("vars must be valid names in data"= all(vars %in% names(.data)))
  as.vector(as.matrix(.data[vars]) %*% coeffs)
}

#' Confirm is a single number
#'
#' Evaluates if the argument is a single number
#' @param x a variable to evaluate
#' @return `TRUE` if is a single number, `FALSE` otherwise
#' @export
#' @examples
#' is_single_number(3)  #TRUE
#' is_single_number(c(3,3,3)) #FALSE
#' is_single_number(list(a=3)) #FALSE
#' is_single_number("3") #FALSE
is_single_number<- function(x){
  is.atomic(x) &
    length(x)==1 &
    is.numeric(x)
}

#' Functions to help in time conversion
#'
#' This set of functions help in the time conversion, taking into account
#' generic times and not specific times. The conversions are based on the
#' assumption that 1 year is 365.25 days and is 12 months. There is no
#' adjustment for lap days or ours or difference of days between months
#'
#' @param x the time to convert
#' @return the converted time
#' @name fx_h_time
NULL

#' @describeIn fx_h_time convert days to months
#' @export
dtom <- function(x){x*12/365.25}

#' @describeIn fx_h_time convert months to days
#' @export
mtod <- function(x){x*365.25/12}

#' @describeIn fx_h_time convert days to years
#' @export
dtoy <- function(x){x/365.25}

#' @describeIn fx_h_time convert years to days
#' @export
ytod <- function(x){x*365.25}

#' @describeIn fx_h_time convert months to year
#' @export
mtoy <- function(x){dtoy(mtod(x))}

#' @describeIn fx_h_time convert years to months
#' @export
ytom <- function(x){dtom(ytod(x))}



# testdf <- data.frame(
#   id = c(1, 2, 3, 4, 5),
#   treatment = c(0, 0, 1, 1, 1),
#   age = c(1, 0, 0, 1, 1)
# ) %>%
#   mutate(lc = linsum(
#     .,
#     vars = c("treatment", "age"),
#     coeffs = c(0.5, 0.1)
#   )) %>%
#   mutate(lc2 = linsum(
#     .,
#     vars = c("age", "treatment"),
#     coeffs = c(0.1, 0.5)
#   ))
#
