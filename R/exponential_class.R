## Survival objects for simulations
## by JJAV 20220420

#' Factory of SURVIVAL objects with Exponential distributions
#'
#' Creates a SURVIVAL object with an Exponential distribution.
#'
#' @section Parameters:
#'
#' To create an exponential survival object the following
#' options are available:
#'
#' _`lambda`_ to specify the canonical parameter of the distribution, or
#'
#' _`surv`_ and _`t`_ for the proportion surviving (no events) at time t, or
#'
#' _`fail`_ and _`t`_ for the proportion failing (events) at time t
#'
#' lambda is defined as   {lambda = -log(surv) /t} or {lambda = -log((1-fail)) /t}
#'
#' The parameters should be spell correctly as partial matching is not available
#'
#' @param ... Parameters to define the distribution. See the Parameters for details
#' @return a SURVIVAL object of the exponential distribution family. See the
#' documentation of `s_factory` for the methods available for SURVIVAL objects
#' @importFrom stats runif
#' @export
#' @examples
#' s_exponential(lambda = 3)
#' s_exponential(surv = 0.4, t = 2)
#' s_exponential(fail = 0.6, t = 2)
s_exponential <- function(...) {
  params <- list(...)
  nparam <- names(params)

  # This function is the factory of the class
  .factory_exponential <- function(lambda) {
    iCum_Hfx <- function(H){
      stopifnot("Must be positive number" = all(H >= 0))
      H/lambda
    }
    structure(
      list(
        distribution = "EXPONENTIAL",
        params = list(lambda = lambda),
        sfx = function(t) {
          stopifnot("t must be numeric" = is.numeric(t))
          stopifnot("t must be positive number" = all(t >= 0))
          exp(-lambda*t)
        },
        hfx = function(t) {
          stopifnot("t must be numeric" = is.numeric(t))
          stopifnot("t must be positive number" = all(t >= 0))
          rep(lambda,length(t))
        },
        Cum_Hfx = function(t) {
          stopifnot("t must be numeric" = is.numeric(t))
          stopifnot("t must be positive number" = all(t >= 0))
          lambda*t
        },
        invCum_Hfx=iCum_Hfx,
        rsurv =  function(n){
          stopifnot("n must be numeric" = is.numeric(n))
          stopifnot("n must be positive number" = all(n > 0))
          iCum_Hfx(-log(runif(n)))
        },
        rsurvhr = function(hr){
          stopifnot("hr must be numeric" = is.numeric(hr))
          stopifnot("hr must be positive numbers > 0" = all(hr > 0))
          # Following Bender, Augustin and Blettner 2005
          iCum_Hfx(-log(runif(length(hr)))/hr)
        }
      ),
      class = c("SURVIVAL")
    )
  }

  # Definition based on lambda
  if (length(nparam == 1) &
      ("lambda" %in% nparam)) {
    stopifnot("lambda should be a single number" = is_single_number(params$lambda) )
    stopifnot("lambda should be greater than 0" = params$lambda > 0 )

    return(.factory_exponential(params$lambda))
  }

  # Definition based in proportion surviving and time
  if(
    length(nparam == 2) &
    all(c("surv","t") %in% nparam)) {
    stopifnot("surv must be a single number" = is_single_number(params$surv))
    stopifnot("surv must be greater than 0" = params$surv > 0)
    stopifnot("surv must be smaller than 1" = params$surv < 1)
    stopifnot("t must be a single number" = is_single_number(params$t))
    stopifnot("t must be greater than 0" = params$t > 0)

    lambda = -log(params$surv) / params$t
    return(.factory_exponential(lambda))
  }

  # Definition based on proportion failing and time
  if(
    length(nparam == 2) &
    all(c("fail","t") %in% nparam)) {
    stopifnot("fail must be a single number" = is_single_number(params$fail))
    stopifnot("fail must be greater than 0" = params$fail > 0)
    stopifnot("fail must be lower than 1" = params$fail < 1)
    stopifnot("t must be a single number" = is_single_number(params$t))
    stopifnot("t must be greater than 0" = params$t > 0)

    lambda = -log(1 -params$fail)/params$t
    return(.factory_exponential(lambda))
  }
  cat("\nError defining s_exponential object \n")
  cat("Valid parameters are: \n")
  cat("lambda: for the canonical parameter of the distribution, or\n")
  cat("surv, t: for the surviving proportion (no events) at time t, or\n")
  cat("fail, t: for the failure proportion (events) at time t \n")
  stop("Error in parameters")
}
