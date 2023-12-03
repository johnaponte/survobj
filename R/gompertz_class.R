## Gompertz survival objects for simulations
## by JJAV 20231128


#' Factory of SURVIVAL objects with Gompertz distributions
#'
#' Creates a SURVIVAL object with an Gompertz distribution.
#'
#' @section Parameters:
#'
#' To create an exponential survival object the following
#' options are available:
#'
#' _`scale`_ and _`shape`_ to specify the canonical parameter of the distribution, or
#'
#' _`surv`_, _`t`_ and _`shape`_  for the proportion surviving (no events) at time t and shape, or
#'
#' _`fail`_ and _`t`_ and _`shape`_ for the proportion failing (events) at time t and shape.
#'
#' scale = -log(surv)·shape/(exp(shape·t))
#'
#' scale = -log(1-fail)·shape/(exp(shape·t))
#'
#' The parameters should be spell correctly as partial matching is not available
#'
#' @param ... Parameters to define the distribution. See the Parameters for details
#' @return a SURVIVAL object of the Gompertz distribution family. See the
#' documentation of `s_factory` for the methods available for SURVIVAL objects
#' @export
#' @importFrom stats runif
#' @examples
#' s_gompertz(scale = 1, shape = 1.5)
#' s_gompertz(surv = 0.4, t = 2, shape = 1.5)
#' s_gompertz(fail = 0.6, t = 2, shape = 1.5)

s_gompertz <- function(...) {
  params <- list(...)
  nparam <- names(params)

  # This function is the factory of the class
  .factory_gompertz <- function(scale, shape) {
    iCum_Hfx <- function(H){
      stopifnot("Must be positive number" = all(H >= 0))
      1/shape*log(shape/scale*H+1)
      #log(shape/scale*H+1)/shape
    }
    structure(
      list(
        distribution = "GOMPERTZ",
        params = list(scale = scale, shape = shape),
        sfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          exp(scale/shape*(1-exp(shape*t)))
          #exp(-scale/shape*(exp(shape*t)-1))
          },
        hfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          scale*exp(shape*t)
          #scale*exp(shape*t)
          },
        Cum_Hfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          scale/shape*(exp(shape*t)-1)
#          scale/shape*(exp(shape*t)-1)
        },
        invCum_Hfx=iCum_Hfx,
        rsurv =  function(n){
          stopifnot("Must be positive number" = all(n > 0))
          iCum_Hfx(-log(runif(n)))
        },
        rsurvhr = function(hr){
          stopifnot("Must be positive numbers > 0" = all(hr > 0))
          # Following Bender, Augustin and Blettner 2005
          iCum_Hfx(-log(runif(length(hr)))/hr)
        }
      ),
      class = c("SURVIVAL")
    )
  }

  # Definition based on scale and shape
  if (length(nparam == 2) &
      all(c("scale","shape") %in% nparam)) {
    stopifnot("scale should be a single number" = is_single_number(params$scale))
    stopifnot("scale must be greater than 0" = params$scale > 0)
    stopifnot("shape should be a single number" = is_single_number(params$shape))
    stopifnot("shape can't be 0 " = params$shape != 0)

    return(.factory_gompertz(params$scale, params$shape))
  }

  # Definition based in proportion surviving, time and shape
  if(
    length(nparam == 3) &
    all(c("surv","t","shape") %in% nparam)) {
    stopifnot("surv must be a single number" = is_single_number(params$surv))
    stopifnot("surv must be greater than 0" = params$surv > 0)
    stopifnot("surv must be smaller than 1" = params$surv < 1)
    stopifnot("t must be a single number" = is_single_number(params$t))
    stopifnot("t must be greater than 0" = params$t > 0)
    stopifnot("shape should be a single number" = is_single_number(params$shape))
    stopifnot("shape can't be 0 " = params$shape != 0)
    scale = -log(params$surv)*params$shape/(exp(params$shape*params$t)-1)
    return(.factory_gompertz(scale, params$shape))

  }

  # Definition based on proportion failing and time
  if(
    length(nparam == 3) &
    all(c("fail","t","shape") %in% nparam)) {
    stopifnot("fail must be a single number" = is_single_number(params$fail))
    stopifnot("fail must be greater than 0" = params$fail > 0)
    stopifnot("fail must be lower than 1" = params$fail < 1)
    stopifnot("t must be a single number" = is_single_number(params$t))
    stopifnot("t must be greater than 0" = params$t > 0)
    stopifnot("shape should be a single number" = is_single_number(params$shape))
    stopifnot("shape can't be 0 " = params$shape != 0)
    scale = -log((1-params$fail))*params$shape/(exp(params$shape*params$t)-1)
    return(.factory_gompertz(scale, params$shape))
  }

  cat("\nError defining s_weibull  object \n")
  cat("Valid parameters are: \n")
  cat("scale and shape: for the canonical parameters of the distribution, or\n")
  cat("surv, t and shape: for the surviving proportion (no events) at time t and shape, or\n")
  cat("fail, t and shape: for the failure proportion (events) at time t and shape, or \n")
  stop("Error in parameters")
}


