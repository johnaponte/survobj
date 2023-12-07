## Log Normal survival objects for simulations
# by JJAV 20221204

#' Factory of SURVIVAL objects with Log Normal distributions
#'
#' Creates a SURVIVAL object with a Log Normal distribution.
#'
#' @section Parameters:
#'
#' To create an exponential survival object the following
#' options are available:
#'
#' _`scale`_ and _`shape`_ to specify the canonical parameters of the distribution, or
#'
#' _`surv`_, _`t`_ and _`shape`_ for the proportion surviving (no events) at time t and the shape parameter, or
#'
#' _`fail`_, _`t`_ and _`shape`_ for the proportion failing (events) at time t and the shape parameter or
#'
#' _`intercept`_ and _`shape`_ for the parameters returned by `survreg(.., dist = "lognormal")` models.
#'
#' The scale parameter is the median value of the distribution, and the shape is the log standard deviation

#'
#' The parameters should be spell correctly as partial matching is not available
#'
#' @param ... Parameters to define the distribution. See the Parameters for details
#' @return a SURVIVAL object of the log-normal distribution family. See the
#' documentation of `s_factory` for the methods available for SURVIVAL objects
#' @export
#' @importFrom stats runif
#' @importFrom stats dlnorm
#' @importFrom stats plnorm
#' @importFrom stats qlnorm
#' @importFrom stats qnorm
#' @examples
#' s_lognormal(scale = 2,shape = 2)
#' s_lognormal(surv = 0.6, t= 12, shape = 0.5)
#' s_lognormal(fail = 0.4, t = 12, shape =0.5)
#' s_lognormal(intercept = 0.4, scale = 0.5)
s_lognormal <- function(...) {
  params <- list(...)
  nparam <- names(params)
  # We use the parametrization of scale instead u = log(scale)
  # This function is the factory of the class
  .factory_lognormal <- function(scale, shape) {
    iCum_Hfx <- function(H){
      stopifnot("Must be positive number" = all(H >= 0))
      qlnorm(1 - exp(-H), log(scale), shape)
      }
    structure(
      list(
        distribution = "LOGNORMAL",
        params = list(scale = scale, shape = shape),
        sfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          1-plnorm(t, log(scale), shape)
        },
        hfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          ifelse(t == 0,
                 NA_real_,
                 dlnorm(t,log(scale),shape) / (1-plnorm(t,log(scale),shape))
          )
        },
        Cum_Hfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          -log(1-plnorm(t, log(scale), shape))
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
    stopifnot("scale must be greater than 0 " = params$scale > 0)
    stopifnot("shape should be a single number" = is_single_number(params$shape))
    stopifnot("shape must be greater than 0 " = params$shape > 0)

    return(.factory_lognormal(params$scale, params$shape))
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
    stopifnot("shape must be greater than 0 " = params$shape > 0)

    scale <- exp(log(params$t) - qnorm(1 - params$surv) * params$shape)
    return(.factory_lognormal(scale, params$shape))

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
    stopifnot("shape must be greater than 0 " = params$shape > 0)
    surv = 1- params$fail
    scale <- exp(log(params$t) - qnorm(1 - surv) * params$shape)
    return(.factory_lognormal(scale, params$shape))
  }

  if(
    length(nparam == 2) &
    all(c("intercept","scale") %in% nparam)) {
    stopifnot("intercept must be a single number" = is_single_number(params$intercept))
    stopifnot("scale must be a single number" = is_single_number(params$scale))
    pscale = exp(params$intercept)
    pshape = params$scale
    return(.factory_lognormal(pscale, pshape))
  }

  message(
    "Valid parameters to define a lognormal distribution are: \n",
    "scale and shape: for the canonical parameters of the distribution, or\n",
    "surv, t and shape: for the surviving proportion (no events) at time t and shape, or\n",
    "fail, t and shape: for the failure proportion (events) at time t and shape, or\n",
    "intercept and scale: for values from a survreg regression\n")
  stop("Not valid parameters")
}
