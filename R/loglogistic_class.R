## Log Logisitic survival objects for simulations
# by JJAV 20221204

#' Factory of SURVIVAL objects with Log Logistic distributions
#'
#' Creates a SURVIVAL object with a Log Logistic distribution.
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
#' _`intercept`_ and _`scale`_ for the parameters returned by `survreg(.., dist = "loglogistic")` models.
#'
#' The parameters should be spell correctly as partial matching is not available
#'
#' @param ... Parameters to define the distribution. See the Parameters for details
#' @return a SURVIVAL object of the log-logistic distribution family. See the
#' documentation of `s_factory` for the methods available for SURVIVAL objects
#' @export
#' @importFrom stats runif
#' @examples
#' s_loglogistic(scale = 2,shape = 2)
#' s_loglogistic(surv = 0.6, t= 12, shape = 0.5)
#' s_loglogistic(fail = 0.4, t = 12, shape =0.5)
#' s_loglogistic(intercept = 0.4, scale = 0.5)
s_loglogistic <- function(...) {
  params <- list(...)
  nparam <- names(params)

  # This function is the factory of the class
  .factory_loglogistic <- function(scale, shape) {
    iCum_Hfx <- function(H){
      stopifnot("Must be positive number" = all(H >= 0))
      ((exp(H)-1)^(1/shape))/scale
    }
    structure(
      list(
        distribution = "LOGLOGISTIC",
        params = list(scale = scale, shape = shape),
        sfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          1/(1+(t*scale)^shape)
        },
        hfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          scale*shape*(scale*t)^(shape -1)/(1+(scale*t)^shape)
        },
        Cum_Hfx = function(t) {
          stopifnot("Must be positive number" = all(t >= 0))
          log(1+(t*scale)^shape)
        },
        invCum_Hfx=iCum_Hfx,
        rsurv =  function(n){
          stopifnot("Must be positive number" = all(n > 0))
          iCum_Hfx(-log(runif(n)))
        },
        rsurvhr = function(hr){
          stopifnot("hr must be numeric" = is.numeric(hr))
          stopifnot("Must be positive numbers > 0" = all(hr > 0))
          # Following Bender, Augustin and Blettner 2005
          iCum_Hfx(-log(runif(length(hr)))/hr)
        },
        rsurvaft = function(aft){
          stopifnot("aft must be numeric" = is.numeric(aft))
          stopifnot("aft must be positive numbers > 0" = all(aft > 0))
          iCum_Hfx(-log(runif(length(aft))))/aft
        },
        rsurvah = function(aft,hr){
          stopifnot("aft must be numeric" = is.numeric(aft))
          stopifnot("hr must be numeric" = is.numeric(hr))
          stopifnot("aft and hr must be of the same length" = length(aft)==length(hr) )
          stopifnot("aft must be positive numbers > 0" = all(aft > 0))
          stopifnot("hr must be positive numbers > 0" = all(hr > 0))
          iCum_Hfx(-log(runif(length(aft)))/hr)/aft
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
    stopifnot("shape must be greater than 0 " = params$shape > 0)

    return(.factory_loglogistic(params$scale, params$shape))
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

    scale = ((1 - params$surv) / params$surv)^(1/params$shape) / params$t
    return(.factory_loglogistic(scale, params$shape))

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
    scale = ((1 -surv) / surv)^(1/params$shape) / params$t
    return(.factory_loglogistic(scale, params$shape))
  }

  if(
    length(nparam == 2) &
    all(c("intercept","scale") %in% nparam)) {
    stopifnot("intercept must be a single number" = is_single_number(params$intercept))
    stopifnot("scale must be a single number" = is_single_number(params$scale))
    pscale = 1/exp(params$intercept)
    pshape = 1/params$scale
    return(.factory_loglogistic(pscale, pshape))
  }

  message(
    "Valid parameters to define a loglogistic distribution are: \n",
    "scale and shape: for the canonical parameters of the distribution, or\n",
    "surv, t and shape: for the surviving proportion (no events) at time t and shape, or\n",
    "fail, t and shape: for the failure proportion (events) at time t and shape, or\n",
    "intercept and scale: for values from a survreg regression\n")
  stop("Not valid parameters")
}
