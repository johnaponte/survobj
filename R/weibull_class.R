# Weibull survival objects for simulations
# by JJAV 20220425


#' Parameters for a Weibull distribution
#'
#' Create an object with parameters for a Weibull exponential distribution
#' @param fail proportion fail at time t
#' @param surv proportion surviving at time t
#' @param t  time t
#' @param scale scale parameter
#' @param shape shape parameter
#' @param logscale scale parameter from a survreg model
#' @param logshape shape parameter from a survreg model
#' @export
#' @return an object of class SURVIVALPARAM, PWEIBULL
WEIBULL_hazard <- function(scale, shape) {
  stopifnot("scale must be greater than 0" = scale > 0)
  stopifnot("shape must be greater than 0 " = shape > 0)
  structure(
    list(
      distribution = "Weibull",
      scale = scale,
      shape = shape
    ),
    class = c("SURVIVALPARAM", "PWEIBULL")
  )
}

#' @describeIn WEIBULL_hazard Survival at time t
#' @export
WEIBULL_surviving <- function(surv, t, shape) {
  stopifnot("surv must be greater than 0" = surv > 0)
  stopifnot("surv must be smaller than 1" = surv < 1)
  stopifnot("t must be greater than 0" = t > 0)
  stopifnot("shape must be greater than 0 " = shape > 0)
  scale = -log(surv)/(t^shape)
  WEIBULL_hazard(scale, shape)
}

#' @describeIn WEIBULL_hazard Failure at time t
#' @export
WEIBULL_failure <- function(fail, t, shape) {
  stopifnot("fail must be greater than 0" = fail > 0)
  stopifnot("fail must be lower than 1" = fail <1)
  stopifnot("t must be greater than 0" = t > 0)
  stopifnot("shape must be greater than 0 " = shape > 0)
  WEIBULL_surviving(1-fail, t, shape)
}

#' @describeIn WEIBULL_hazard From survreg model
#' @export
WEIBULL_surverg <- function(logscale, logshape ){
  shape = 1/exp(logshape)
  scale = exp(logscale)^-shape
  WEIBULL_hazard(shape, scale)
}


#' @describeIn  SURVIVAL_factory Weibull distribution
#' @export
#' @importFrom stats runif
SURVIVAL_factory.PWEIBULL <- function(PSURVIVAL,...){
  stopifnot("PSURVIVAL should be of class SURVIVALPARAM" = inherits(PSURVIVAL, "SURVIVALPARAM"))
  stopifnot("PSURVIVAL should be of class PWEIBULL" = inherits(PSURVIVAL,"PWEIBULL"))
  iCum_Hfx <- function(H){
    (H/PSURVIVAL$scale)^(1/PSURVIVAL$shape)
  }
    structure(
    list(
      params = PSURVIVAL,
      sfx = function(t) {
        exp(-PSURVIVAL$scale*(t^PSURVIVAL$shape))
      },
      hfx = function(t) {
        PSURVIVAL$scale*PSURVIVAL$shape*(t)^(PSURVIVAL$shape-1)
      },
      Cum_Hfx = function(t) {
        PSURVIVAL$scale*t^PSURVIVAL$shape
      },
      invCum_Hfx=function(H){
        iCum_Hfx(H)
      },
      rsurv =  function(n){
        iCum_Hfx(-log(runif(n)))
      },
      rsurvdf = function(.data,vars, coeffs){
        iCum_Hfx(-log(runif(nrow(.data)))*exp(-linsum(.data,vars,coeffs)))

      }
    ),
    class = c("SURVIVAL","EXPONENTIAL")
  )
}


# myp <- WEIBULL_failure(0.70, t = 40 , shape = 2.4)
# myS <- SURVIVAL_factory(myp)
# myS
# plot_survival(myS, 40)
# ggplot_random(myS,40,250,100)
