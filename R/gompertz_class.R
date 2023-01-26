# Gompertz survival objects for simulations
# by JJAV 20220425


#' Parameters for a Gompertz distribution
#'
#' Create an object with parameters for a Gompertz distribution
#'
#' @param fail proportion fail at time t
#' @param surv proportion surviving at time t
#' @param t  time t
#' @param scale scale parameter
#' @param shape shape parameter
#' @export
#' @return  an object
GOMPERTZ_hazard <- function(scale, shape) {
  stopifnot("scale should be a single number" = is.atomic(scale) )
  stopifnot("scale should be a single number" = length(scale) == 1 )
  stopifnot("scale should be a single number" = is.numeric(scale) )
  stopifnot("scale should be a number greater than 0" = scale > 0 )
  stopifnot("shape should be a single number" = is.atomic(shape) )
  stopifnot("shape should be a single number" = length(shape) == 1 )
  stopifnot("shape should be a single number" = is.numeric(shape) )
  stopifnot("shape can't be 0" = shape != 0 )

  structure(
    list(
      distribution = "Gompertz",
      scale = scale,
      shape = shape
    ),
    class = c("SURVIVALPARAM", "PGOMPERTZ")
  )
}

#' @describeIn GOMPERTZ_hazard Survival at time t
#' @export
GOMPERTZ_surviving <- function(surv, t, shape) {
  stopifnot("surv must be greater than 0" = surv > 0)
  stopifnot("surv must be smaller than 1" = surv < 1)
  stopifnot("t must be greater than 0" = t > 0)
  stopifnot("shape can't be 0 " = shape != 0)
  scale = -log(surv)*shape/(exp(shape*t)-1)
  GOMPERTZ_hazard(scale, shape)
}

#' @describeIn GOMPERTZ_hazard Failure at time t
#' @export
GOMPERTZ_failure <- function(fail, t, shape) {
  stopifnot("fail must be greater than 0" = fail > 0)
  stopifnot("fail must be lower than 1" = fail <1)
  stopifnot("t must be greater than 0" = t > 0)
  stopifnot("shape can't be 0 " = shape != 0)
  GOMPERTZ_surviving(1-fail, t, shape)
}



#' @describeIn  SURVIVAL_factory Gompertz distribution
#' @export
#' @importFrom stats runif
SURVIVAL_factory.PGOMPERTZ <- function(PSURVIVAL,...){
  stopifnot("PSURVIVAL should be of class SURVIVALPARAM" = inherits(PSURVIVAL, "SURVIVALPARAM"))
  stopifnot("PSURVIVAL should be of class PGOMPERTZ" = inherits(PSURVIVAL,"PGOMPERTZ"))
  iCum_Hfx <- function(H){
    log(PSURVIVAL$shape/PSURVIVAL$scale*H+1)/PSURVIVAL$shape
  }
    structure(
    list(
      params = PSURVIVAL,
      sfx = function(t) {
        exp(-PSURVIVAL$scale/PSURVIVAL$shape*(exp(PSURVIVAL$shape*t)-1))
      },
      hfx = function(t) {
        PSURVIVAL$scale*exp(PSURVIVAL$shape*t)
      },
      Cum_Hfx = function(t) {
        PSURVIVAL$scale/PSURVIVAL$shape*(exp(PSURVIVAL$shape*t) -1)
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


# myp <- GOMPERTZ_failure(0.70, t = 40, shape = 0.025)
# myS <- SURVIVAL_factory(myp)
# myS
# plot_survival(myS, 40)
# ggplot_random(myS,40,250,100)
