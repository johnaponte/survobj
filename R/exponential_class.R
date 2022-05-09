# Survival objects for simulations
# by JJAV 20220420


#' Parameters for a exponential distribution
#'
#' Create an object with parameters for a survival exponential distribution
#'
#' @param fail proportion fail at time t
#' @param surv proportion surviving at time t
#' @param t  time t
#' @param lambda hazard parameter
#' @export
#' @return an object of class SURVIVALPARAM, PEXPONENTIAL
EXPONENTIAL_hazard <- function(lambda) {
structure(
  list(
    distribution = "Exponential",
    lambda = lambda
  ),
  class = c("SURVIVALPARAM", "PEXPONENTIAL")
)
}

#' @describeIn EXPONENTIAL_hazard Survival proportion at time t
#' @export
EXPONENTIAL_surviving <- function(surv, t) {
    stopifnot("surv must be greater than 0" = surv > 0)
    stopifnot("surv must be smaller than 1" = surv < 1)
    stopifnot("t must be greater than 0" = t > 0)
    lambda = -log(surv) / t
    EXPONENTIAL_hazard(lambda)
}

#' @describeIn EXPONENTIAL_hazard Failure proportion at time t
#' @export
EXPONENTIAL_failure <- function(fail, t) {
  stopifnot("fail must be greater than 0" = fail > 0)
  stopifnot("fail must be lower than 1" = fail <1)
  stopifnot("t must be greater than 0" = t > 0)
  EXPONENTIAL_surviving(1 - fail, t)
}


#' @describeIn  in  SURVIVAL_factory Exponential distribution
#' @export
#' @importFrom stats runif
SURVIVAL_factory.PEXPONENTIAL <- function(PSURVIVAL,...){
  stopifnot("PSURVIVAL should be of class SURVIVALPARAM" = inherits(PSURVIVAL, "SURVIVALPARAM"))
  stopifnot("PSURVIVAL should be of class PEXPONENTIAL" = inherits(PSURVIVAL,"PEXPONENTIAL"))
  iCum_Hfx <- function(H){H/PSURVIVAL$lambda}

  structure(
    list(
      params = PSURVIVAL,
      sfx = function(t) {
        exp(-PSURVIVAL$lambda*t)
      },
      hfx = function(t) {
        rep(PSURVIVAL$lambda,length(t))
      },
      Cum_Hfx = function(t) {
        PSURVIVAL$lambda*t
      },
      invCum_Hfx=iCum_Hfx,
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


# myp <- EXPONENTIAL_failure(0.70, t = 40)
# myS <- SURVIVAL_factory(myp)
# myS
# plot_survival(myS, 40)
# ggplot_random(myS,40,250,100)
#
# df <-
#   expand_grid(simid = 1:100, treatment = c(rep(0,1000),rep(1,1000))) %>%
#   group_by(simid) %>%
#   mutate(timeto = myS$rsurvdf(tibble(treatment), "treatment", log(1-0.5))) %>%
#   ddply(
#     .(simid),
#     function(x){
#       coxph(Surv(timeto) ~treatment, data = x) %>%
#       broom::tidy() %>% mutate(ve = (1-exp(estimate))*100)
#     })
