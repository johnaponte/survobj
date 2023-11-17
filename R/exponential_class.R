## Survival objects for simulations
## by JJAV 20220420

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
  stopifnot("lambda should be a single positive number" = is.atomic(lambda) )
  stopifnot("lambda should be a single positive number" = length(lambda) == 1 )
  stopifnot("lambda should be a single positive number" = is.numeric(lambda) )
  stopifnot("lambda should be a single positive number" = lambda > 0 )

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


#' @describeIn SURVIVAL_factory Exponential distribution
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
      rsurvhr = function(hr){
        iCum_Hfx(-log(runif(length(hr)))/hr)
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
#   mutate(timeto = myS$rsurvhr(ifelse(treatment == 0, 1,0.3))) %>%
#   ddply(
#     .(simid),
#     function(x){
#       coxph(Surv(timeto) ~treatment, data = x) %>%
#       broom::tidy() %>% mutate(ve = (1-exp(estimate))*100)
#     })
#
#
#
# df <-
#   expand_grid(simid = 1:100, group = c(rep(0,1000),rep(1,1000)),rep(2,1000)) %>%
#   mutate(group0 = ifelse(group == 0, 1, 0)) %>%
#   mutate(group1 = ifelse(group == 1, 1, 0)) %>%
#   mutate(group2 = ifelse(group == 2, 1, 0)) %>%
#   mutate(hr = case_when(group ==0 ~ 1, group ==1 ~ 0.75, group ==2 ~ 0.50)) %>%
#   mutate(timeto = myS$rsurvhr(hr)) %>%
#   ddply(
#     .(simid),
#     function(x){
#       coxph(Surv(timeto) ~group1 + group2, data = x) %>%
#         broom::tidy() %>% mutate(ve = (1-exp(estimate))*100)
#     })
