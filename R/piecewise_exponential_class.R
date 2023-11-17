## Piece wise survival
## 20220425 by JJAV
#####################


#' Parameters for a piece wise exponential distribution
#'
#' Create an object with parameters for a survival piece wise exponential distribution
#'
#' @param fail proportion fail at last break point
#' @param surv proportion surviving at last break point
#' @param breaks breaks that indicates a change in hazard
#' @param segments hazard scale valid up to the next break
#' @param hazards hazards values valid up to the next break
#' @export
#' @return an object of class SURVIVALPARAM, PPIECEWISE_EXP
PIECEWISE_hazards <- function(breaks, hazards) {
  stopifnot("breaks should be a vector" = is.atomic(breaks))
  stopifnot("hazards should be a vector" = is.atomic(vector))
  stopifnot("first break must be >0" = breaks[1] > 0)
  stopifnot("length of breaks and hazards must be equal"= length(breaks) == length(hazards))
  stopifnot("breaks must be in ascending order" = breaks[order(breaks)]== breaks)
  if (breaks[length(breaks)]!= Inf){
    breaks = c(breaks,Inf)
    hazards = c(hazards,hazards[length(hazards)])
  }
  structure(
    list(
      distribution = "Piecewise Exponential",
      breaks = breaks,
      hazards = hazards,
      ntimes = length(breaks)
    ),
    class = c("SURVIVALPARAM", "PPIECEWISE_EXP"))
}

#' @describeIn PIECEWISE_hazards Survival at last segment
#' @export
PIECEWISE_surviving <- function(surv, breaks, segments){
  stopifnot("surv must be a single number" = is.atomic(surv))
  stopifnot("surv must be a single number" = length(surv) == 1)
  stopifnot("surv must be numeric" = is.numeric(surv))
  stopifnot("surv must be greater than 0" = surv > 0)
  stopifnot("surv must be lower than 1" = surv < 1)
  ntimes = length(breaks)
  dtime = c(breaks[1],breaks[2:ntimes] - breaks[1:(ntimes-1)])
  unscaledH = sum(segments*dtime)
  scalefactor = -log(surv)/unscaledH
  hazards = segments*scalefactor
  cumH = sum(hazards*dtime)
  stopifnot("Unsucess scale" = abs(exp(-cumH) - surv )< 1e-12)
  PIECEWISE_hazards(breaks,hazards)
}

#' @describeIn PIECEWISE_hazards Failure at last segment
#' @export
PIECEWISE_failure <- function(fail, breaks, segments){
  stopifnot("fail must be greater than 0" = fail > 0)
  stopifnot("fail must be lower than 1" = fail < 1)
  PIECEWISE_surviving(1-fail,breaks,segments)
}


#' @describeIn  SURVIVAL_factory Piece wise exponential distribution
#' @export
#' @importFrom stats runif
#' @importFrom stats uniroot
SURVIVAL_factory.PPIECEWISE_EXP <- function(PSURVIVAL,...){
  stopifnot("PSURVIVAL should be of class SURVIVALPARAM" = inherits(PSURVIVAL, "SURVIVALPARAM"))
  stopifnot("PSURVIVAL should be of class PPIECEWISE_EXP" = inherits(PSURVIVAL,"PPIECEWISE_EXP"))
  # Maximum finit value of the PSURVIVAL
  maxfinit <-  max(PSURVIVAL$breaks[!is.infinite(PSURVIVAL$breaks)])

  # Function to calculate the cumulative hazard
  cumH <- function(t){
    vapply(
      t,
      function(x){
        wh = which(PSURVIVAL$breaks >= x)
        xbreaks <- PSURVIVAL$breaks
        xbreaks[wh[1]]<- x
        if (wh[1] < PSURVIVAL$ntimes) {
          xbreaks[wh[-1]] <- xbreaks[wh[1]]
        }
        xdtime = c(xbreaks[1],xbreaks[2:PSURVIVAL$ntimes] - xbreaks[1:(PSURVIVAL$ntimes-1)])
        sum(PSURVIVAL$hazard*xdtime)
      },
      0.0
    )
  }

  # Function to invert the  cumulative hazard
  iCum_Hfx <- function(H){
    vapply(
      H,
      function(x){
        difwithH <- function(y) {cumH(y)-x}
        uniroot(difwithH, lower = 0,upper = maxfinit, extendInt = "upX")$root
      },
      0.0
    )
  }

  structure(
    list(
      params = PSURVIVAL,
      sfx = function(t) {
        exp(-cumH(t))
      },
      hfx = function(t) {
        vapply(
          t,
          function(x){
            wh = which(PSURVIVAL$breaks >= x)
            PSURVIVAL$hazards[wh[1]]
          },
          0.0
        )
      },
      Cum_Hfx = function(t) {
        cumH(t)
      },
      invCum_Hfx = function(H){
        iCum_Hfx(H)
      },
      rsurv =  function(n){
        iCum_Hfx(-log(runif(n)))
      },
      rsurvhr = function(hr){
        iCum_Hfx(-log(runif(length(hr)))/hr)
      }
    ),
    class = c("SURVIVAL","PIECEWISE_EXP")
  )
}


# myp <- PIECEWISE_failure(0.70,c(10,20,30),c(1,2,3))
# myS <- SURVIVAL_factory(myp)
# myS
# plot_survival(myS, 40)
# ggplot_random(myS,40,250,100)
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
# summary(df$ve)
