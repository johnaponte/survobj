## Survival Class
## 20220425 by JJAV
##################

#'SURVIVAL class object's factory generic
#'
#' Creates a SURVIVAL object using the information from the SURVIVALPARAM parameter
#' This is implemented by each subclass of the SURVIVALPARAM, i.e.
#'PEXPONENTIAL, PWEIBULL, PGOMPERTZ PPIECEWISE.
#'
#' @param PSURVIVAL an object of class SURVIVALPARAM
#' @param ... Currently not used
#' @return a SURVIVAL object
#' @export
#' @note
#' The created object has the following methods (functions)
#'
#' sfx(t): S function that returns the survival proportion at a given time
#'
#' hfx(t): A function that returns the hazard at a given time
#'
#' Cum_Hfx(t): A function that returns the cumulative hazard at a given time
#'
#' invHFx(H): A function the returns the time given a cumulative hazard value
#'
#' rsurv(x): A function that returns random times from the survival distribution
#'
#' rsurvhr(hr): A function that returns random times from the survival distribution
#' but following a proportional hazard assumption. For example a rsurvhr(c(1,0.5))
#' will produce a random survival time in which the first observation has a hazard
#' ratio of 1 and the second a hazard ratio of 0.5.
SURVIVAL_factory <- function(PSURVIVAL, ...){
  UseMethod("SURVIVAL_factory", PSURVIVAL)
}


#' @describeIn SURVIVAL_factory Default implementation
#' @aliases SURVIVAL_factory
#' @export
SURVIVAL_factory.default <- function(PSURVIVAL, ...) {
  stopifnot("params should of class SURVIVALPARAM" = inherits(PSURVIVAL, "SURVIVALPARAM"))
  structure(
    list(
      .params = PSURVIVAL,
      sfx = function(t) {
        stop("sfx not implementet")
      },
      hfx = function(t) {
        stop("hfx not implemented")
      },
      Cum_Hfx = function(t) {
        stop("hfx not implemented")
      },
      invCum_Hfx = function(H) {
        stop("invCum_Hfx not implemented")
      },
      rsurv =  function(n){
        stop("rsurv not implemented")
        #invCum_Hfx(-log(runif(n)))
      },
      rsurvdf = function(.data,vars, coeffs){
        stop("rsurvdf not implemented")
        #invCum_Hfx(-log(runif(nrow(.data)))*exp(-linsum(df,vars,coeffs)))
      }

    ),
    class = "SURVIVAL"
  )
}



#' @export
print.SURVIVALPARAM<- function(x, ...){
  cat("Parameters for survival distribution\n")
  namesx <- names(x)
  lapply(namesx, function(y){cat(y,":",x[[y]],"\n")})
}

#' @export
print.SURVIVAL <- function(x, ...){
  cat("Survival object\n")
  namesx <- names(x$params)
  lapply(namesx, function(y){cat(y,":",x$params[[y]],"\n")})
}


#' Survival proportion
#'
#' Survival proportion at time t using the method from the SURVIVAL object
#'
#' @param t Time
#' @param SURVIVAL a SURVIVAL object
#' @return Proportion surviving at time t
#' @export
sfx <- function(t, SURVIVAL){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$sfx(t)
}

#' Hazard
#'
#' Hazard a time t using the hazard method from the SURVIVAL object
#'
#' @param t Time
#' @param SURVIVAL a Survival object
#' @return the hazard at time t
#' @export
hfx <- function(t, SURVIVAL){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$hfx(t)
}


#' Cumulative hazard
#'
#' Cumulative hazard at time t using the cumulative hazard method
#' from the SURVIVAL object
#'
#' @param t Time
#' @param SURVIVAL a SURVIVAL object
#' @return a cumulative hazard at time t
#' @export
Cum_Hfx <- function(t, SURVIVAL){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$Cum_Hfx(t)
}

#' Inverse of the cumulative hazard
#'
#' Return the time t for a give cumulative hazard using the method
#' from the SURVIVAL object
#' @param H cumulative hazard
#' @param SURVIVAL a SURVIVAL object
#' @export
invCum_Hfx <- function(H, SURVIVAL){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$invCum_Hfx(H)
}

#' Random survival times
#'
#' Return random survival times following the distribution defined in the SURVIVAL
#' object
#'
#' @param n number of observations
#' @param SURVIVAL a SURVIVAL object
#' @return random survival times
#' @export
rsurv <- function(n, SURVIVAL){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$rsurv(n)
}

#' Random survival times for a data frame
#'
#' Return random survival times following the distribution defined in the SURVIVAL
#' object, and assuming a proportionality of the hazards, the log(time) is
#' modified by the linear combination of the vars and coeffs values in the data
#' frame
#'
#' @param .data a data.frame
#' @param vars a character vector with names of variables in the data.frame
#' @param coeffs a numeric vector with the value of the coefficient to multiply
#' to the corresponding variable
#' @param SURVIVAL a SURVIVAL object
#' @return random survival times
#' @export
rsurvdf <- function(.data, vars, coeffs, SURVIVAL){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$rsuvdf(.data,vars,coeffs)
}

#' Plot a SURVIVAL OBJECT
#'
#' Plot of the functions in the survival object until timeto t
#'
#' @param SURVIVAL a SURVIVAL object
#' @param timeto timeto used in the graphs
#' @export
#' @importFrom graphics par
plot_survival <- function(SURVIVAL, timeto) {
  oldpar <- par(no.readonly = TRUE)
  par(mfrow=c(2,2))
  plot(SURVIVAL$sfx, from = 0, to = timeto, main = "Survival function", xlab = "Time", ylab = "Proportion without events")
  plot(SURVIVAL$hfx, from = 0, to = timeto, main = "Hazard function", xlab = "Time", ylab = "Hazard")
  plot(SURVIVAL$Cum_Hfx, from = 0, to = timeto, main = "Cumulative Hazard", xlab = "Time", ylab = "Cumulative Hazard")
  plot(SURVIVAL$invCum_Hfx, from = 0, to = SURVIVAL$Cum_Hfx(timeto), main = "Inverse Cumulative Hazard", xlab = "Cumulative Hazard", ylab = "Time")
  par(oldpar)
}


#' Plot random draws from the distribution
#'
#' a ggPlot with random survival times using the random generator in the
#' SURVIVAL object
#'
#' @param SURVIVAL a SURVIVAL object
#' @param timeto plot the distribution up to timeto
#' @param subjects number of subjects to simulate in each simulation
#' @param nsim number of simulations
#' @param alpha alpha value for the graph
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 ggtitle
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom plyr ddply
#' @importFrom plyr .
#' @importFrom survival survfit
ggplot_random <- function(SURVIVAL, timeto, subjects, nsim, alpha = 0.1) {
  df<- data.frame(simid = 1:nsim) %>%
    ddply(
      .(simid),
      function(x){
        data.frame(simtime = SURVIVAL$rsurv(subjects)) %>%
        mutate(simevent = ifelse(simtime <= timeto,1,0)) %>%
        mutate(simtime = ifelse(simevent == 0, timeto, simtime))
    })

  survdf <-
    df %>%
    ddply(
      .(simid),
      function(x){
        so <- survfit(Surv(simtime,simevent)~1, data = x)
        data.frame(time = so$time, survival = so$surv, cumhazard = so$cumhaz)
      }
    ) %>%
    pivot_longer(
      -c(simid,time), names_to = "varname" , values_to = "value"
    ) %>% mutate(varname = factor(varname, levels = c("survival", "cumhazard"),
                              labels = c("Survival","Cumulative Hazard")))

  expsurv <-
    survdf %>%
    select(time) %>%
    unique() %>%
    mutate(survival = SURVIVAL$sfx(time)) %>%
    mutate(cumhazard = SURVIVAL$Cum_Hfx(time)) %>%
    mutate(simid = 0) %>%
    pivot_longer(
      -c(simid,time), names_to = "varname" , values_to = "value"
    )  %>% mutate(varname = factor(varname, levels = c("survival", "cumhazard"),
                               labels = c("Survival","Cumulative Hazard")))


  survdf %>%
    ggplot() +
    aes(x = time, y = value, group = simid) +
    geom_step(alpha = alpha) +
    geom_step(data = expsurv, color = "red") +
    facet_wrap(~varname, scales = "free_y") +
    scale_y_continuous("") +
    ggtitle(
      "Simulations from a SURVIVAL object",
      subtitle = paste0("Subjects: ", subjects, "  Number of simulations: ", nsim))
}

