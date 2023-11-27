## Survival Class
## 20220425 by JJAV
##################

#'SURVIVAL class object's factory generic
#'
#' Creates a SURVIVAL object using the information from the SURVIVALPARAM parameter
#' This is implemented by each subclass of the SURVIVALPARAM, i.e.
#'PEXPONENTIAL, PWEIBULL, PGOMPERTZ PPIECEWISE.
#'
#' @param s_family an object of class S_FAMILY
#' @param ... parameters to define the survival distribution
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
s_factory <- function(s_family, ...){
    s_family(...)
}

#' @export
print.SURVIVAL <- function(x, ...){
  cat("SURVIVAL object\n")
  namesx <- names(x$params)
  cat("Distribution: ", x$distribution,"\n")
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
sfx <- function(SURVIVAL, t){
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
hfx <- function(SURVIVAL, t){
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
Cum_Hfx <- function(SURVIVAL, t){
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
invCum_Hfx <- function(SURVIVAL, H){
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
rsurv <- function(SURVIVAL, n){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$rsurv(n)
}

#' Random survival times for a hazard rate
#'
#' Return random survival times following the distribution defined in the SURVIVAL
#' object, and assuming a proportionality of the hazards, the log(time) is
#' modified by the hazard ratio. A hazard ratio of 1 means not change to the
#' baseline hazard of the distribution. A hazard ratio lower than 1 means longer
#' times and hazard ratios higher than 1 means shorter times.
#' frame
#'
#' @param hr a vector with hazard rates.
#' @param SURVIVAL a SURVIVAL object
#' @return random survival times
#' @export
rsurvdf <- function(SURVIVAL, hr){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$rsurvhr(hr)
}

#' Plot a SURVIVAL OBJECT
#'
#' Plot of the functions in the survival object until timeto t
#'
#' @param SURVIVAL a SURVIVAL object
#' @param timeto timeto used in the graphs
#' @export
#' @importFrom graphics par
plot_survival <- function(SURVIVAL, timeto, main = "SURVIVAL object") {
  oldpar <- par(no.readonly = TRUE)
  par(mfrow=c(2,2))
  plot(
    SURVIVAL$sfx,
    from = 0,
    to = timeto,
    main = "Survival function",
    xlab = "Time",
    ylab = "Proportion without events")
  plot(
    SURVIVAL$hfx,
    from = 0,
    to = timeto,
    main = "Hazard function",
    xlab = "Time",
    ylab = "Hazard")
  plot(
    SURVIVAL$Cum_Hfx,
    from = 0,
    to = timeto,
    main = "Cumulative Hazard",
    xlab = "Time",
    ylab = "Cumulative Hazard")
  plot(
    SURVIVAL$invCum_Hfx,
    from = 0,
    to = SURVIVAL$Cum_Hfx(timeto),
    main = "Inverse Cumulative Hazard",
    xlab = "Cumulative Hazard",
    ylab = "Time")
  title(main, outer = TRUE, line = -1)
  par(oldpar)
}

#' @export
plot.SURVIVAL <- function(x, main = "SURVIVAL object"){
  p5 <- try(uniroot(function(y){x$sfx(y)-0.05}, interval=c(0,1000)),silent = T)
  if (! inherits(p5,"try-error")){
    plot_survival(x, timeto = p5$root, main = main)
  }
  else {
    stop("Error finding and adequate time interval. Use the function plot_survival instead")
  }
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

