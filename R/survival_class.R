## Survival Class
## 20220425 by JJAV
##################

#' Factory of objects of class SURVIVAL
#'
#' Create objects of the class SURVIVAL
#'
#' The objects of the class SURVIVAL define different distributions of
#' survival times. Each class has its own set of parameters but once the
#' SURVIVAL object is defined, they have access to the same functions to
#' calculate:
#'
#'   - survival time function: `sfx()`,
#'
#'   - hazard time function: `hfx()`,
#'
#'   - cumulative hazard function: `Cum_Hfx()`
#'
#'   - the inverse of the cumulative hazard function: `invCum_Hfx()`.
#'
#'   - generate random survival times: `rsurv()`
#'
#'   - generate random survival times under proportional hazard ratio: `rsurvhr()`.
#'
#' There several functions to plot the distributions
#'
#'   - generic S3:  `plot.SURVIVAL()`
#'
#'   - `plot_survival()`: to plot the functions
#'
#'   - `ggplot_survival_random()`: to ggplot random draws from the distribution
#'
#'   - `compare_survival()`:  to compare the functions of two SURVIVAL objects
#'
#' @section Distributions:
#' The current factories are implemented:
#'
#'   - `s_exponential()`: for Exponential distributions
#'
#'   - `s_weibull()`: for Weibull distributions
#'
#'   - `s_gompertz()`: for Gompertz distributions
#'
#'   - `s_picewise()`: for Piecewise exponential distributions
#'
#' @param s_family a factory for a specific distribution
#' @param ... parameters to define the survival distribution
#' @return a SURVIVAL object
#' @export
#' @examples
#' # Define a SURVIVAL object
#' obj <- s_factory(s_exponential, lambda = 2)
#'
#' # Survival, Hazard and Cumulative hazard at time 0.4
#' sfx(SURVIVAL = obj, t= 0.4)
#' hfx(SURVIVAL = obj, t = 0.4)
#' Cum_Hfx(SURVIVAL = obj, t = 0.4)
#'
#' # Time when the Cumulative hazard is 0.8
#' invCum_Hfx(SURVIVAL = obj, H = 0.8)
#'
#' # Draw one random survival time from the distribution
#' rsurv(SURVIVAL = obj, n = 1)
#'
#' # Draw one random survival time from the distribution, with hazard ratio 0.5
#' rsurvhr(SURVIVAL = obj, hr = 0.5)
#'
#' # Plot the survival functions
#' plot(obj)
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


#' Functions for SURVIVAL objects
#'
#' All the SURVIVAL objects have access to the functions described here
#'
#' @param t Time
#' @param SURVIVAL a SURVIVAL object
#' @return Depending on the function a proportion surviving, hazard,
#' cumulative hazard, inverse of the cumulative hazard, a random draw or a plot
#' @examples
#' #' # Define a SURVIVAL object
#' obj <- s_factory(s_weibull, surv = 0.8, t = 2, shape = 1.2)
#'
#' # Survival, Hazard and Cumulative hazard at time 0.4
#' sfx(SURVIVAL = obj, t= 0.4)
#' hfx(SURVIVAL = obj, t = 0.4)
#' Cum_Hfx(SURVIVAL = obj, t = 0.4)
#'
#' # Time when the Cumulative hazard is 0.8
#' invCum_Hfx(SURVIVAL = obj, H = 0.8)
#'
#' # Draw one random survival time from the distribution
#' rsurv(SURVIVAL = obj, n = 1)
#'
#' # Draw one random survival time from the distribution, with hazard ratio 0.5
#' rsurvhr(SURVIVAL = obj, hr = 0.5)
#'
#' # Plot the survival functions
#' plot_survival(SURVIVAL = obj, timeto = 2, main = "Example of Weibull distribution" )
#'
#' @export
#' @describeIn SURVIVAL Survival function
sfx <- function(SURVIVAL, t){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$sfx(t)
}

#' @param t Time
#' @param SURVIVAL a Survival object
#' @export
#' @describeIn SURVIVAL Hazard function
hfx <- function(SURVIVAL, t){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$hfx(t)
}


#' @param t Time
#' @param SURVIVAL a SURVIVAL object
#' @export
#' @describeIn SURVIVAL Cumulative Hazard function
Cum_Hfx <- function(SURVIVAL, t){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$Cum_Hfx(t)
}

#' Inverse of the cumulative hazard
#'
#' @param H cumulative hazard
#' @param SURVIVAL a SURVIVAL object
#' @export
#' @describeIn SURVIVAL Inverse of the Cumulative Hazard function
invCum_Hfx <- function(SURVIVAL, H){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$invCum_Hfx(H)
}

#' @param n number of observations
#' @param SURVIVAL a SURVIVAL object
#' @export
#' @describeIn SURVIVAL Generate random values from the distribution
rsurv <- function(SURVIVAL, n){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$rsurv(n)
}

#' @param hr a vector with hazard rates.
#' @param SURVIVAL a SURVIVAL object
#' @export
#' @describeIn SURVIVAL Generate random values from the distribution under proportional hazard ratios
rsurvhr <- function(SURVIVAL, hr){
  stopifnot("Not a SURVIVAL OBJECT" = inherits(SURVIVAL,"SURVIVAL"))
  SURVIVAL$rsurvhr(hr)
}

#' @param SURVIVAL a SURVIVAL object
#' @param timeto timeto used in the graphs
#' @param main title of the graph
#' @export
#' @importFrom graphics par
#' @importFrom graphics title
#' @describeIn SURVIVAL Plot of the survival functions
plot_survival <- function(SURVIVAL, timeto, main) {
  if (missing(main)) main = NA_character_
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(2,2))
  plot(
    SURVIVAL$sfx,
    from = 0,
    to = timeto,
    main = "Survival function",
    xlab = "Time",
    ylab = "Proportion without events",
    ylim = c(0,1))
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
  if (! is.na(main)) {
    title(main, outer = TRUE, line = -1)
  }
  par(oldpar)
}



#' @export
#' @importFrom stats uniroot
plot.SURVIVAL <- function(x,...){
  params <- list(...)
  if (! "main" %in% names(params)) {
    maint = paste0(x$distribution, " distribution")
  }
  else {
    maint = params$main
  }
  p5 <- uniroot(function(y){ifelse(y < 0, -10,x$sfx(y)-0.05)}, lower = 0, upper = 10, extendInt = "downX")
  if (! inherits(p5,"try-error")){
    plot_survival(x, timeto = p5$root, main = maint)
  }
  else {
    stop("Error finding and adequate time interval. Use the function plot_survival instead")
  }
}


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
#' @importFrom survival survfit
#' @describeIn SURVIVAL ggplot of the simulation of survival times
ggplot_survival_random <- function(SURVIVAL, timeto, subjects, nsim, alpha = 0.1) {

  ldf<- lapply(
    1:nsim,
    function(x){
        simtime <- SURVIVAL$rsurv(subjects)
        simevent <- ifelse(simtime <= timeto,1,0)
        simtime <- ifelse(simevent == 0, timeto, simtime)
        so<- survfit(Surv(simtime, simevent) ~ 1)
        return(
          data.frame(
            simid = x,
            time = so$time,
            survival = so$surv,
            cumhazard = so$cumhaz
          )
        )
    }
  )
  survdf <- do.call(rbind,ldf) |>
    pivot_longer(
      -c(simid,time), names_to = "varname" , values_to = "value"
    ) |>
    mutate(varname = factor(varname, levels = c("survival", "cumhazard"),
                              labels = c("Survival","Cumulative Hazard")))

  expsurv <-
    survdf |>
    select(time) |>
    unique() |>
    mutate(survival = SURVIVAL$sfx(time)) |>
    mutate(cumhazard = SURVIVAL$Cum_Hfx(time)) |>
    mutate(simid = 0) |>
    pivot_longer(
      -c(simid,time), names_to = "varname" , values_to = "value"
    )  |>
    mutate(varname = factor(varname, levels = c("survival", "cumhazard"),
                               labels = c("Survival","Cumulative Hazard")))


  survdf |>
    ggplot() +
    aes(x = time, y = value, group = simid) +
    geom_step(alpha = alpha) +
    geom_step(data = expsurv, color = "red") +
    facet_wrap(~varname, scales = "free_y") +
    scale_y_continuous("") +
    ggtitle(
      paste(SURVIVAL$distribution, "simulations"),
      subtitle = paste0("Subjects: ", subjects, "  Number of simulations: ", nsim))
}

#' @param SURVIVAL1 a SURVIVAL object
#' @param SURVIVAL2 a SURVIVAL object
#' @param timeto timeto used in the graphs
#' @param main title of the graph
#' @export
#' @importFrom graphics par
#' @importFrom graphics title
#' @describeIn SURVIVAL Compare graphically two survival distributions
compare_survival <- function(SURVIVAL1, SURVIVAL2, timeto, main) {
  if (missing(main)) main = NA_character_
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  col1 = 2
  col2 = 3
  par(mfrow=c(2,3))

  plot(
    SURVIVAL1$sfx,
    from = 0,
    to = timeto,
    main = "Survival function",
    xlab = "Time",
    ylab = "Proportion without events",
    ylim = c(0,1),
    col = col1)
  plot(
    SURVIVAL2$sfx,
    from = 0,
    to = timeto,
    col = col2,
    add = TRUE
  )

  #min for scale
  yvals <- c(SURVIVAL1$hfx(0:timeto), SURVIVAL2$hfx(c(0,timeto)))
  yvals<- yvals[is.finite(yvals)]

  plot(
    SURVIVAL1$hfx,
    from = 0,
    to = timeto,
    main = "Hazard function",
    xlab = "Time",
    ylab = "Hazard",
    ylim = c(min(yvals,na.rm = T),
             max(yvals,na.rm = T)),
    col = col1)
  plot(
    SURVIVAL2$hfx,
    from = 0,
    to = timeto,
    col = col2,
    add = TRUE
  )

  plot(
    SURVIVAL1$Cum_Hfx,
    from = 0,
    to = timeto,
    main = "Cumulative Hazard",
    xlab = "Time",
    ylab = "Cumulative Hazard",
    col = col1)
  plot(
    SURVIVAL2$Cum_Hfx,
    from = 0,
    to = timeto,
    col = col2,
    add = TRUE)

  plot(
    function(x){SURVIVAL2$hfx(x) / SURVIVAL1$hfx(x)},
    from = 0,
    to = timeto,
    main = "Hazard Ratio",
    xlab = "Time",
    ylab = "Ratio"
  )

  plot(
    function(x){SURVIVAL2$Cum_Hfx(x) / SURVIVAL1$Cum_Hfx(x)},
    from = 0,
    to = timeto,
    main = "Ratio of Cumulative hazard",
    xlab = "Time",
    ylab = "Ratio"
  )

  plot(
    function(x){SURVIVAL2$Cum_Hfx(x) - SURVIVAL1$Cum_Hfx(x)},
    from = 0,
    to = timeto,
    main = "Difference in Cumulative hazard",
    xlab = "Time",
    ylab = "Difference"
  )

  if (! is.na(main)) {
    title(main, outer = TRUE, line = -1)
  }
  par(oldpar)
}


