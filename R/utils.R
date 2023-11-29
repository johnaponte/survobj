# Util files for the package
# by JJAV 20220425
############################



#' Confirm is a single number
#'
#' Evaluates if the argument is a single number
#' @param x a variable to evaluate
#' @return `TRUE` if is a single number, `FALSE` otherwise
#' @export
#' @examples
#' is_single_number(3)  #TRUE
#' is_single_number(c(3,3,3)) #FALSE
#' is_single_number(list(a=3)) #FALSE
#' is_single_number("3") #FALSE
is_single_number<- function(x){
  is.atomic(x) &
    length(x)==1 &
    is.numeric(x)
}

#' Functions to help in time conversion
#'
#' This set of functions help in the time conversion, taking into account
#' generic times and not specific times. The conversions are based on the
#' assumption that 1 year is 365.25 days and is 12 months. There is no
#' adjustment for lap days or ours or difference of days between months
#'
#' @param x the time to convert
#' @return the converted time
#' @name fx_h_time
#' @examples
#' dtom(365.25)
#' mtod(12)
#' dtoy(165.25)
#' ytod(1)
#' mtoy(12)
#' ytom(365.25)
NULL

#' @describeIn fx_h_time convert days to months
#' @export
dtom <- function(x){x*12/365.25}

#' @describeIn fx_h_time convert months to days
#' @export
mtod <- function(x){x*365.25/12}

#' @describeIn fx_h_time convert days to years
#' @export
dtoy <- function(x){x/365.25}

#' @describeIn fx_h_time convert years to days
#' @export
ytod <- function(x){x*365.25}

#' @describeIn fx_h_time convert months to year
#' @export
mtoy <- function(x){dtoy(mtod(x))}

#' @describeIn fx_h_time convert years to months
#' @export
ytom <- function(x){dtom(ytod(x))}

#' Censor of events
#'
#'  if censor_time < time, event is change to 0, otherwise not changed
#'
#'  if censor_time < time, time is changed to censor_time, otherwise no change
#'
#'  Be careful and do not overwrite the time with censor time to not loose track
#'  of the events
#'
#' @param censor_time the time to censor
#' @param time the time variable to censor
#' @param event if there is an event at time
#' @return censored time or event
#' @export
#' @examplesIf {FALSE}
#'
#' # Typical workflow on a simulation of survival time.
#' # Simulate time to event (sim_t_event)
#' # and simulates the time to lost to follow up (tim_t_ltof)
#' # the simulation time frame is 1, so everything after 1 is censored
#'
#' require(dplyr)
#' data.frame(
#'   sim_t_event = c(0.5,0.6,1,10,20),
#'   sim_t_ltof = c(2,0.5,2,2,0.8)
#'  ) |>
#'  mutate(sevent = censor_event(1,sim_t_event,sim_event=1)) |>
#'  mutate(stime = censor_time(1,sim_t_event)) |>
#'  mutate(event = censor_event(sim_t_ltof, stime, sevent)) |>
#'  mutate(timeto = censor_time(sim_t_ltof, stime))
censor_event <- function(censor_time, time, event){
  ifelse(censor_time < time, 0, event)
}

#' @describeIn censor_event Censor time
#' @export
censor_time <- function(censor_time, time) {
  ifelse(censor_time < time, censor_time, time)
}



