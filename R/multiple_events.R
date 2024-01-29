# Multiple episodes
# by JJAV 20240114

#' Generate random recurrent episodes under a renewal Poisson process
#'
#' Generate a random draws from the distribution of recurrent set of survival
#' times under a renewal Poisson process following Leemis (1987)
#'
#' @param SURVIVAL Object of survival class
#' @param prevtime Vector of previous survival times
#' @name renewal
NULL

#' @param hr Vector of hazard ratios
#' @return Vector of survival times
#' @export
#' @examples
#' s_obj <- s_exponential(fail = 0.4, t = 1)
#' hr <- c(1,1,0.5,0.5)
#' time1 <- rsurvhr(s_obj, hr)
#' time2 <- renewhr(s_obj, hr, time1)
#' @describeIn renewal Recurrent episodes under a proportional hazard model
renewhr <- function(SURVIVAL, hr,  prevtime){
  stopifnot("Is not a SURVIVAL object" = inherits(SURVIVAL, "SURVIVAL"))
  stopifnot("prevtime should be numeric" = is.numeric(prevtime))
  stopifnot("hr should be numeric" = is.numeric(hr))
  stopifnot("Length prevtime and hr must be the same" = length(prevtime) == length(hr))
  stopifnot("prevtime must be >=0" = all(prevtime >= 0))
  prevtime + rsurvhr(SURVIVAL, hr)
}

#' @param SURVIVAL Object of survival class
#' @param aft Vector of accelerated failure time ratios
#' @return Vector of survival times
#' @export
#' @examples
#'
#' s_obj2 <- s_exponential(fail = 0.4, t = 1)
#' aft <- c(1,1,0.5,0.5)
#' timea <- rsurvaft(s_obj2, aft)
#' timeb <- renewaft(s_obj2, aft, timea)
#' @describeIn renewal Recurrent episodes under an accelerated failure time model
renewaft <- function(SURVIVAL, prevtime, aft){
  stopifnot("Is not a SURVIVAL object" = inherits(SURVIVAL, "SURVIVAL"))
  stopifnot("prevtime should be numeric" = is.numeric(prevtime))
  stopifnot("aft should be numeric" = is.numeric(aft))
  stopifnot("Length prevtime and aft must be the same" = length(prevtime) == length(aft))
  stopifnot("prevtime must be >=0" = all(prevtime >= 0))
  prevtime + rsurvaft(SURVIVAL, aft)
}



#' Generate random recurrent episodes under a non homogeneous Poisson process
#'
#' Generate a random draws from the distribution of recurrent set of survival
#' times under a a non homogeneous Poisson process following Leemis (1987)
#'
#' @param SURVIVAL Object of survival class
#' @param prevtime Vector of previous survival times
#' @return Vector of survival times
#' @name nhpp
NULL

#' @describeIn nhpp Recurrent episodes under a proportional hazard model
#' @param hr Vector of hazard ratios
#' @examples
#' s_obj <- s_exponential(fail = 0.4, t = 1)
#' hr <- c(1,1,0.5,0.5)
#' time1 <- rsurvhr(s_obj, hr)
#' time2 <- nhpphr(s_obj,hr, time1)
#' @export
nhpphr <- function(SURVIVAL, hr, prevtime){
  stopifnot("Is not a SURVIVAL object" = inherits(SURVIVAL, "SURVIVAL"))
  stopifnot("prevtime should be numeric" = is.numeric(prevtime))
  stopifnot("hr should be numeric" = is.numeric(hr))
  stopifnot("Length prevtime and hr must be the same" = length(prevtime) == length(hr))
  stopifnot("prevtime must be >=0" = all(prevtime >= 0))
  invCum_Hfx(SURVIVAL, Cum_Hfx(SURVIVAL, prevtime)-log(runif(length(prevtime)))/hr)
}

#' @describeIn nhpp Recurrent episodes under an accelerated failure time model
#' @param aft Vector of accelerated failure ratios
#' @examples
#'
#' s_obj <- s_exponential(fail = 0.4, t = 1)
#' aft <- c(1,1,0.5,0.5)
#' timea <- rsurvaft(s_obj, aft)
#' timeb <- nhppaft(s_obj, aft, timea)
#' @export
nhppaft <- function(SURVIVAL, aft, prevtime){
  stopifnot("Is not a SURVIVAL object" = inherits(SURVIVAL, "SURVIVAL"))
  stopifnot("prevtime should be numeric" = is.numeric(prevtime))
  stopifnot("aft should be numeric" = is.numeric(aft))
  stopifnot("Length prevtime and aft must be the same" = length(prevtime) == length(aft))
  stopifnot("prevtime must be >=0" = all(prevtime >= 0))
  invCum_Hfx(SURVIVAL, Cum_Hfx(SURVIVAL,prevtime*aft)-log(runif(length(prevtime))))/aft
}


