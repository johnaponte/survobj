## Piecewise survival
## 20221129 by JJAV
#####################


#' Factory of SURVIVAL objects with Piecewise Exponential distributions
#'
#' Creates a SURVIVAL object with an Piecewise Exponential distribution.
#'
#' @section Parameters:
#'
#' To create an piecewise exponential survival object the following
#' options are available:
#'
#' _`breaks`_ and _`hazards`_ to specify the exponential (constant) hazard until each break, or
#'
#' _`surv`_, _`breaks`_ and _`segments`_ for the proportion surviving (no events) at the end of last segment or
#'
#' _`fail`_, _`breaks`_ and _`segments`_ for the proportion failing (events) at the end of last segment
#'
#' If _`surv`_ or _`fail`_ parameters are indicated, the _`segments`_ are scaled to hazards in order
#' to mach the surviving or failing proportion at the end of the last segment.
#'
#' Define the last break point as `Inf` to fully define the distribution, otherwise
#' an error will be produce if function after the last break is requested
#'
#' The parameters should be spell correctly as partial matching is not available
#'
#'
#' @param ... Parameters to define the distribution. See the Parameters for details
#' @return a SURVIVAL object of the piecewise exponential distribution family. See the
#' documentation of `s_factory` for the methods available for SURVIVAL objects
#' @importFrom stats runif
#' @importFrom stats uniroot
#' @export
#' @examples
#' s_piecewise(breaks = c(1,2,3,Inf), hazards = c(0.5,0.6,0.5,0.1))
#' s_piecewise(surv = 0.4, breaks = c(1,2,3,Inf), segments = c(1,2,1,2))
#' s_piecewise(fail = 0.6, breaks = c(1,2,3,Inf), segments = c(1,2,1,2))
s_piecewise <- function(...) {
  params <- list(...)
  nparam <- names(params)

  # This function is the factory of the class
  .factory_piecewise <- function(breaks, hazards) {
    maxfinitb <- which(breaks == max(breaks[is.finite(breaks)]))
    dtimes <- c(breaks[1],breaks[2:maxfinitb]-breaks[1:(maxfinitb-1)])
    chazards <- c(0,dtimes*hazards[1:maxfinitb])
    cumhazards<-c(cumsum(chazards))
    tbreaks <- c(0, breaks[1:maxfinitb])

    if (is.finite(breaks[length(breaks)])) {
      warning("Last break is not Inf. The distribution is not fully defined")
    }

    # Cumulative Hazard function
    cumH <- function(t){
      stopifnot("t must be numeric" = is.numeric(t))
      stopifnot("t must be positive number" = all(t >= 0))
      vapply(
        t,
        function(x){
          wh <- max(which(tbreaks <= x))
          cumhazards[wh]+hazards[wh]*(x-tbreaks[wh])
        },
        0.0
      )
    }


    # Function to invert analytically the cumulative hazard
    iCum_Hfx <- function(H){
      stopifnot("Must be positive number" = all(H >= 0))
      vapply(
        H,
        function(x){
          wh <- max(which(cumhazards <= x))
          (x-cumhazards[wh])/hazards[wh] + tbreaks[wh]
        },
        0.0
      )
    }

    structure(
      list(
        distribution = "PIECEWISE",
        params = list(breaks = breaks, hazards = hazards),
        sfx = function(t) {
          stopifnot("t must be numeric" = is.numeric(t))
          stopifnot("t must be positive number" = all(t >= 0))
          exp(-cumH(t))
        },
        hfx = function(t) {
          stopifnot("t must be numeric" = is.numeric(t))
          stopifnot("t must be positive number" = all(t >= 0))
          vapply(
            t,
            function(x){
              wh = which(breaks >= x)
              hazards[wh[1]]
            },
            0.0
          )
        },
        Cum_Hfx = cumH,
        invCum_Hfx=iCum_Hfx,
        rsurv =  function(n){
          stopifnot("n must be numeric" = is.numeric(n))
          stopifnot("n must be positive number" = all(n > 0))
          iCum_Hfx(-log(runif(n)))
        },
        rsurvhr = function(hr){
          stopifnot("hr must be numeric" = is.numeric(hr))
          stopifnot("hr must be positive numbers > 0" = all(hr > 0))
          # Following Bender, Augustin and Blettner 2005
          iCum_Hfx(-log(runif(length(hr)))/hr)
        },
        rsurvaft = function(aft){
          stopifnot("aft must be numeric" = is.numeric(aft))
          stopifnot("aft must be positive numbers > 0" = all(aft > 0))
          iCum_Hfx(-log(runif(length(aft))))*aft
        },
        rsurvah = function(aft,hr){
          stopifnot("aft must be numeric" = is.numeric(aft))
          stopifnot("hr must be numeric" = is.numeric(hr))
          stopifnot("aft and hr must be of the same length" = length(aft)==length(hr) )
          stopifnot("aft must be positive numbers > 0" = all(aft > 0))
          stopifnot("hr must be positive numbers > 0" = all(hr > 0))
          iCum_Hfx(-log(runif(length(aft)))/hr)*aft
        }
      ),
      class = c("SURVIVAL")
    )
  }

  # Definition based on lambda
  if (length(nparam == 2) & all(c("breaks","hazards") %in% nparam)) {
      stopifnot("breaks must be numeric" = is.numeric(params$breaks))
      stopifnot("breaks and hazards must be of same length" = length(params$breaks) == length(params$hazards))
      stopifnot("breaks must > 0" = all(params$breaks > 0))
      stopifnot("breaks must be increasing"  = all(params$breaks == cummax(params$breaks)))
      stopifnot("breaks must not be duplicated" = ! any(duplicated(params$breaks)))
      stopifnot("hazards must be numeric" = is.numeric(params$breaks))
      stopifnot("hazards must be >= 0" = all(params$hazards >= 0))
      return(.factory_piecewise(params$breaks, params$hazards))
  }

  # Definition based in proportion surviving and time
  if(
    length(nparam == 3) &
    all(c("surv","breaks","segments") %in% nparam)) {
    stopifnot("surv must be a single number" = is_single_number(params$surv))
    stopifnot("surv must be greater than 0" = params$surv > 0)
    stopifnot("surv must be smaller than 1" = params$surv < 1)
    stopifnot("breaks must be numeric" = is.numeric(params$breaks))
    stopifnot("breaks and segments must be of same length" = length(params$breaks) == length(params$segments))
    stopifnot("breaks must > 0" = all(params$breaks > 0))
    stopifnot("breaks must be increasing"  = all(params$breaks == cummax(params$breaks)))
    stopifnot("breaks must not be duplicated" = ! any(duplicated(params$breaks)))
    stopifnot("segments must be numeric" = is.numeric(params$segments))
    stopifnot("segments must be >= 0" = all(params$segments >= 0))
    wfin <- which(is.finite(params$breaks))
    ntimes = length(params$breaks[wfin])
    dtime = c(params$breaks[1],params$breaks[2:ntimes] - params$breaks[1:(ntimes-1)])
    unscaledH = sum(params$segments[wfin]*dtime)
    scalefactor = -log(params$surv)/unscaledH
    hazards = params$segments*scalefactor
    cumH = sum(hazards[wfin]*dtime)
    stopifnot("Unsucess scale" = abs(exp(-cumH) - params$surv ) < 1e-12)
    return(.factory_piecewise(params$breaks, hazards))
  }

  # Definition based on proportion failing and time
  if(
    length(nparam == 3) &
    all(c("fail","breaks","segments") %in% nparam)) {
    stopifnot("fail must be a single number" = is_single_number(params$fail))
    stopifnot("fail must be greater than 0" = params$fail > 0)
    stopifnot("fail must be lower than 1" = params$fail < 1)
    stopifnot("breaks must be numeric" = is.numeric(params$breaks))
    stopifnot("breaks and segments must be of same length" = length(params$breaks) == length(params$segments))
    stopifnot("breaks must > 0" = all(params$breaks > 0))
    stopifnot("breaks must be increasing"  = all(params$breaks == cummax(params$breaks)))
    stopifnot("breaks must not be duplicated" = ! any(duplicated(params$breaks)))
    stopifnot("segments must be numeric" = is.numeric(params$segments))
    stopifnot("segments must be >= 0" = all(params$segments >= 0))
    wfin <- which(is.finite(params$breaks))
    ntimes = length(params$breaks[wfin])
    dtime = c(params$breaks[1],params$breaks[2:ntimes] - params$breaks[1:(ntimes-1)])
    unscaledH = sum(params$segments[wfin]*dtime)
    scalefactor = -log(1-params$fail)/unscaledH
    hazards = params$segments*scalefactor
    cumH = sum(hazards[wfin]*dtime)
    stopifnot("Unsucess scale" = abs(exp(-cumH) - (1-params$fail) ) < 1e-12)
    return(.factory_piecewise(params$breaks, hazards))
  }
  message(
    "Valid parameters to define a Piecewise Exponential distribution are: \n",
    "breaks and hazards: for the exponential hazard until each break, or\n",
    "surv, breaks and segments : for the surviving proportion (no events) at the last break, or\n",
    "fail, breaks and segments: for the failure proportion (events) at the last finite break \n")
  stop("Not valid parameters")
}

