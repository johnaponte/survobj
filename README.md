<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/survobj)](https://CRAN.R-project.org/package=survobj)
<!-- badges: end -->
<img src="man/figures/logo.png" align="right" height="139" alt="survobj website" />

# survobj

*by John Aponte*

`survobj` simulates survival times through a consistent,
reusable interface for each distribution, using an object-oriented
design. It supports the Exponential, Weibull, Gompertz, Log-Logistic,
Log-Normal, and Piecewise Exponential distributions, and can generate
random variates under Proportional Hazards, Accelerated Failure Time,
and Extended Hazards models, as well as under renewal and
non-homogeneous Poisson recurrent event processes.

It is meant for simulation studies: power calculations, sample size
justification, or checking how an analysis method behaves under a
known data-generating process.

## Quick start

```r
library(survobj)

# Define a Weibull SURVIVAL object from a failure proportion at a given time
obj <- s_weibull(fail = 0.4, t = 1, shape = 1.5)

# Survival, hazard and cumulative hazard at time 0.5
sfx(obj, 0.5)
hfx(obj, 0.5)
Cum_Hfx(obj, 0.5)

# Draw 10 random survival times from the baseline distribution
rsurv(obj, 10)

# Draw random survival times under a hazard ratio of 0.7
rsurvhr(obj, rep(0.7, 10))

# Plot the survival, hazard, cumulative hazard and inverse cumulative
# hazard functions
plot(obj)
```

## Usage

It is necessary first to define a `SURVIVAL` object for a distribution
(e.g. with `s_weibull()`), in order to evaluate its properties or to
simulate survival times from it. This object encapsulates, in a
consistent way, all the functions associated with that distribution,
so the same code works regardless of the distribution chosen.

Once a `SURVIVAL` object is defined, it gives access to the same set
of functions regardless of the underlying distribution:

-   `sfx()`: survival function
-   `hfx()`: hazard function
-   `Cum_Hfx()`: cumulative hazard function
-   `invCum_Hfx()`: inverse of the cumulative hazard function
-   `rsurv()`: random survival times from the baseline distribution
-   `rsurvhr()`: random survival times under a Proportional Hazards
    model
-   `rsurvaft()`: random survival times under an Accelerated Failure
    Time model
-   `rsurveh()`: random survival times under the Extended Hazards
    model (combined Proportional Hazards and Accelerated Failure Time
    effects)

Recurrent events (repeated episodes per subject) can be simulated
under a renewal process or a non-homogeneous Poisson process:

-   `renewhr()` / `renewaft()`: next episode time under a renewal
    process, given the previous episode time
-   `nhpphr()` / `nhppaft()`: next episode time under a non-homogeneous
    Poisson process, given the previous episode time

A set of plotting helpers visualize simulations against the baseline
distribution using Kaplan-Meier and cumulative hazard curves:

-   `plot(obj)` / `plot_survival()`: plot the functions of a single
    `SURVIVAL` object
-   `ggplot_survival_random()`: plot simulated draws from the baseline
    distribution
-   `ggplot_survival_hr()` / `ggplot_survival_aft()` /
    `ggplot_survival_eh()`: plot simulated draws under each model,
    against the baseline
-   `compare_survival()`: compare two `SURVIVAL` objects graphically,
    even from different distribution families

## Distributions

`SURVIVAL` objects can be created using the following factory functions:

-   `s_exponential()`: Exponential distribution
-   `s_weibull()`: Weibull distribution
-   `s_gompertz()`: Gompertz distribution
-   `s_piecewise()`: Piecewise Exponential distribution
-   `s_loglogistic()`: Log-Logistic distribution
-   `s_lognormal()`: Log-Normal distribution

Each of these functions can be parameterized either by its canonical
parameters, or by the proportion surviving/failing at a given time,
which is often more natural when planning a study.

See the vignettes for worked examples of simulating survival data,
including recurrent events and non-proportional hazards trials.

## Installation

To install the released version from CRAN use:

```r
install.packages("survobj")
```

To install the development version of this package from GitHub use:

```r
devtools::install_github("johnaponte/survobj", build_manual = TRUE, build_vignettes = TRUE)
```

## Learn more

<https://johnaponte.github.io/survobj/>
