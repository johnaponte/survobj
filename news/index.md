# Changelog

## survobj 3.2.0

CRAN release: 2026-09-20

- Fix reversed argument order in
  [`renewaft()`](https://johnaponte.github.io/survobj/reference/renewal.md):
  the signature was `(SURVIVAL, prevtime, aft)`, inconsistent with
  [`renewhr()`](https://johnaponte.github.io/survobj/reference/renewal.md)/[`nhpphr()`](https://johnaponte.github.io/survobj/reference/nhpp.md)/[`nhppaft()`](https://johnaponte.github.io/survobj/reference/nhpp.md),
  causing generated renewal times to be computed with `prevtime` and
  `aft` swapped and occasionally precede the prior event.

- Fix index overflow in
  [`s_piecewise()`](https://johnaponte.github.io/survobj/reference/s_piecewise.md)
  when only a single finite break is supplied
  (e.g. `breaks = c(1, Inf)`), which produced reversed index ranges and
  made the function fail with “Unsucess scale”.

- Fix
  [`s_gompertz()`](https://johnaponte.github.io/survobj/reference/s_gompertz.md)
  silently generating `NaN` for a negative `shape` (decreasing hazard).
  Draws that fall into the resulting cure fraction now correctly return
  `Inf` instead of `NaN`. See the new “Negative shape” documentation
  section for
  [`s_gompertz()`](https://johnaponte.github.io/survobj/reference/s_gompertz.md).

- Fix numerical truncation to `Inf` in
  [`s_lognormal()`](https://johnaponte.github.io/survobj/reference/s_lognormal.md)’s
  [`invCum_Hfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md)
  and
  [`Cum_Hfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md)
  for large cumulative hazards / late times, caused by probabilities
  underflowing to exactly 0 or 1 in double precision. Both now use
  `log.p`/`lower.tail` forms of
  [`qlnorm()`](https://rdrr.io/r/stats/Lognormal.html)/[`plnorm()`](https://rdrr.io/r/stats/Lognormal.html)
  to stay accurate.

- Rename `rsurvah()`/`ggplot_survival_ah()` to
  [`rsurveh()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md)/[`ggplot_survival_eh()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md).
  These generate the Extended Hazards model of Chen & Jewell (2001)
  (`h*(t) = hr * aft * h0(aft * t)`, nesting proportional hazards and
  accelerated failure time as special cases), not the Accelerated
  Hazards model of Chen & Wang (2000) as the previous name implied. This
  is a breaking rename with no deprecated alias.

- Improve documentation and vignettes

## survobj 3.1.1

CRAN release: 2024-08-16

Fix graph after simulation to start in survival 1 at time 0 and
cumulative risk 0 at time 0

### survobj 3.1.0

Add logo

### survobj 3.0.0

Addition of recurrent event simulation under homogeneous and
non-homogeneous Poisson process

### survobj 2.0.0

Addition of new distributions and new random generators functions

- Add Log-Logistic distribution

- Add Log-Normal distribution

- Add generation of accelerated failure random times

- Add generation of accelerated hazard times

- Improve the cumulative hazard and inverse cumulative hazard function
  for the Exponential Piece wise distribution

- Add function to graph random generated times under proportional hazard
  model, accelerated failure time models, and accelerated hazard model.

- New vignette shows the generator of accelerated failure random times.

### survobj 1.0

Submission to CRAN

### survobj 0.3

Total refractory of the factories and functions.

- function rsurvdf is defunct and replace by rsurvhr which is easier to
  use

- lincomb function is not needed anymore and deleted

- Inclusion of functions for censoring

- Inclusion of graphs for comparison

- Removal dependency on plyr

- Inclusion of tests

### survobj 0.2

- Include checks to ensure the parameter objects have single numbers

### survobj 0.1

- First version is ready to run
