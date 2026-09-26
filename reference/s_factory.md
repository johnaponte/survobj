# Factory of objects of class SURVIVAL

Create objects of the class SURVIVAL

## Usage

``` r
s_factory(s_family, ...)
```

## Arguments

- s_family:

  a factory for a specific distribution

- ...:

  parameters to define the survival distribution

## Value

a SURVIVAL object

## Details

The objects of the class SURVIVAL define different distributions of
survival times. Each class has its own set of parameters but once the
SURVIVAL object is defined, they have access to the same functions to
calculate:

- survival function:
  [`sfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md),

- hazard function:
  [`hfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md),

- cumulative hazard function:
  [`Cum_Hfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md)

- the inverse of the cumulative hazard function:
  [`invCum_Hfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md).

- generate random survival times:
  [`rsurv()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md)

- generate random survival times under proportional hazard ratio:
  [`rsurvhr()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md).

There are several functions to plot the distributions

- generic S3: `plot.SURVIVAL()`

- [`plot_survival()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md):
  to plot the functions

- [`ggplot_survival_random()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md):
  to ggplot random draws from the distribution

- [`compare_survival()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md):
  to compare the functions of two SURVIVAL objects

## Distributions

The following factories are implemented:

- [`s_exponential()`](https://johnaponte.github.io/survobj/reference/s_exponential.md):
  for Exponential distributions

- [`s_weibull()`](https://johnaponte.github.io/survobj/reference/s_weibull.md):
  for Weibull distributions

- [`s_gompertz()`](https://johnaponte.github.io/survobj/reference/s_gompertz.md):
  for Gompertz distributions

- [`s_piecewise()`](https://johnaponte.github.io/survobj/reference/s_piecewise.md):
  for Piecewise exponential distributions

- [`s_loglogistic()`](https://johnaponte.github.io/survobj/reference/s_loglogistic.md):
  for Log-Logistic distributions

- [`s_lognormal()`](https://johnaponte.github.io/survobj/reference/s_lognormal.md):
  for Log-Normal distributions

## Examples

``` r
# Define a SURVIVAL object
obj <- s_factory(s_exponential, lambda = 2)

# Survival, Hazard and Cumulative hazard at time 0.4
sfx(SURVIVAL = obj, t= 0.4)
#> [1] 0.449329
hfx(SURVIVAL = obj, t = 0.4)
#> [1] 2
Cum_Hfx(SURVIVAL = obj, t = 0.4)
#> [1] 0.8

# Time when the Cumulative hazard is 0.8
invCum_Hfx(SURVIVAL = obj, H = 0.8)
#> [1] 0.4

# Draw one random survival time from the distribution
rsurv(SURVIVAL = obj, n = 1)
#> [1] 0.1737629

# Draw one random survival time from the distribution, with hazard ratio 0.5
rsurvhr(SURVIVAL = obj, hr = 0.5)
#> [1] 0.05279276

# Plot the survival functions
plot(obj)
```
