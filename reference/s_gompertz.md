# Factory of SURVIVAL objects with Gompertz distributions

Creates a SURVIVAL object with a Gompertz distribution.

## Usage

``` r
s_gompertz(...)
```

## Arguments

- ...:

  Parameters to define the distribution. See the Parameters for details

## Value

a SURVIVAL object of the Gompertz distribution family. See the
documentation of `s_factory` for the methods available for SURVIVAL
objects

## Parameters

To create a Gompertz survival object the following options are
available:

*`scale`* and *`shape`* to specify the canonical parameters of the
distribution, or

*`surv`*, *`t`* and *`shape`* for the proportion surviving (no events)
at time t and shape, or

*`fail`* and *`t`* and *`shape`* for the proportion failing (events) at
time t and shape.

scale = -log(surv)·shape/(exp(shape·t))

scale = -log(1-fail)·shape/(exp(shape·t))

The parameters should be spelled correctly as partial matching is not
available

## Negative shape

A negative `shape` produces a decreasing hazard whose cumulative hazard
is bounded above by `scale / abs(shape)` (following Bender, Augustin &
Blettner 2005). This makes the distribution improper: a cure fraction of
`exp(-scale / abs(shape))` of the subjects never experience the event,
so their simulated survival time (`rsurv`, `rsurvhr`, `rsurvaft`,
`rsurveh`) and
[`invCum_Hfx()`](https://johnaponte.github.io/survobj/reference/SURVIVAL.md)
are returned as `Inf`.

## Examples

``` r
s_gompertz(scale = 1, shape = 1.5)
#> SURVIVAL object
#> Distribution:  GOMPERTZ 
#> scale : 1 
#> shape : 1.5 
s_gompertz(surv = 0.4, t = 2, shape = 1.5)
#> SURVIVAL object
#> Distribution:  GOMPERTZ 
#> scale : 0.07201454 
#> shape : 1.5 
s_gompertz(fail = 0.6, t = 2, shape = 1.5)
#> SURVIVAL object
#> Distribution:  GOMPERTZ 
#> scale : 0.07201454 
#> shape : 1.5 
```
