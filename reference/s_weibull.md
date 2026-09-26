# Factory of SURVIVAL objects with Weibull distributions

Creates a SURVIVAL object with a Weibull distribution.

## Usage

``` r
s_weibull(...)
```

## Arguments

- ...:

  Parameters to define the distribution. See the Parameters for details

## Value

a SURVIVAL object of the Weibull distribution family. See the
documentation of `s_factory` for the methods available for SURVIVAL
objects

## Parameters

To create a Weibull survival object the following options are available:

*`scale`* and *`shape`* to specify the canonical parameters of the
distribution, or

*`surv`*, *`t`* and *`shape`* for the proportion surviving (no events)
at time t and the shape parameter, or

*`fail`*, *`t`* and *`shape`* for the proportion failing (events) at
time t and the shape parameter, or

*`intercept`* and *`scale`* for the parameters returned by
`survreg(.., dist = "weibull")` models.

scale = -log(surv)/(t^shape)

scale = -log(1-fail)/(t^shape)

The parameters should be spelled correctly as partial matching is not
available

## Examples

``` r
s_weibull(scale = 2,shape = 2)
#> SURVIVAL object
#> Distribution:  WEIBULL 
#> scale : 2 
#> shape : 2 
s_weibull(surv = 0.6, t= 12, shape = 0.5)
#> SURVIVAL object
#> Distribution:  WEIBULL 
#> scale : 0.1474627 
#> shape : 0.5 
s_weibull(fail = 0.4, t = 12, shape =0.5)
#> SURVIVAL object
#> Distribution:  WEIBULL 
#> scale : 0.1474627 
#> shape : 0.5 
s_weibull(intercept = 0.4, scale = 0.5)
#> SURVIVAL object
#> Distribution:  WEIBULL 
#> scale : 0.449329 
#> shape : 2 
```
