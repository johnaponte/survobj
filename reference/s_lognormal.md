# Factory of SURVIVAL objects with Log Normal distributions

Creates a SURVIVAL object with a Log Normal distribution.

## Usage

``` r
s_lognormal(...)
```

## Arguments

- ...:

  Parameters to define the distribution. See the Parameters for details

## Value

a SURVIVAL object of the log-normal distribution family. See the
documentation of `s_factory` for the methods available for SURVIVAL
objects

## Parameters

To create a Log Normal survival object the following options are
available:

*`scale`* and *`shape`* to specify the canonical parameters of the
distribution, or

*`surv`*, *`t`* and *`shape`* for the proportion surviving (no events)
at time t and the shape parameter, or

*`fail`*, *`t`* and *`shape`* for the proportion failing (events) at
time t and the shape parameter, or

*`intercept`* and *`scale`* for the parameters returned by
`survreg(.., dist = "lognormal")` models.

The scale parameter is the median value of the distribution, and the
shape is the log standard deviation

The parameters should be spelled correctly as partial matching is not
available

## Examples

``` r
s_lognormal(scale = 2,shape = 2)
#> SURVIVAL object
#> Distribution:  LOGNORMAL 
#> scale : 2 
#> shape : 2 
s_lognormal(surv = 0.6, t= 12, shape = 0.5)
#> SURVIVAL object
#> Distribution:  LOGNORMAL 
#> scale : 13.62056 
#> shape : 0.5 
s_lognormal(fail = 0.4, t = 12, shape =0.5)
#> SURVIVAL object
#> Distribution:  LOGNORMAL 
#> scale : 13.62056 
#> shape : 0.5 
s_lognormal(intercept = 0.4, scale = 0.5)
#> SURVIVAL object
#> Distribution:  LOGNORMAL 
#> scale : 1.491825 
#> shape : 0.5 
```
