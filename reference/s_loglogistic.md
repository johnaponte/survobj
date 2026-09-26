# Factory of SURVIVAL objects with Log Logistic distributions

Creates a SURVIVAL object with a Log Logistic distribution.

## Usage

``` r
s_loglogistic(...)
```

## Arguments

- ...:

  Parameters to define the distribution. See the Parameters for details

## Value

a SURVIVAL object of the log-logistic distribution family. See the
documentation of `s_factory` for the methods available for SURVIVAL
objects

## Parameters

To create a Log Logistic survival object the following options are
available:

*`scale`* and *`shape`* to specify the canonical parameters of the
distribution, or

*`surv`*, *`t`* and *`shape`* for the proportion surviving (no events)
at time t and the shape parameter, or

*`fail`*, *`t`* and *`shape`* for the proportion failing (events) at
time t and the shape parameter, or

*`intercept`* and *`scale`* for the parameters returned by
`survreg(.., dist = "loglogistic")` models.

The parameters should be spelled correctly as partial matching is not
available

## Examples

``` r
s_loglogistic(scale = 2,shape = 2)
#> SURVIVAL object
#> Distribution:  LOGLOGISTIC 
#> scale : 2 
#> shape : 2 
s_loglogistic(surv = 0.6, t= 12, shape = 0.5)
#> SURVIVAL object
#> Distribution:  LOGLOGISTIC 
#> scale : 0.03703704 
#> shape : 0.5 
s_loglogistic(fail = 0.4, t = 12, shape =0.5)
#> SURVIVAL object
#> Distribution:  LOGLOGISTIC 
#> scale : 0.03703704 
#> shape : 0.5 
s_loglogistic(intercept = 0.4, scale = 0.5)
#> SURVIVAL object
#> Distribution:  LOGLOGISTIC 
#> scale : 0.67032 
#> shape : 2 
```
