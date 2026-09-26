# Factory of SURVIVAL objects with Exponential distributions

Creates a SURVIVAL object with an Exponential distribution.

## Usage

``` r
s_exponential(...)
```

## Arguments

- ...:

  Parameters to define the distribution. See the Parameters for details

## Value

a SURVIVAL object of the exponential distribution family. See the
documentation of `s_factory` for the methods available for SURVIVAL
objects

## Parameters

To create an exponential survival object the following options are
available:

*`lambda`* to specify the canonical parameter of the distribution, or

*`surv`* and *`t`* for the proportion surviving (no events) at time t,
or

*`fail`* and *`t`* for the proportion failing (events) at time t

lambda = -log(surv)/t

lambda = -log(1-fail)/t

The parameters should be spelled correctly as partial matching is not
available

## Examples

``` r
s_exponential(lambda = 3)
#> SURVIVAL object
#> Distribution:  EXPONENTIAL 
#> lambda : 3 
s_exponential(surv = 0.4, t = 2)
#> SURVIVAL object
#> Distribution:  EXPONENTIAL 
#> lambda : 0.4581454 
s_exponential(fail = 0.6, t = 2)
#> SURVIVAL object
#> Distribution:  EXPONENTIAL 
#> lambda : 0.4581454 
```
