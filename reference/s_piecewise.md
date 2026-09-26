# Factory of SURVIVAL objects with Piecewise Exponential distributions

Creates a SURVIVAL object with a Piecewise Exponential distribution.

## Usage

``` r
s_piecewise(...)
```

## Arguments

- ...:

  Parameters to define the distribution. See the Parameters for details

## Value

a SURVIVAL object of the piecewise exponential distribution family. See
the documentation of `s_factory` for the methods available for SURVIVAL
objects

## Parameters

To create a piecewise exponential survival object the following options
are available:

*`breaks`* and *`hazards`* to specify the exponential (constant) hazard
until each break, or

*`surv`*, *`breaks`* and *`segments`* for the proportion surviving (no
events) at the end of the last segment, or

*`fail`*, *`breaks`* and *`segments`* for the proportion failing
(events) at the end of the last segment

If *`surv`* or *`fail`* parameters are indicated, the *`segments`* are
scaled to hazards in order to match the surviving or failing proportion
at the end of the last segment.

Define the last break point as `Inf` to fully define the distribution,
otherwise an error will be produced if the function after the last break
is requested

The parameters should be spelled correctly as partial matching is not
available

## Examples

``` r
s_piecewise(breaks = c(1,2,3,Inf), hazards = c(0.5,0.6,0.5,0.1))
#> SURVIVAL object
#> Distribution:  PIECEWISE 
#> breaks : 1 2 3 Inf 
#> hazards : 0.5 0.6 0.5 0.1 
s_piecewise(surv = 0.4, breaks = c(1,2,3,Inf), segments = c(1,2,1,2))
#> SURVIVAL object
#> Distribution:  PIECEWISE 
#> breaks : 1 2 3 Inf 
#> hazards : 0.2290727 0.4581454 0.2290727 0.4581454 
s_piecewise(fail = 0.6, breaks = c(1,2,3,Inf), segments = c(1,2,1,2))
#> SURVIVAL object
#> Distribution:  PIECEWISE 
#> breaks : 1 2 3 Inf 
#> hazards : 0.2290727 0.4581454 0.2290727 0.4581454 
```
