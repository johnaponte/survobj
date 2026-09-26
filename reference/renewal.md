# Generate random recurrent episodes under a renewal Poisson process

Generate random draws from the distribution of a recurrent set of
survival times under a renewal Poisson process following Leemis (1987)

## Usage

``` r
renewhr(SURVIVAL, hr, prevtime)

renewaft(SURVIVAL, aft, prevtime)
```

## Arguments

- SURVIVAL:

  Object of survival class

- hr:

  Vector of hazard ratios

- prevtime:

  Vector of previous survival times

- aft:

  Vector of accelerated failure time ratios

## Value

Vector of survival times

Vector of survival times

## Functions

- `renewhr()`: Recurrent episodes under a proportional hazard model

- `renewaft()`: Recurrent episodes under an accelerated failure time
  model

## Examples

``` r
s_obj <- s_exponential(fail = 0.4, t = 1)
hr <- c(1,1,0.5,0.5)
time1 <- rsurvhr(s_obj, hr)
time2 <- renewhr(s_obj, hr, time1)

s_obj2 <- s_exponential(fail = 0.4, t = 1)
aft <- c(1,1,0.5,0.5)
timea <- rsurvaft(s_obj2, aft)
timeb <- renewaft(s_obj2, aft, timea)
```
