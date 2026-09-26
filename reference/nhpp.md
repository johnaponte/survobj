# Generate random recurrent episodes under a non-homogeneous Poisson process

Generate random draws from the distribution of a recurrent set of
survival times under a non-homogeneous Poisson process following Leemis
(1987)

## Usage

``` r
nhpphr(SURVIVAL, hr, prevtime)

nhppaft(SURVIVAL, aft, prevtime)
```

## Arguments

- SURVIVAL:

  Object of survival class

- hr:

  Vector of hazard ratios

- prevtime:

  Vector of previous survival times

- aft:

  Vector of accelerated failure ratios

## Value

Vector of survival times

## Functions

- `nhpphr()`: Recurrent episodes under a proportional hazard model

- `nhppaft()`: Recurrent episodes under an accelerated failure time
  model

## Examples

``` r
s_obj <- s_exponential(fail = 0.4, t = 1)
hr <- c(1,1,0.5,0.5)
time1 <- rsurvhr(s_obj, hr)
time2 <- nhpphr(s_obj,hr, time1)

s_obj <- s_exponential(fail = 0.4, t = 1)
aft <- c(1,1,0.5,0.5)
timea <- rsurvaft(s_obj, aft)
timeb <- nhppaft(s_obj, aft, timea)
```
