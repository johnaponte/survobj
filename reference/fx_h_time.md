# Functions to help in time conversion

This set of functions helps in the time conversion, taking into account
generic times and not specific times. The conversions are based on the
assumption that 1 year is 365.25 days and is 12 months. There is no
adjustment for leap days or hours or difference of days between months

## Usage

``` r
dtom(x)

mtod(x)

dtoy(x)

ytod(x)

mtoy(x)

ytom(x)
```

## Arguments

- x:

  the time to convert

## Value

the converted time

## Functions

- `dtom()`: convert days to months

- `mtod()`: convert months to days

- `dtoy()`: convert days to years

- `ytod()`: convert years to days

- `mtoy()`: convert months to year

- `ytom()`: convert years to months

## Examples

``` r
dtom(365.25)
#> [1] 12
mtod(12)
#> [1] 365.25
dtoy(165.25)
#> [1] 0.4524298
ytod(1)
#> [1] 365.25
mtoy(12)
#> [1] 1
ytom(365.25)
#> [1] 4383
```
