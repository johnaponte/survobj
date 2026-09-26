# Confirm is a single number

Evaluates if the argument is a single number

## Usage

``` r
is_single_number(x)
```

## Arguments

- x:

  a variable to evaluate

## Value

`TRUE` if it is a single number, `FALSE` otherwise

## Examples

``` r
is_single_number(3)  #TRUE
#> [1] TRUE
is_single_number(c(3,3,3)) #FALSE
#> [1] FALSE
is_single_number(list(a=3)) #FALSE
#> [1] FALSE
is_single_number("3") #FALSE
#> [1] FALSE
```
