# Introduction to the *survobj* Package

by John Aponte

## Introduction

This package defines a set of classes that encapsulate survival distributions

The objects of the class SURVIVAL encapsulate distributions of survival times. Each class has its own set of parameters but once the SURVIVAL object is defined, they have access to the same functions to calculate:

-   survival time function: `sfx()`,

-   hazard time function: `hfx()`,

-   cumulative hazard function: `Cum_Hfx()`

-   the inverse of the cumulative hazard function: `invCum_Hfx()`.

-   generate random survival times: `rsurv()`

-   generate random survival times under proportional hazard ratio: `rsurvhr()`.

-   generate random survival times under accelerated failure : `rsuvrvaft()`

-   generate random survival times under accelerate hazard: `rsurvah()`


There are several functions to plot the distributions

-   generic S3: `plot.SURVIVAL()`

-   `plot_survival()`: to plot the functions

-   `ggplot_survival_random()`: to ggplot random draws from the distribution

-   `ggplot_survival_hr()`: to ggplot random draws from the distribution using hazard ratio

-   `ggplot_survival_aft()`: to ggplot random draws from the distribution using accelerated time failure

-   `ggplot_survival_ah()`: to ggplot random draws from the distribution using accelerated hazard 

-   `compare_survival()`: to graphically compare the functions of two SURVIVAL objects

## Distributions

The current factories are implemented:

-   `s_exponential()`: for Exponential distributions

-   `s_weibull()`: for Weibull distributions

-   `s_gompertz()`: for Gompertz distributions

-   `s_piecewise()`: for Piecewise exponential distributions

-   `s_loglogistic()`: for Log Logisitic distributions

-   `s_lognormal()`: for Log Normal distributions

See the vignettes for examples on the use on simulation of survival data.

## Installation

To install the development version of this package from github use:

`devtools::install_github("johnaponte/survobj", build_manual = T, build_vignettes = T)`

For more information:

<https://johnaponte.github.io/survobj/>
