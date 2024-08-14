# News
# survobj 3.1.1
Fix graph after simulation to start in survival 1 at time 0 and cumulative 
risk 0 at time 0

## survobj 3.1.0
Add logo

## survobj 3.0.0
Addition of recurrent event simulation under homogeneous and non homogeneous
Poisson process

## survobj 2.0.0
Addition of new distributions and new random generators functions

  - Add Log-Logistic distribution
  
  - Add Log-Normal distribution
  
  - Add generation of accelerated failure random times
  
  - Add generation of accelerated hazard times
  
  - Improve the cumulative hazard and inverse cumulative hazard function for 
  the Exponential Piece wise distribution
  
  - Add function to graph random generated times under proportional hazard
  model, accelerated failure time models, and accelerated hazard model.
  
  - New vignette shows the generator of accelerated failure random times.

## survobj 1.0
Submission to CRAN

## survobj 0.3
Total refractory of the factories and functions.

- function rsurvdf is defunct and replace by rsurvhr which is easier to use

- lincomb function is not needed anymore and deleted

- Inclusion of functions for censoring

- Inclusion of graphs for comparison

- Removal dependency on plyr

- Inclusion of tests


## survobj 0.2
- Include checks to ensure the parameter objects have single numbers

## survobj 0.1

- First version is ready to run
