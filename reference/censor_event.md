# Censor of events

if censor_time \< time, event is changed to 0, otherwise not changed

## Usage

``` r
censor_event(censor_time, time, event)

censor_time(censor_time, time)
```

## Arguments

- censor_time:

  the time to censor

- time:

  the time variable where the censor_time is applied

- event:

  the variable with the event. It can be initialized in the call with a
  value for all times.

## Value

censored time or event

## Details

if censor_time \< time, time is changed to censor_time, otherwise no
change

Be careful and do not overwrite the time variable with the censor time
variable to not lose track of the events

## Functions

- `censor_time()`: Censor time

## Examples

``` r
if (FALSE) { # {     FALSE }

# Typical workflow in a simulation of survival time.
# Simulate time to event (sim_t_event)
# and simulates the time to lost to follow up (tim_t_ltof)
# the simulation time frame is 1, so everything after 1 is censored

require(dplyr)
data.frame(
  sim_t_event = c(0.5,0.6,1,10,20),
  sim_t_ltof = c(2,0.5,2,2,0.8)
 ) |>
 mutate(sevent = censor_event(1,sim_t_event,event=1)) |>
 mutate(stime = censor_time(1,sim_t_event)) |>
 mutate(event = censor_event(sim_t_ltof, stime, sevent)) |>
 mutate(timeto = censor_time(sim_t_ltof, stime))
}
```
