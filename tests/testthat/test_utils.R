## Test utils
## JJAV 20231123


test_that("conversion dates works", {
 expect_equal(ytod(1), 365.25)
 expect_equal(dtoy(365.25), 1)
 expect_equal(dtoy(ytod(1)),1)
 expect_equal(dtom(mtod(12)),12)
 expect_equal(mtoy(ytom(1)),1)
})


test_that(
  "is_single_number works well",{
    expect_true(is_single_number(3))
    expect_false(is_single_number(c(3,3,3)))
    expect_false(is_single_number(list(a=3)))
    expect_false(is_single_number("3"))
  }
)


test_that(
  "Censoring is working",{
  df <- data.frame(
     sim_t_event = c(0.5,0.6,1,10,20),
     sim_t_ltof = c(2,0.5,2,2,0.8)
  )
  df$sevent = censor_event(1,df$sim_t_event, 1)
  df$stime = censor_time(1,df$sim_t_event)
  df$event = censor_event(df$sim_t_ltof, df$stime, df$sevent)
  df$timeto = censor_time(df$sim_t_ltof, df$stime)

  expect_equal(df$event, c(1,0,1,0,0))
  expect_equal(df$timeto, c(0.5,0.5,1,1,0.8))
})
