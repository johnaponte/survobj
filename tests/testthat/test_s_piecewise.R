# Tests for piecewise
# JJAV 20231130

test_that(
  "Piecewise factory works well",
  {
    xx <- s_factory(s_piecewise, breaks = c(1,2,3,Inf), hazards = c(3,2,1,4))
    expect_type(xx,"list")
    expect_s3_class(xx,"SURVIVAL")
    expect_s3_class(s_factory(s_piecewise, surv = 0.4, breaks = c(1,2,3,Inf), segments = c(3,2,1,4)), "SURVIVAL")
    expect_s3_class(s_factory(s_piecewise, fail = 0.6, breaks = c(1,2,3,Inf), segments = c(3,2,1,4)), "SURVIVAL")
    expect_s3_class(s_piecewise(fail = 0.6, breaks = c(1,2,3,Inf), segments = c(3,2,1,4)), "SURVIVAL")
    expect_s3_class(s_piecewise(surv = 0.6, breaks = c(1,2,3,Inf), segments = c(3,2,1,4)), "SURVIVAL")
    expect_error(s_factory(s_piecewise, hola = 4))
    expect_error(s_piecewise(breaks = c(1,2,4,5,Inf), hazards = c(1,2,3,4)))
    expect_equal(s_piecewise(fail = 0.4, breaks = c(1,2,3,Inf), segments = c(1,2,3,4))$sfx(3),0.6)
    expect_length(xx$rsurv(10), 10)
    expect_length(xx$rsurvhr(rep(1,10)), 10)
    expect_error(xx$sfx(-1))
    expect_error(xx$hfx(-1))
    expect_error(xx$Cum_Hfx(-1))
    expect_error(xx$invCum_Hfx(-1))
    expect_error(xx$rsurv(-1))
    expect_error(xx$rsurvhr(-1))
    xx <- s_piecewise(fail = 0.4, breaks = c(1.5,2.5,3.5,4.5,Inf), segments = c(2,4,6,8,10))
    expect_equal(xx$invCum_Hfx(xx$Cum_Hfx(seq(0,10,0.5))), seq(0,10,0.5))

})

test_that(
  "Piecewise with a single finite break does not overflow the interval-width index range (regression)",
  {
    # A single finite break followed by Inf means maxfinitb/wfin has length 1,
    # which previously produced reversed index ranges (2:1, 1:0) and Inf/NA dtimes.
    xxs <- s_piecewise(surv = 0.5, breaks = c(1, Inf), segments = c(1, 1))
    expect_s3_class(xxs, "SURVIVAL")
    expect_equal(xxs$sfx(1), 0.5)

    xxf <- s_piecewise(fail = 0.6, breaks = c(1, Inf), segments = c(1, 1))
    expect_s3_class(xxf, "SURVIVAL")
    expect_equal(xxf$sfx(1), 0.4)

    # segments other than 1 should scale correctly too
    xxf2 <- s_piecewise(fail = 0.3, breaks = c(2, Inf), segments = c(5, 5))
    expect_equal(xxf2$sfx(2), 0.7)
})

