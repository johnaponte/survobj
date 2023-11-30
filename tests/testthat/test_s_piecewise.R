# Tests for piecewise
# JJAV 20231130

test_that(
  "Piecewise factory works well",
  {
    xx <- s_factory(s_piecewise, breaks = c(1,2,3,Inf), hazards = c(3,2,1,4))
    expect_type(xx,"list")
    expect_s3_class(xx,"SURVIVAL")
    expect_s3_class(s_factory(s_piecewise, surv = 0.4, breaks = c(1,2,3,Inf), hazards = c(3,2,1,4)), "SURVIVAL")
    expect_s3_class(s_factory(s_piecewise, fail = 0.6, breaks = c(1,2,3,Inf), hazards = c(3,2,1,4)), "SURVIVAL")
    expect_s3_class(s_piecewise(fail = 0.6, breaks = c(1,2,3,Inf), hazards = c(3,2,1,4)), "SURVIVAL")
    expect_s3_class(s_piecewise(surv = 0.6, breaks = c(1,2,3,Inf), hazards = c(3,2,1,4)), "SURVIVAL")
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

})

