require(survival)
test_that(
  "lognormal factory works well",
  {
    xx <- s_factory(s_lognormal, scale = 1, shape = 2)
    expect_type(xx,"list")
    expect_s3_class(xx,"SURVIVAL")
    expect_s3_class(s_factory(s_lognormal, surv = 0.4, t=1, shape = 0.5), "SURVIVAL")
    expect_s3_class(s_factory(s_lognormal, fail = 0.6, t=1, shape = 1.5), "SURVIVAL")
    expect_s3_class(s_lognormal(fail = 0.6, t=1, shape = 1.1), "SURVIVAL")
    expect_s3_class(s_lognormal(surv = 0.6, t=1, shape = 1.1), "SURVIVAL")
    expect_error(s_factory(s_lognormal, hola = 4))
    expect_error(s_lognormal(scale = 1, shape = -2 ))
    expect_error(s_lognormal(scale = -1, shape = 1.5 ))
    yy <- s_lognormal(surv = 0.4, t = 1, shape = 2)
    expect_equal(yy$sfx(1), 0.4)
    expect_equal(yy$invCum_Hfx(yy$Cum_Hfx(1)), 1)
    expect_length(xx$rsurv(10), 10)
    expect_length(xx$rsurvhr(rep(1,10)), 10)
    expect_error(xx$sfx(-1))
    expect_error(xx$hfx(-1))
    expect_error(xx$Cum_Hfx(-1))
    expect_error(xx$invCum_Hfx(-1))
    expect_error(xx$rsurv(-1))
    expect_error(xx$rsurvhr(-1))

    # This test takes a lot of time. Only done if required

  if (FALSE) {

    # Test the survhr
    reps = 1000
    hr = 0.5
    scale = 2
    shape = 1.5
    npergroup = 1000
    xobj <- s_lognormal(scale = scale, shape = shape)
    grp <- c(rep(0,npergroup),rep(1,npergroup))
    hrvector = c(rep(1,npergroup), rep(hr, npergroup))
    res <- lapply(1:reps, function(x){
      t = xobj$rsurvhr(hrvector)
      df <- data.frame(grp, t)
      # CoxPH
      creg <- survival::coxph(Surv(t) ~ grp, data = df)
      rcox <- exp(coef(creg))
      return(c( hrcox = unname(rcox)))
    })
    res2 <- apply(do.call(rbind,res),2,mean)

    expect_equal(unname(res2["hrcox"]),hr, tolerance = 1/reps*10)


   # test parameters from survreg
   reps = 1000
   intercept = 2
   scale = 1.5
   yobj <- s_lognormal(intercept = 2, scale = scale)
   resp <- lapply(1:reps, function(x){
     t<- yobj$rsurv(1000)
     fitmod <- survival::survreg(Surv(t) ~ 1, dist = "lognormal")
     pintercept = fitmod$coefficients["(Intercept)"]
     pscale = fitmod$scale
     return(c(intercept = unname(pintercept), scale = unname(pscale)))
   })
   resp2 <- apply(do.call(rbind, resp),2,mean)
   expect_equal(unname(resp2["intercept"]),intercept, tolerance = 1/reps*10)
   expect_equal(unname(resp2["scale"]),scale, tolerance = 1/reps*10)

   # Test the aft but it takes a lot of time
   reps = 1000
   aft = 0.5
   npergroup = 1000
   intercept = 2
   scale = 1.5
   zobj <- s_lognormal(intercept = intercept, scale = scale)
   grp <- c(rep(0,npergroup),rep(1,npergroup))
   aftvector = c(rep(1,npergroup), rep(aft, npergroup))
   res <- lapply(1:reps, function(x){
     t = zobj$rsurvaft(aftvector)
     df <- data.frame(grp, t)
     # From AFT model
     fitaft <- survival::survreg(Surv(t) ~ grp, data = df, dist = "lognormal")
     pintercept = fitaft$coefficients["(Intercept)"]
     pscale = fitaft$scale
     baft <- exp(-coef(fitaft))
     return(c(intercept = unname(pintercept),
              scale = unname(pscale),
              baft = unname(baft[2])))
   })
   res2 <- apply(do.call(rbind,res),2,mean)
   expect_equal(unname(res2["scale"]),scale, tolerance = 1/reps*10)
   expect_equal(unname(res2["intercept"]),intercept, tolerance = 1/reps*10)
   expect_equal(unname(res2["baft"]), aft, tolerance =1/reps*10)


  }

})

test_that(
  "lognormal invCum_Hfx does not truncate to Inf for large H (regression)",
  {
    # 1 - exp(-H) underflows to exactly 1 for H > ~36 (S(t) < 2.2e-16),
    # which used to make qlnorm(1 - exp(-H), ...) truncate the quantile to Inf.
    xx <- s_lognormal(scale = 2, shape = 2)

    expect_false(is.infinite(xx$invCum_Hfx(40)))
    expect_false(is.infinite(xx$invCum_Hfx(1000)))
    expect_true(all(is.finite(xx$invCum_Hfx(c(40, 100, 500, 1000)))))

    # round trip should hold well beyond the old underflow boundary
    expect_equal(xx$Cum_Hfx(xx$invCum_Hfx(1000)), 1000, tolerance = 1e-6)

    # should still agree with the old formula in the range where it was accurate
    expect_equal(
      xx$invCum_Hfx(c(0.001, 0.5, 1, 5)),
      qlnorm(1 - exp(-c(0.001, 0.5, 1, 5)), log(2), 2),
      tolerance = 1e-6
    )
})

test_that(
  "lognormal hfx(0) equals the limit 0 instead of NA (regression)",
  {
    # dlnorm(0,...) and plnorm(0,...) are both exactly 0 in double precision,
    # so f(0)/S(0) = 0/1 = 0 with no division-by-zero risk; hfx() used to
    # special-case t == 0 to NA_real_ instead of returning this true limit.
    xx <- s_lognormal(scale = 2, shape = 0.5)
    expect_identical(xx$hfx(0), 0)
    expect_false(anyNA(xx$hfx(c(0, 0.5, 1, 2))))
    expect_equal(
      xx$hfx(c(0, 0.5, 1, 2)),
      dlnorm(c(0, 0.5, 1, 2), log(2), 0.5) / (1 - plnorm(c(0, 0.5, 1, 2), log(2), 0.5))
    )
})
