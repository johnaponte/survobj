require(survival)
test_that(
  "Weibull factory works well",
  {
    xx <- s_factory(s_weibull, scale = 1, shape = 2)
    expect_type(xx,"list")
    expect_s3_class(xx,"SURVIVAL")
    expect_s3_class(s_factory(s_weibull, surv = 0.4, t=1, shape = 0.5), "SURVIVAL")
    expect_s3_class(s_factory(s_weibull, fail = 0.6, t=1, shape = 1.5), "SURVIVAL")
    expect_s3_class(s_weibull(fail = 0.6, t=1, shape = 1.1), "SURVIVAL")
    expect_s3_class(s_weibull(surv = 0.6, t=1, shape = 1.1), "SURVIVAL")
    expect_error(s_factory(s_weibull, hola = 4))
    expect_error(s_weibull(scale = 1, shape = -2 ))
    expect_error(s_weibull(scale = -1, shape = 1.5 ))
    expect_equal(xx$sfx(1),  exp(-1*(1^2)))
    expect_equal(xx$hfx(1),  1*2*(1)^(2-1))
    expect_equal(xx$Cum_Hfx(1), 1*1^2)
    expect_equal(xx$invCum_Hfx(0.5), (0.5/1)^(1/2))
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
    xobj <- s_weibull(scale = scale, shape = shape)
    grp <- c(rep(0,npergroup),rep(1,npergroup))
    hrvector = c(rep(1,npergroup), rep(hr, npergroup))
    res <- lapply(1:reps, function(x){
      t = xobj$rsurvhr(hrvector)
      df <- data.frame(grp, t)
      # From AFT to PH
      fitaft <- survival::survreg(Surv(t) ~ grp, data = df, dist = "weibull", scale = 0)
      pscale = exp(-fitaft$coefficients["(Intercept)"]/fitaft$scale)
      pshape = 1/fitaft$scale
      phr <- exp(-fitaft$coefficients["grp"]/fitaft$scale)
      # CoxPH
      creg <- survival::coxph(Surv(t) ~ grp, data = df)
      rcox <- exp(coef(creg))
      return(c(scale = unname(pscale), shape = unname(pshape), hrph = unname(phr), hrcox = unname(rcox)))
    })
    res2 <- apply(do.call(rbind,res),2,mean)

    expect_equal(unname(res2["scale"]),scale, tolerance = 1/reps*10)
    expect_equal(unname(res2["shape"]),shape, tolerance = 1/reps*10)
    expect_equal(unname(res2["hrph"]),hr, tolerance = 1/reps*10)
    expect_equal(unname(res2["hrcox"]),hr, tolerance = 1/reps*10)


   # test parameters from survreg
   intercept = 2
   scale = 1.5
   yobj <- s_weibull(intercept = intercept, scale = scale)
   resp <- lapply(1:reps, function(x){
     t<- yobj$rsurv(1000)
     fitmod <- survival::survreg(Surv(t) ~ 1, dist = "weibull")
     pintercept = fitmod$coefficients["(Intercept)"]
     pscale = fitmod$scale
     return(c(intercept = unname(pintercept), scale = unname(pscale)))
   })
   resp2 <- apply(do.call(rbind, resp),2,mean)
   expect_equal(unname(resp2["scale"]),scale, tolerance = 1/reps*10)
   expect_equal(unname(resp2["intercept"]),intercept, tolerance = 1/reps*10)
  }

})
