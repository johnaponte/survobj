require(survival)
test_that(
  "Gompertz factory works well",
  {
    xx <- s_factory(s_gompertz, scale = 1, shape = 2)
    expect_type(xx,"list")
    expect_s3_class(xx,"SURVIVAL")
    expect_s3_class(s_factory(s_gompertz, surv = 0.4, t=1, shape = 0.5), "SURVIVAL")
    expect_s3_class(s_factory(s_gompertz, fail = 0.6, t=1, shape = 1.5), "SURVIVAL")
    expect_s3_class(s_gompertz(fail = 0.6, t=1, shape = 1.1), "SURVIVAL")
    expect_s3_class(s_gompertz(surv = 0.6, t=1, shape = 1.1), "SURVIVAL")
    expect_error(s_factory(s_gompertz, hola = 4))
    expect_error(s_gompertz(scale = 1, shape = 0 ))
    expect_error(s_gompertz(scale = -1, shape = 1.5 ))
    expect_length(xx$rsurv(10), 10)
    expect_length(xx$rsurvhr(rep(1,10)), 10)
    expect_error(xx$sfx(-1))
    expect_error(xx$hfx(-1))
    expect_error(xx$Cum_Hfx(-1))
    expect_error(xx$invCum_Hfx(-1))
    expect_error(xx$rsurv(-1))
    expect_error(xx$rsurvhr(-1))

    # This test takes a lot of time. Only done if required

  if (TRUE) {

    # Test the survhr
    reps = 1000
    hr = 0.5
    scale = 2
    shape = 1.5
    npergroup = 1000
    xobj <- s_gompertz(scale = scale, shape = shape)
    grp <- c(rep(0,npergroup),rep(1,npergroup))
    hrvector = c(rep(1,npergroup), rep(hr, npergroup))
    res <- lapply(1:reps, function(x){
      t = xobj$rsurvhr(hrvector)
      df <- data.frame(grp, t)
     # CoxPH
      creg <- survival::coxph(Surv(t) ~ grp, data = df)
      rcox <- exp(coef(creg))
      return(c(hrcox = unname(rcox)))
    })
    res2 <- apply(do.call(rbind,res),2,mean)

    expect_equal(unname(res2["hrcox"]),hr, tolerance = 1/reps*10)
  }
})
