require(survival)
test_that(
  "Exponential factory works well",
  {
    xx <- s_factory(s_exponential, lambda = 3)
    expect_type(xx,"list")
    expect_s3_class(xx,"SURVIVAL")
    expect_s3_class(s_factory(s_exponential, surv = 0.4, t=1), "SURVIVAL")
    expect_s3_class(s_factory(s_exponential, fail = 0.6, t=1), "SURVIVAL")
    expect_s3_class(s_exponential(fail = 0.6, t=1), "SURVIVAL")
    expect_s3_class(s_exponential(surv = 0.6, t=1), "SURVIVAL")
    expect_error(s_factory(s_exponential, hola = 4))
    expect_error(s_exponential(lambda = -2))
    expect_equal(xx$sfx(1),exp(-3))
    expect_equal(xx$hfx(1), 3)
    expect_equal(xx$Cum_Hfx(1), 3)
    expect_equal(xx$invCum_Hfx(0.5), 0.5/3)
    expect_length(xx$rsurv(10), 10)
    expect_length(xx$rsurvhr(rep(1,10)), 10)
    expect_error(xx$sfx(-1))
    expect_error(xx$hfx(-1))
    expect_error(xx$Cum_Hfx(-1))
    expect_error(xx$invCum_Hfx(-1))
    expect_error(xx$rsurv(-1))
    expect_error(xx$rsurvhr(-1))

  if (FALSE) {

    # Test the survhr but it takes a lot of time
    reps = 1000
    hr = 0.5
    lambda = 3
    npergroup = 1000
    xobj <- s_exponential(lambda = lambda)
    grp <- c(rep(0,npergroup),rep(1,npergroup))
    hrvector = c(rep(1,npergroup), rep(hr, npergroup))
    res <- lapply(1:reps, function(x){
      t = xobj$rsurvhr(hrvector)
      df <- data.frame(grp, t)
      # From AFT to PH
      fitaft <- survival::survreg(Surv(t) ~ grp, data = df, dist = "exponential")
      rph <- exp(-coef(fitaft) / fitaft$scale)
      # CoxPH
      creg <- survival::coxph(Surv(t) ~ grp, data = df)
      rcox <- exp(coef(creg))
      return(c(lambda = unname(rph[1]), hrph = unname(rph[2]), hrcox = unname(rcox)))
    })
    res2 <- apply(do.call(rbind,res),2,mean)
    expect_equal(unname(res2["lambda"]),lambda, tolerance = 1/reps*10)
    expect_equal(unname(res2["hrph"]),hr, tolerance = 1/reps*10)
    expect_equal(unname(res2["hrcox"]),hr, tolerance = 1/reps*10)


    # Test the aft but it takes a lot of time
    reps = 1000
    aft = 0.5
    lambda = 3
    npergroup = 1000
    xobj <- s_exponential(lambda = lambda)
    grp <- c(rep(0,npergroup),rep(1,npergroup))
    aftvector = c(rep(1,npergroup), rep(aft, npergroup))
    res <- lapply(1:reps, function(x){
      t = xobj$rsurvaft(aftvector)
      df <- data.frame(grp, t)
      # From AFT model
      fitaft <- survival::survreg(Surv(t) ~ grp, data = df, dist = "exponential")
      rph <- exp(-coef(fitaft) / fitaft$scale)
      baft <- exp(coef(fitaft))
      return(c(lambda = unname(rph[1]), baft = unname(baft[2])))
    })
    res2 <- apply(do.call(rbind,res),2,mean)
    expect_equal(unname(res2["lambda"]),lambda, tolerance = 1/reps*10)
    expect_equal(unname(res2["baft"]),aft, tolerance = 1/reps*10)
  }

})

