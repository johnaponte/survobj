test_that(
  ".hazard_yvals samples both SURVIVAL objects on the same grid and captures interior peaks (regression)",
  {
    # Previously compare_survival() computed the hazard ylim as
    # c(SURVIVAL1$hfx(0:timeto), SURVIVAL2$hfx(c(0, timeto))): SURVIVAL1 was
    # sampled across the domain but SURVIVAL2 only at the two endpoints, so an
    # interior peak in SURVIVAL2's hazard (e.g. log-logistic, log-normal) was
    # dropped from the range used to set the plot's y-axis limits.
    s1 <- s_weibull(scale = 0.05, shape = 1)          # flat, low hazard
    s2 <- s_loglogistic(scale = 0.3, shape = 3)       # interior-mode hazard
    timeto <- 10

    yvals <- .hazard_yvals(s1, s2, timeto)

    xx <- seq(0, timeto, length.out = 10001)
    true_peak <- max(s2$hfx(xx))

    expect_gte(max(yvals), true_peak * 0.99)

    # the old formula would have missed it: endpoints only capture the
    # much lower hazard at t = 0 and t = timeto
    old_yvals <- c(s1$hfx(0:timeto), s2$hfx(c(0, timeto)))
    old_yvals <- old_yvals[is.finite(old_yvals)]
    expect_lt(max(old_yvals), true_peak * 0.9)

    # symmetric: both SURVIVAL objects are sampled on the same grid
    xgrid <- seq(0, timeto, length.out = 101)
    expect_equal(sort(yvals), sort(c(s1$hfx(xgrid), s2$hfx(xgrid))[is.finite(c(s1$hfx(xgrid), s2$hfx(xgrid)))]))
})

test_that(
  "compare_survival runs without error for distributions with interior-mode hazards",
  {
    s1 <- s_weibull(scale = 0.05, shape = 1)
    s2 <- s_loglogistic(scale = 0.3, shape = 3)
    tmp <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp)
    on.exit({grDevices::dev.off(); unlink(tmp)})
    expect_no_error(compare_survival(s1, s2, timeto = 10))
})

test_that(
  "plot.SURVIVAL fails gracefully instead of crashing when uniroot finds no root (regression)",
  {
    # An improper distribution (negative-shape Gompertz) has a cure fraction
    # whose survival never drops to 0.05 within [0, 10], so uniroot(...,
    # extendInt = "downX") finds no sign change and used to throw an
    # unhandled error straight out of uniroot() instead of the intended
    # "Error finding an adequate time interval" message.
    obj <- s_gompertz(scale = 0.05, shape = -0.5)
    expect_true(obj$sfx(10) > 0.05)

    expect_error(
      plot(obj),
      "Error finding an adequate time interval"
    )

    # a proper distribution should still plot without error
    obj2 <- s_weibull(scale = 1, shape = 1.5)
    tmp <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp)
    on.exit({grDevices::dev.off(); unlink(tmp)})
    expect_no_error(plot(obj2))
})
