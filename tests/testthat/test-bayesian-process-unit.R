# Unit tests for Bayesian Process analysis

installJaspProcessForUnitTests()

test_that("Test that .procBayesStandardizedResidualsFromDraws works", {
  set.seed(1)
  dat <- data.frame(x = rnorm(50), w = rnorm(50))
  dat$m <- 0.5 * dat$x + rnorm(50)
  dat$y <- 0.3 * dat$x + 0.4 * dat$m + 0.2 * dat$w + rnorm(50)

  # With and without a mean structure (intercepts)
  for (meanstructure in c(TRUE, FALSE)) {
    fit    <- lavaan::sem("m ~ x\ny ~ x + m + w", data = dat, fixed.x = TRUE, meanstructure = meanstructure)
    parTbl <- lavaan::parameterTable(fit)
    data   <- lavaan::lavInspect(fit, "data")
    # Free parameters are named as in the Stan output of blavaan
    parTbl$pxnames <- ifelse(parTbl$free > 0, paste0("par[", parTbl$free, "]"), NA)
    nFree <- max(parTbl$free)
    est   <- parTbl$est[match(seq_len(nFree), parTbl$free)]

    # Draws that all equal the estimates give the plug-in standardized residuals and zero-width intervals
    draws <- matrix(est, nrow = 5, ncol = nFree, byrow = TRUE, dimnames = list(NULL, paste0("par[", seq_len(nFree), "]")))
    res   <- jaspProcess:::.procBayesStandardizedResidualsFromDraws(parTbl, data, draws, "y", ciLevel = 0.95)
    expect_equal(res[["mean"]], jaspProcess:::.procResiduals(fit)[["y"]][["standardized"]])
    expect_equal(res[["lower"]], res[["mean"]])
    expect_equal(res[["upper"]], res[["mean"]])

    # Without a credible interval only the posterior means are returned
    expect_named(jaspProcess:::.procBayesStandardizedResidualsFromDraws(parTbl, data, draws, "y"), "mean")

    # Varying draws, processed in several blocks of observations: compare with a loop over the draws
    draws <- draws + matrix(rnorm(length(draws), sd = 0.05), nrow(draws))
    drawOf <- function(s, lhs, op, rhs) draws[s, parTbl$pxnames[parTbl$lhs == lhs & parTbl$op == op & parTbl$rhs == rhs]]
    expected <- sapply(seq_len(nrow(draws)), function(s) {
      beta      <- c(drawOf(s, "y", "~", "x"), drawOf(s, "y", "~", "m"), drawOf(s, "y", "~", "w"))
      intercept <- if (meanstructure) drawOf(s, "y", "~1", "") else mean(dat$y) - sum(beta * colMeans(dat[, c("x", "m", "w")]))
      (dat$y - intercept - drop(as.matrix(dat[, c("x", "m", "w")]) %*% beta)) / sqrt(drawOf(s, "y", "~~", "y"))
    })
    res <- jaspProcess:::.procBayesStandardizedResidualsFromDraws(parTbl, data, draws, "y", ciLevel = 0.8, blockSize = 7)
    expect_equal(res[["mean"]], rowMeans(expected))
    expect_equal(res[["lower"]], apply(expected, 1, quantile, probs = 0.1, names = FALSE))
    expect_equal(res[["upper"]], apply(expected, 1, quantile, probs = 0.9, names = FALSE))

    # Observations with a missing value are dropped, as in .procResiduals
    if (meanstructure) {
      data[3, "w"] <- NA
      resMissing <- jaspProcess:::.procBayesStandardizedResidualsFromDraws(parTbl, data, draws, "y", ciLevel = 0.8)
      expect_equal(resMissing[["mean"]], res[["mean"]][-3])
    }
  }
})
