#' Calculate conditional SDS gain from Z-scores
#'
#' @description
#' Fast calculation of conditional SDS gain taking the full history of data
#' into account.
#'
#' @param z Numeric vector with personal data in standard scores. Missing values
#' are accepted.
#' @param r Symmetric numeric matrix of size `length(z)` with
#' Pearson correlations between elements of `z`.
#' @param h Integer, >= 2, setting columns `1:h` as historic predictors, and columns
#' `(h + 1):length(z)` as future time points.
#' @param band Integer specifying the maximum number of historic predictor
#' to include.
#' @return A list with components:
#' 1. `gain`: Conditional SDS gain for values in `z`. `NA` if `z` is missing. No
#' gain score is calculated for the first element in `z`. (n)
#' 1. `hat`: Predicted value for past and future time points (n)
#' 1. `sigma2`: Error variance for past and future time points (n)
#' 1. `beta`: Matrix of regression weights (n x h)
#' @examples
#' r <- matrix(c(1.000, 0.702, 0.663, 0.580,
#'               0.702, 1.000, 0.856, 0.791,
#'               0.663, 0.856, 1.000, 0.844,
#'               0.580, 0.791, 0.844, 1.000),
#'               nrow = 4, ncol = 4, byrow = FALSE)
#' z <- c(-0.5, 1, 0.5, -0.5)
#'
#' # up to 10 historic predictor
#' calculate_gain(z, r)
#'
#' # one-step back conditional SDS
#' calculate_gain(z, r, band = 1)
#'
#' # set cols 1:3 as observed and col 4 as future time point
#' calculate_gain(z, r, h = 3)
#' @export
calculate_gain <- function(z, r, h = length(z), band = 10L) {

  stopifnot(is.matrix(r))
  stopifnot(exprs = {
    length(z) == nrow(r)
    length(z) == ncol(r)})
  h <- h[1L]
  stopifnot(h >= 1L && h <= length(z))

  # create backshift matrix from band
  n <- length(z)
  f <- n - h
  p <- outer(1L:h, 1L:h,
             function(X, Y) {
               d = X - Y;
               ifelse(d > 0L & d <= band, 1L, 0L)
             })

  # static model for future points
  # add column h as predictor, but limit number of predictors to band
  if (f) {
    static <- c(ifelse(band >= h, 1L, 0L), p[h, -h])
    static <- matrix(static, nrow = f, ncol = h, byrow = TRUE)
    p <- rbind(p, static)
  }

  # initialise
  beta <- matrix(0, nrow = n, ncol = h)
  sigma2 <- rep(NA_real_, times = n)

  # parameter for time-dependent models for current time
  for (i in 1L:h) {
    idx <- which(p[i, ] == 1L)
    if (!length(idx)) {
      beta[i, ] <- NA_real_
      next
    }
    s <- fastmatrix::sweep.operator(r, idx)
    beta[i, idx] <- s[idx, i]
    sigma2[i] <- s[i, i]
  }
  # parameters for static model for future time points
  if (f) {
    idx <- which(p[h + 1L, ] == 1L)
    idy <- (h + 1L):n
    if (!length(idx)) {
      beta[idy, idx] <- NA_real_
      sigma2[idy] <- NA_real_
    } else {
      s <- fastmatrix::sweep.operator(r, idx)
      beta[idy, idx] <- t(s[idx, idy])
      sigma2[idy] <- diag(s)[idy]
    }
  }

  # predict mean
  data <- matrix(z[1L:h], nrow = n, ncol = h, byrow = TRUE)
  data[is.na(data) & p == 0L] <- 0
  hat <- rowSums(beta * data, na.rm = FALSE)

  # calculate conditional SDS gain
  gain <- (z - hat)/sqrt(sigma2)

  # make hat and sigma consistent
  sigma2[is.na(hat)] <- NA_real_

  return(list(gain = gain,
              hat = hat,
              sigma2 = sigma2,
              beta = beta))
}


#' Append observed ages with a grid of future time points
#'
#' @param t Numeric vector, observed ages (in months)
#' @param width Numeric scalar, width of the grid (in months)
#' @param end Numeric scalar, maximum age (in months)
#' @param delta Numeric scalar, distance between points (in months)
#' @param gap Numeric scalar, gap between `t` and start of grid
#' @return A vector of ages
#' @examples
#' make_future_grid(0.44)
#' @export
append_future_grid <- function(t, width = 6, delta = 0.25, gap = 0.5) {
  start <- floor(4 * (max(t, na.rm = TRUE) + gap)) / 4
  f <- seq(from = start, to = start + width, by = delta)
  c(t, f)
}


#' Fit the Cole 1995 model to a correlation matrix
#'
#' @param r A numeric square correlation matrix with `dimnames(r)` defined
#' as time points.
#' @param nugget A pair of values needed to extrapolate close to the diagonal.
#' `nugget[1]` is the limiting value used for modelling the diagonal.
#' It is 0.95 by default. `nugget[2]` is the shift parameter used to indicate
#' the distance from the diagonal. It is 0.1 by default.
#' @examples
#' r <- matrix(c(1.000, 0.702, 0.663, 0.580,
#'               0.702, 1.000, 0.856, 0.791,
#'               0.663, 0.856, 1.000, 0.844,
#'               0.580, 0.791, 0.844, 1.000),
#'               nrow = 4, ncol = 4, byrow = FALSE)
#' dimnames(r) <- list(0:3, 0:3)
#' fit <- fit_cormodel(r)
#' summary(fit)
#' # tanh = inverse Fisher transform
#' rhat <- tanh(predict(fit))
#' robs <- tanh(model.frame(fit)$phi)
#' plot(x = robs, y = rhat, xlab = "Observed", ylab = "Predicted")
#' abline(0, 1)
fit_cormodel <- function(r, nugget = c(0.95, 0.1)) {
  knots <- as.numeric(dimnames(r)[[1]])

  # apply nugget effect by resetting the diagonal
  diag(r) <- nugget[1L]

  # prepare dataset
  phi <- atanh(r)
  tl <- expand.grid(t1 = knots, t2 = knots + nugget[2L])
  data <- data.frame(tl, phi = as.vector(phi))
  data <- data[data$t1 <= data$t2, ]
  data$V1 <- log((data$t1 + data$t2) / 2)
  data$V2 <- log(data$t2 - data$t1)
  data$V3 <- 1 / (data$t2 - data$t1)

  # fit nodel
  fit <- lm(phi ~ V1 + V2 + V3 + I(V1 * V2) + I(V1^2), data = data)
  fit
}

fit_cormodel2 <- function(r, nugget = c(0.95, 0.1)) {
  knots <- as.numeric(dimnames(r)[[1]])

  # apply nugget effect by resetting the diagonal
  diag(r) <- nugget[1L]

  # prepare dataset
  phi <- atanh(r)
  tl <- expand.grid(t1 = knots, t2 = knots + nugget[2L])
  data <- data.frame(tl, phi = as.vector(phi))
  data <- data[data$t1 <= data$t2, ]

  # use regression tress
  data$V1 <- log((data$t1 + data$t2) / 2)
  data$V2 <- log(data$t2 - data$t1)
  data$V3 <- 1 / (data$t2 - data$t1)

  # fit nodel
  fit <- ranger::ranger(phi ~ ., data = data)
  list(fit = fit, data = data)
}


#' Obtain correlation table for arbitrary time points
#'
#' @param t A numeric vector of time points
#' @param yname Character. Either `"wgt_z"` or `"hgt_z"`
#' @inheritParams fit_cormodel
#' @return
#' A square numeric matrix of size `length(t)` with predicted correlations.
#' The `dimnames` are created as `as.character(t)`.
#' @examples
#' # weight correlations months 0:6
#' interpolate_cor(0:6)
#' @export
interpolate_cor <- function(t,
                            yname = c("wgt_z", "hgt_z"),
                            nugget = c(0.95, 0.1))
{
  yname <- match.arg(yname)

  # preserve original order and sort
  srt <- order(t)
  t <- t[srt]

  tt <- expand.grid(t1 = t, t2 = t)
  tt <- tt[tt$t1 < tt$t2, ]
  t1 <- tt$t1
  t2 <- tt$t2

  # OLS model estimates using shift 0.1 and nugget 0.95
  w_wgt <- c(`(Intercept)` = 1.53940532876132, V1 = 0.328888244058599, V2 = -0.950548077260837,
             V3 = -0.189961689073634, `I(V1 * V2)` = 0.142698942859639, `I(V1^2)` = 0.000515725478574575)
  w_hgt <- c(`(Intercept)` = 1.32894924459706, V1 = 0.250138268840872, V2 = -0.672294951930094,
             V3 = -0.104379119142487, `I(V1 * V2)` = 0.105029619067978, `I(V1^2)` = -0.0029889263501264)
  b <- switch(yname,
              wgt_z = w_wgt,
              hgt_z = w_hgt)

  # calculate predicted value
  v0 <- rep(1, length(t1))
  v1 <- log((t1 + t2) / 2)
  v2 <- log(t2 - t1)
  v3 <- 1/(t2 - t1)
  v4 <- v1 * v2
  v5 <- v1^2
  p <- tanh(v0 * b[1] + v1 * b[2] + v2 * b[3] + v3 * b[4] + v4 * b[5] + v5 * b[6])

  # handle very short intervals
  p <- ifelse(t2 - t1 < nugget[2L], nugget[1L], p)

  # fill matrix
  r <- diag(nrow = length(t))
  r[upper.tri(r)] <- p
  r[lower.tri(r)] <- t(r)[lower.tri(r)]
  dimnames(r) <- list(t, t)

  # return in original time sequence
  r[srt, srt]
}

plot_dual <- function(x, y, ref) {
  oldpar <- par(mfrow = c(1, 2), mar = c(4, 4, 1, 2))
  on.exit(par(oldpar))
  mycolors <- c(grDevices::hcl(120, 100, 40, 0.8),
                grDevices::hcl(240, 100, 40, 0.8),
                grDevices::hcl(0, 100, 40, 0.8))

  # Calculate z_gain
  n <- length(x)
  z <- AGD::y2z(x = x/12, y = y, sex = "F", ref = ref)
  r <- R_hgt[as.character(x), as.character(x)]
  g <- calculate_gain(z = z, r = r, band = n - 1)

  # LEFT: raw scale plot
  # draw population references
  sds <- c(-2, -1, 0, 1, 2)
  age <- seq(0, 7, 0.5)
  zm <- rep(sds, times = length(age))
  xm <- rep(age, each = length(sds))
  wm <- AGD::z2y(z = zm, x = xm/12, sex = 'F', ref = ref)
  wm <- matrix(wm, ncol = length(sds), byrow = TRUE)
  dimnames(wm) <- list(age, sds)
  matplot(x = age, y = wm, type = "l", lty = 1,
          col = "grey", lwd = c(1, 1, 2, 1, 1),
          xlab = "Age (months)", ylab = "Length (cm)")
  # draw personalised references
  cent <- c(-2:2, -1.64)
  z_end <- g$hat[n] + cent * sqrt(g$sigma2[n])
  for(i in 1:length(z_end)) {
    zout <- approx(x = c(x[length(x) - 1], 6), y = c(z[n - 1], z_end[i]), xout = age)$y
    yout <- AGD::z2y(x = age/12, z = zout, sex = "F", ref = ref)
    lty <- ifelse(i %in% c(3, 6), 1, 3)
    col <- mycolors[3]
    if (i == 6) col <- "black"
    lwd <- ifelse (i %in% c(3), 2, 1.3)
    matlines(x = age, y = yout, pch = 20, lwd = lwd, col = col, lty = lty)
  }
  # draw growth curve
  zout <- approx(x = x, y = z, xout = age)$y
  yout <- AGD::z2y(x = age/12, z = zout, sex = "F", ref = ref)
  matlines(x = age, y = yout, col = mycolors[2], lwd = 2)
  matpoints(x = x, y = y, pch = 20, cex = 1.5, col = mycolors[2])

  # RIGHT: Z-score plot
  # draw population references
  v <- matrix(c(c(0, 7), rep(-2:2, each = 2)), ncol = 6, byrow = FALSE)
  matplot(x = v[,1], y = v[,2:6],
          type = "l", lty = 1,
          col = "grey", lwd = c(1, 1, 1.5, 1, 1),
          ylim = c(-2.5, 2.5),
          xlab = "Age (months)", ylab = "Length (SDS)")
  # draw personalised references
  for (i in 1:length(z_end)) {
    zout <- approx(x = x[c(length(x) - 1, length(x))],
                   y = c(z[length(x) - 1], z_end[i]), xout = age)$y
    lty <- ifelse(i %in% c(3, 6), 1, 3)
    col <- mycolors[3]
    if (i == 6) col <- "black"
    lwd <- ifelse (i %in% c(3), 2, 1.3)
    matlines(x = age, y = zout, lwd = lwd, col = col, lty = lty)
  }
  # draw growth curve
  matlines(x = x, y = z, col = mycolors[2], lwd = 2)
  matpoints(x = x, y = z, pch = 20, cex = 1.5, col = mycolors[2])

  text(x = 4, y = -1.5, labels = paste("Conditional SDS gain:", round(g$gain[n], 2)), font = 2)

  return(list(x = x, y = y, z = z, sigma2 = g$sigma[n],
              zhat = g$hat[n], gain =  g$gain[n], z_end = z_end,
              zout = zout, yout = yout))
}
