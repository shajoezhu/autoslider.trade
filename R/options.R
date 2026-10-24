#' Black-Scholes price of a European option
#'
#' The closed-form price of a European call or put on a non-dividend-paying
#' underlying, which is what the Greeks and the implied volatility below are
#' derived from.
#'
#' @param spot `numeric` price of the underlying
#' @param strike `numeric` strike price
#' @param tte `numeric` time to expiry, in years
#' @param sigma `numeric` volatility, annualized
#' @param r `numeric` continuously compounded risk-free rate
#' @param type `character` `"call"` or `"put"`
#' @return `numeric` option price
#' @noRd
bs_price <- function(spot, strike, tte, sigma, r = 0.02, type = c("call", "put")) {
  type <- match.arg(type)
  if (!is.finite(sigma) || sigma <= 0 || tte <= 0 || spot <= 0 || strike <= 0) {
    return(NA_real_)
  }
  d1 <- (log(spot / strike) + (r + sigma^2 / 2) * tte) / (sigma * sqrt(tte))
  d2 <- d1 - sigma * sqrt(tte)
  disc <- strike * exp(-r * tte)
  if (type == "call") {
    spot * pnorm(d1) - disc * pnorm(d2)
  } else {
    disc * pnorm(-d2) - spot * pnorm(-d1)
  }
}

#' Black-Scholes Greeks of a European option
#'
#' `vega` and `rho` are scaled the way they are quoted: vega per one volatility
#' point (1%) and rho per one rate point (1%), not per 1.00.
#'
#' @param spot `numeric` price of the underlying
#' @param strike `numeric` strike price
#' @param tte `numeric` time to expiry, in years
#' @param sigma `numeric` volatility, annualized
#' @param r `numeric` continuously compounded risk-free rate
#' @param type `character` `"call"` or `"put"`
#' @return A named `numeric` vector of `delta`, `gamma`, `vega`, `theta`, `rho`
#' @noRd
bs_greeks <- function(spot, strike, tte, sigma, r = 0.02, type = c("call", "put")) {
  type <- match.arg(type)
  na <- setNames(rep(NA_real_, 5L), c("delta", "gamma", "vega", "theta", "rho"))
  if (!is.finite(sigma) || sigma <= 0 || tte <= 0 || spot <= 0 || strike <= 0) {
    return(na)
  }
  sqrt_t <- sqrt(tte)
  d1 <- (log(spot / strike) + (r + sigma^2 / 2) * tte) / (sigma * sqrt_t)
  d2 <- d1 - sigma * sqrt_t
  disc <- strike * exp(-r * tte)
  gamma <- dnorm(d1) / (spot * sigma * sqrt_t)
  vega <- spot * dnorm(d1) * sqrt_t
  out <- c(
    delta = if (type == "call") pnorm(d1) else pnorm(d1) - 1,
    gamma = gamma,
    vega = vega / 100,
    theta = if (type == "call") {
      (-spot * dnorm(d1) * sigma / (2 * sqrt_t) - r * disc * pnorm(d2)) / 365
    } else {
      (-spot * dnorm(d1) * sigma / (2 * sqrt_t) + r * disc * pnorm(-d2)) / 365
    },
    rho = if (type == "call") {
      strike * tte * disc * pnorm(d2) / 100
    } else {
      -strike * tte * disc * pnorm(-d2) / 100
    }
  )
  out
}

#' Implied volatility solved from an option price
#'
#' Inverts `bs_price()` by bisection on the volatility. Bisection rather than
#' Newton: it cannot diverge, and one volatility is cheap to price.
#'
#' A price below its intrinsic value, or above the value of the underlying, has
#' no volatility that produces it, and gives `NA`.
#'
#' @param price `numeric` observed option price
#' @param spot `numeric` price of the underlying
#' @param strike `numeric` strike price
#' @param tte `numeric` time to expiry, in years
#' @param r `numeric` continuously compounded risk-free rate
#' @param type `character` `"call"` or `"put"`
#' @return `numeric` annualized implied volatility, or `NA_real_`
#' @noRd
bs_iv <- function(price, spot, strike, tte, r = 0.02, type = c("call", "put")) {
  type <- match.arg(type)
  lo <- 1e-4
  hi <- 5
  if (!is.finite(price) || tte <= 0 || spot <= 0 || strike <= 0) {
    return(NA_real_)
  }
  if (price <= bs_price(spot, strike, tte, lo, r, type) ||
    price >= bs_price(spot, strike, tte, hi, r, type)) {
    return(NA_real_)
  }
  for (i in seq_len(100L)) {
    mid <- (lo + hi) / 2
    if (bs_price(spot, strike, tte, mid, r, type) < price) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  (lo + hi) / 2
}

#' Read an option quote table into one row per contract
#'
#' @param options `data.frame` with columns `STRIKE`, `TYPE`, `PRICE`, `TTE`
#' @param spot `numeric` price of the underlying
#' @param r `numeric` risk-free rate
#' @return A `data.frame` with the contract, its implied volatility and Greeks
#' @noRd
option_rows <- function(options, spot, r = 0.02) {
  assert_that(has_name(options, c("STRIKE", "TYPE", "PRICE", "TTE")))
  out <- do.call(rbind, lapply(seq_len(nrow(options)), function(i) {
    type <- tolower(as.character(options$TYPE[i]))
    type <- if (type %in% c("c", "call")) "call" else "put"
    iv <- bs_iv(options$PRICE[i], spot, options$STRIKE[i], options$TTE[i], r, type)
    g <- bs_greeks(spot, options$STRIKE[i], options$TTE[i], iv, r, type)
    data.frame(
      CONTRACT = sprintf(
        "%s %.4g %.2fy", if (type == "call") "Call" else "Put",
        options$STRIKE[i], options$TTE[i]
      ),
      Moneyness = 100 * (spot / options$STRIKE[i] - 1),
      Price = options$PRICE[i],
      ImpliedVol = 100 * iv,
      Delta = g[["delta"]],
      Gamma = g[["gamma"]],
      Vega = g[["vega"]],
      Theta = g[["theta"]],
      Rho = g[["rho"]],
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

#' Implied volatility at the tenors and the strike closest to the money
#'
#' Pairs each tenor with the contract that best matches it, so an implied
#' volatility can be compared against the realized volatility of the same window.
#'
#' @param options `data.frame` with columns `STRIKE`, `TYPE`, `PRICE`, `TTE`
#' @param spot `numeric` price of the underlying
#' @param tenors `numeric` time to expiry wanted, in years
#' @param r `numeric` risk-free rate
#' @return `numeric` implied volatilities, one per tenor, `NA` where no contract fits
#' @noRd
atm_iv <- function(options, spot, tenors, r = 0.02) {
  vapply(tenors, function(t) {
    near_tte <- which.min(abs(options$TTE - t))
    if (!length(near_tte)) {
      return(NA_real_)
    }
    same_tenor <- options[abs(options$TTE - options$TTE[near_tte]) < 1e-9, , drop = FALSE]
    atm <- which.min(abs(same_tenor$STRIKE - spot))
    type <- tolower(as.character(same_tenor$TYPE[atm]))
    type <- if (type %in% c("c", "call")) "call" else "put"
    100 * bs_iv(
      same_tenor$PRICE[atm], spot, same_tenor$STRIKE[atm], same_tenor$TTE[atm], r, type
    )
  }, numeric(1))
}
