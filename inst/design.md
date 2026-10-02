# autoslider.trade — design

## Purpose and scope

`autoslider.trade` produces tables, listings and figures for finance trading.
It is a downstream package of `autoslider.core` and follows the same naming
convention: `t_*_slide()` functions build tables, `l_*_slide()` functions build
listings and `g_*_slide()` functions build figures. Where `autoslider.core`
automates clinical study outputs from CDISC data such as `ADSL` and `ADAE`,
`autoslider.trade` automates trading outputs from price and trade data.

Each output function returns an object (`rtables` table, `rlistings` listing or
`ggplot` figure) that is rendered to a slide deck with
`generate_slides()`, re-exported from `autoslider.core`.

The instruments an output is computed on are *not* selected here: that is the
job of the sibling package `filters.trade`, which selects trading codes by
applying saved filter expressions. `autoslider.trade` assumes it is handed data
that has already been reduced to the instruments of interest.

In scope: building tables, listings and figures from trading data, and the
trading code convention that joins them. Computing a small set of technical
indicators (moving averages, Bollinger Bands, RSI, MACD) **for display inside
figures** is in scope, and the helpers live in `R/indicators.R`. So is computing
risk and risk-adjusted return metrics (volatility, Value at Risk, drawdown,
Sharpe, Sortino, Calmar, Omega, Beta, information ratio) **for display inside
figures and tables**; those helpers live in `R/risk.R`.

Out of scope: fetching market data, backtesting and signal generation, and
selecting which instruments are in scope. Those belong to `filters.trade` or to
data providers. Portfolio construction is out of scope too: weights are not
optimized here, so risk parity has no place in the package. Simulation (Monte
Carlo resampling and stress testing) is out of scope for the same reason as
backtesting, and because it would make the outputs depend on a random number
generator rather than on the data alone.

## Requirements

### Naming and structure

- Output functions **shall** follow the `autoslider` convention: `t_*_slide()`
  for tables, `l_*_slide()` for listings, `g_*_slide()` for figures.
- Every output function **shall** return an object renderable by
  `generate_slides()` from `autoslider.core`.
- `generate_slides()` **shall** be re-exported so that attaching this package
  is sufficient to render a deck.

### The trading code convention

- The column holding the trading code **shall** default to `SYMBOL`; the
  default **should** be overridable through a package option.
- The `SYMBOL` convention **shall** match the one used by `filters.trade`, by
  agreement rather than by a package dependency, so the two packages stay
  independent.

### Figures

- The package **shall** provide a function that plots the price history of one
  or more trading codes.
- The function **should** rescale every instrument to a common starting value
  so that instruments with different price levels can be compared, controlled
  by an argument.
- Column names **shall** be configurable through arguments.
- The package **shall** provide a candlestick figure (`g_candle_slide()`) that
  overlays Bollinger Bands and moving averages on the price panel and adds
  volume, RSI and MACD panels, ported and enriched from the user's
  `homepage-stock` charts.

### Listings

- The package **shall** provide a listing of trades, keyed by trading code.
- The listing **should** derive the notional value of each trade from quantity
  and price.

### Tables

- The package **shall** provide a table of per-instrument summary statistics
  covering the number of observations, first and last price, return and maximum
  drawdown.

### Risk and portfolio analytics

- The package **shall** provide a table (`t_risk_slide()`) summarising, per
  instrument, the return, volatility, downside deviation, Value at Risk and
  Conditional Value at Risk, the maximum and average drawdown with its longest
  duration, the Sharpe, Sortino, Calmar and Omega ratios, and the skewness and
  excess kurtosis of returns.
- The table **should** add Beta, tracking error and the information ratio when a
  benchmark trading code is named.
- The package **shall** provide a drawdown figure (`g_drawdown_slide()`) showing,
  per instrument, how far below its own running peak it sits over time.
- The package **shall** provide a return distribution figure
  (`g_return_dist_slide()`) with the Value at Risk and Conditional Value at Risk
  marked, since the fat tails that traded returns have are what a summary number
  hides.
- The package **shall** provide a rolling risk figure
  (`g_rolling_risk_slide()`) stacking rolling volatility, Sharpe ratio, Value at
  Risk and maximum drawdown, because risk changes over time.
- The package **shall** provide a portfolio risk figure
  (`g_portfolio_risk_slide()`) combining a correlation heatmap with rolling Beta
  and the information ratio against a benchmark.
- Returns **shall** be derived from prices in one internal helper
  (`returns_long()`), so that no two outputs can disagree on how a return is
  defined. An instrument's first observation has no predecessor and **shall** be
  dropped rather than emitted as a missing return.
- Multi-instrument comparison **shall** use only the dates on which every trading
  code traded, so that a correlation does not depend on which pair is asked for.
- Outputs **should** degrade gracefully on a short series: a rolling window wider
  than the series yields empty panels, and zero-variance series yield zero rather
  than `NaN` or `Inf`.
- No shipped output **shall** depend on a random number generator.
- The package **shall** provide a horizon table (`t_horizon_slide()`) recomputing
  the return, volatility, drawdown and Sharpe ratio over several trailing
  windows, because a short-term and a long-term view of the same instrument
  differ and one average belongs to neither.
- A window longer than the history available **shall** be dropped rather than
  silently computed on the full series.

### Reports

- The package **shall** provide `render_stock_report()`, which assembles one deck
  for one trading code: the written reading first, then the horizon table, price
  history, drawdown, risk summary, return distribution, candlestick chart and
  rolling risk, and a benchmark comparison when one is named.
- The written reading **shall** be derived from the same metrics the rest of the
  deck shows, so that the prose cannot contradict the numbers.
- The reading **shall** state what the history shows and **shall not** forecast,
  rate or recommend; every slide **shall** carry the disclaimer that it is a
  mechanical reading and not investment advice.
- The deck **shall** be one file: each output is decorated with its own title and
  handed to `generate_slides()` as a list, since an undecorated output has no
  title and fails while the title is split across lines.
- The candlestick slide **should** be left out when the data holds no OHLCV
  columns, rather than drawn from prices that were not observed.
- Changing the trading code **shall** be sufficient to report on a different
  instrument.

### Validation

- Every output function **shall** verify that the required columns are present
  and raise an error otherwise, so that a mistyped column name fails loudly
  rather than producing a misleading output.

### Optional

- Further outputs, for example a holdings table, **will** be added by following
  the same conventions.
- Risk parity weights and Monte Carlo stress testing **will** remain out of
  scope: both belong to portfolio construction and simulation rather than to
  rendering what the data says.
