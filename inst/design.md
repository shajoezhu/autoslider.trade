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
figures** is in scope, and the helpers live in `R/indicators.R`.

Out of scope: fetching market data, backtesting and signal generation, and
selecting which instruments are in scope. Those belong to `filters.trade` or to
data providers.

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

### Validation

- Every output function **shall** verify that the required columns are present
  and raise an error otherwise, so that a mistyped column name fails loudly
  rather than producing a misleading output.

### Optional

- Further outputs, for example a drawdown figure or a holdings table, **will**
  be added by following the same conventions.
