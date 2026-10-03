# autoslider.trade

Trading tables, listings and figures, downstream of
[`autoslider.core`](https://github.com/pharmaverse/autoslider.core). It
produces slide-ready outputs for finance trading in the same style as
`autoslider`, and follows the same naming convention:

- `t_*_slide()` — tables (`rtables`)
- `l_*_slide()` — listings (`rlistings`)
- `g_*_slide()` — figures (`ggplot2`)

The instruments an output is computed on are selected by the sibling package
[`filters.trade`](https://github.com/shajoezhu/filters.trade); this package just
renders them.

## Installation

```r
# install.packages("devtools")
devtools::install()
```

## Usage

```r
library(autoslider.trade)

# A table of per-instrument performance
t_performance_slide(eg_prices)

# An equity curve, with every instrument rescaled to start at 1
g_equity_slide(eg_prices)

# A listing of executed trades
l_trades_slide(eg_trades)

# Risk analytics on a longer series
t_risk_slide(eg_ohlc)
t_risk_slide(eg_ohlc, benchmark = "ANZ.NZ")
g_drawdown_slide(eg_ohlc)
g_return_dist_slide(eg_ohlc)
g_rolling_risk_slide(eg_ohlc)
g_portfolio_risk_slide(eg_ohlc, benchmark = "ANZ.NZ")

# Render any of the above to a slide deck
autoslider.core::generate_slides(t_performance_slide(eg_prices), "performance.pptx")
```

Column names are configurable through the `symbol`, `date` and `close`
arguments, but default to the trading code convention shared with
`filters.trade`:

- `SYMBOL` is the column holding the trading code. Override with
  `options(autoslider.trade.code_col = "ticker")`.

## Risk analytics

The outputs above describe what happened to the price. The risk outputs describe
what it cost in risk terms. All of them are computed from prices rather than from
a return series you have to prepare first, and all accept the same configurable
column names.

| Function | Output |
| --- | --- |
| `t_risk_slide()` | Return, volatility, VaR, CVaR, drawdown and the Sharpe/Sortino/Calmar/Omega ratios per instrument |
| `g_drawdown_slide()` | Underwater plot per instrument |
| `g_return_dist_slide()` | Return histogram with VaR and CVaR marked |
| `g_rolling_risk_slide()` | Rolling volatility, Sharpe, VaR and maximum drawdown |
| `g_portfolio_risk_slide()` | Correlation heatmap, rolling Beta and information ratio against a benchmark |
| `t_horizon_slide()` | The same metrics over several trailing windows, to separate short from long |
| `render_stock_report()` | A whole deck for one trading code, written reading first |

## Beyond price history

Some of these need data the package does not fetch, and they say so rather than
inventing it:

| Function | Output | Data it needs |
| --- | --- | --- |
| `g_range_slide()` | Price against its own trailing high-low band, and where in it the last price sits | Prices |
| `g_vol_term_slide()` | Realized volatility over several windows at once | Prices |
| `t_option_slide()` | Implied volatility and Greeks for a sheet of option quotes | Option quotes (`STRIKE`, `TYPE`, `PRICE`, `TTE`) and spot |
| `g_vol_premium_slide()` | Implied against realized volatility at matched tenors | Prices and option quotes |
| `t_fundamentals_slide()` | Metrics across reporting periods, with the change | Financials you supply as `METRIC` / `PERIOD` / `VALUE` |

`METHODS.md` records the formula behind every number, where it came from, and
which parts of the `quantitative-trading` and `cb_teams_marketplace` plugins were
adopted, which were dropped, and why.

## One-company report

`t_horizon_slide()` and `render_stock_report()` answer "how does this look short
term, and long term" for one instrument. The report puts a written reading of
both first, then the evidence behind it:

```r
render_stock_report(eg_ohlc, "AIR.NZ", benchmark = "ANZ.NZ", "air_nz_report.pptx")
```

The reading is derived from the same metrics the rest of the deck shows, so it
cannot drift from the numbers. Point it at your own history and change the
trading code to report on a different company — `examples/report.R` is that
workflow in a few editable lines. Handing `options` or `financials` to
`render_stock_report()` adds the corresponding slides; leaving them `NULL`
leaves them out.

The benchmark is named as one of the trading codes already in the data, e.g.
`t_risk_slide(eg_ohlc, benchmark = "ANZ.NZ")`. Portfolio weights are not
computed here: describing what the price did is this package's job, deciding what
to hold is not.
