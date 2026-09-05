# autoslider.trade

Trading tables, listings and figures, downstream of
[`autoslider.core`](https://github.com/insightsengineering/autoslider). It
produces slide-ready outputs for finance trading in the same style as
`autoslider`, and follows the same naming convention:

- `t_*_slide()` — tables (`rtables`)
- `l_*_slide()` — listings (`rlistings`)
- `g_*_slide()` — figures (`ggplot2`)

The instruments an output is computed on are selected by the sibling package
[`filters.trade`](https://github.com/joezhu/filters.trade); this package just
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

# Render any of the above to a slide deck
autoslider.core::generate_slides(t_performance_slide(eg_prices), "performance.pptx")
```

Column names are configurable through the `symbol`, `date` and `close`
arguments, but default to the trading code convention shared with
`filters.trade`:

- `SYMBOL` is the column holding the trading code. Override with
  `options(autoslider.trade.code_col = "ticker")`.
