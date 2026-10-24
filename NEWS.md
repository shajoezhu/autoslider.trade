# autoslider.trade 0.0.2

* Fixed `l_trades_slide()`: the listing now carries a default title, so
  `generate_slides()` can render it. Without a title the render failed while
  splitting the title across lines. The title is settable through `title`.

* Added `t_risk_slide()` — a per-instrument risk summary table: return,
  volatility, downside deviation, Value at Risk (historical and Cornish-Fisher)
  and Conditional Value at Risk, maximum and average drawdown with its longest
  duration, and the Sharpe, Sortino, Calmar and Omega ratios, plus skewness and
  excess kurtosis. Passing `benchmark` adds Beta, tracking error and the
  information ratio.

* Added three risk figures: `g_drawdown_slide()` (underwater plot),
  `g_return_dist_slide()` (return histogram with VaR and CVaR marked) and
  `g_rolling_risk_slide()` (rolling volatility, Sharpe ratio, VaR and maximum
  drawdown, stacked as panels like `g_candle_slide()`).

* Added `g_portfolio_risk_slide()` — a correlation heatmap beside rolling Beta
  and per-instrument information ratio against a named benchmark.

* Added `R/risk.R`, the internal helper layer the new outputs share. Returns are
  derived from prices in one place so the table and the figures cannot disagree,
  and no output depends on a random number generator.

* Added `t_horizon_slide()` — the return, volatility, drawdown and Sharpe ratio
  recomputed over several trailing windows (1M / 3M / 1Y / full history), which
  is what separates a short-term view from a long-term one.

* Added `render_stock_report()` — one deck for one trading code: a written
  reading of the long term, the short term, the risk and the comparison against
  a benchmark, followed by the horizon table, price history, drawdown, risk
  summary, return distribution, candlestick chart and rolling risk. The wording
  is generated from the same metrics the deck shows. `examples/report.R` builds
  it from a ticker you can change.

* Added `stock_report` to the MCP server.

* Added `g_range_slide()` — the price against its own trailing high-low band and
  where in that band the last price sits, from the price-performance step of the
  `lseg` plugin's `equity-research` skill.

* Added `g_vol_term_slide()` — realized volatility over several trailing windows
  drawn side by side, from the realized-volatility step of the `lseg` plugin's
  `option-vol-analysis` skill.

* Added `t_option_slide()` and `g_vol_premium_slide()` — Black-Scholes pricing
  with implied volatility and Greeks, and implied against realized volatility at
  matched tenors. Both need option quotes, which the package does not fetch.

* Added `t_fundamentals_slide()` — a trend table for fundamentals or valuation
  metrics across reporting periods. It renders the numbers it is given and does
  not fetch financials.

* Added `METHODS.md`: the formula behind every metric, its source, and which of
  the `quantitative-trading` and `cb_teams_marketplace` skills were adopted,
  which were dropped, and why.

* `render_stock_report()` now includes the price-in-its-range figure and the
  volatility-by-window figure, and reads the long-term reading's position in the
  trailing range. It also accepts `options` and `financials`, adding the option
  pricing, implied-versus-realized and fundamentals slides when they are handed
  in and leaving them out when they are not.

* `g_vol_term_slide()` now says so and stops when no window fits the history,
  rather than drawing an empty chart.

* `render_stock_report()` now rasterizes its figures and writes them with
  `officer`, so their axis labels, legends and annotations are visible in the
  deck. `autoslider.core` draws figures with `grDevices::svg()`, whose cairo
  backend emits text as font glyphs instead of `<text>` elements, and PowerPoint
  does not draw glyph references: figures arrived with their geometry intact and
  every label missing. Use `fig_dpi = NA` to keep vector figures. `officer` is a
  new dependency.

* Fixed `sma()`, `bbands()` and `rsi()`: a window wider than the series returned
  garbage or raised instead of giving `NA`. Reporting on a short history hit it.

* Fixed the Cornish-Fisher Value at Risk: the expansion was applied to the
  upper-tail quantile, so a left-skewed series came out with a *smaller* loss
  than the Gaussian Value at Risk it is meant to correct. It now uses the lower
  tail and reduces to the parametric VaR when skewness and excess kurtosis are
  zero.

# autoslider.trade 0.0.1

* Initial scaffold of the package.

* Added `t_performance_slide()`, `g_equity_slide()` and `l_trades_slide()`,
  plus the example datasets `eg_prices` and `eg_trades`.

* Re-exported `generate_slides()` from `autoslider.core`.

* Added `pptx_to_pdf()` to render slide decks as PDF, using LibreOffice when
  available or Microsoft PowerPoint through Windows COM automation from WSL.

* Added `g_candle_slide()` — a candlestick figure (ported and enriched from
  `homepage-stock`) with Bollinger Bands, moving averages and high/low
  annotations, plus volume, RSI and MACD panels. Added the `cowplot` dependency
  for panel layout.

* Added the `nz_tickers` dataset (the 168-instrument NZX universe copied from
  `homepage-stock`) and a synthetic `eg_ohlc` OHLCV dataset for the candlestick
  figure and its tests.

* Added an MCP server (`inst/mcp/autoslider_trade_mcp_server.R`) that exposes
  the trade outputs as MCP tools for interactive deck building; registered as
  `autoslider_trade` in `~/.codebuddy/.mcp.json`.
