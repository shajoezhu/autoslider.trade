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

* Fixed `sma()`, `bbands()` and `rsi()`: a window wider than the series returned
  garbage or raised instead of giving `NA`. Reporting on a short history hit it.

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
