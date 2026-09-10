<<<<<<< HEAD
# autoslider.trade 0.0.1
=======
# autoslider.trade 0.0.0.9003
>>>>>>> main

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
