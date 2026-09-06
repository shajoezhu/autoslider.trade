# autoslider.trade 0.0.0.9000

* Initial scaffold of the package.

* Added `t_performance_slide()`, `g_equity_slide()` and `l_trades_slide()`,
  plus the example datasets `eg_prices` and `eg_trades`.

* Re-exported `generate_slides()` from `autoslider.core`.

* Added `pptx_to_pdf()` to render slide decks as PDF, using LibreOffice when
  available or Microsoft PowerPoint through Windows COM automation from WSL.
