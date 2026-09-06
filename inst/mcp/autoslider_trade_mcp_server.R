#!/usr/bin/env Rscript
# autoslider.trade MCP Server
#
# Exposes the trading-figure / table / listing functions of autoslider.trade as
# MCP tools, so any MCP-compatible client (Claude Desktop, Claude Code, Cursor,
# Open WebUI, etc.) can build trading slide decks conversationally — the same
# outputs autoslider.core produces for clinical data, but for prices and trades.
#
# Usage:
#   Rscript inst/mcp/autoslider_trade_mcp_server.R
#
# Register in your MCP client, e.g. for CodeBuddy add to ~/.codebuddy/.mcp.json:
#   {
#     "mcpServers": {
#       "autoslider_trade": {
#         "command": "Rscript",
#         "args": ["/abs/path/to/autoslider.trade/inst/mcp/autoslider_trade_mcp_server.R"],
#         "type": "stdio"
#       }
#     }
#   }
#
# Requirements:
#   install.packages("mcptools")   # on CRAN; provides mcp_server()/tool()/type_string()
#   # autoslider.trade already installed / loadable

suppressPackageStartupMessages({
  library(mcptools)
  library(ellmer)
  library(autoslider.trade)
})

# ---- session state ----------------------------------------------------------
# Holds renderable outputs across tool calls within one session.
.state <- new.env(parent = emptyenv())
.state$outputs <- NULL

# ---- helpers ----------------------------------------------------------------
stop_if <- function(cond, msg) if (cond) stop(msg, call. = FALSE)
require_outputs <- function() stop_if(
  is.null(.state$outputs) || length(.state$outputs) == 0L,
  "No outputs built yet. Call a render_* tool (e.g. candle_chart) first."
)

# Resolve a dataset: "example" pulls a bundled dataset from this package,
# otherwise read an .rds file from disk.
load_df <- function(data_path, example_name) {
  if (data_path == "example") {
    get(example_name, envir = asNamespace("autoslider.trade"))
  } else {
    readRDS(data_path)
  }
}

# ---- tool functions ---------------------------------------------------------

fn_list_outputs <- function() {
  paste(
    "Available trade outputs (render_* tools):",
    "  candle_chart  - g_candle_slide(): candlestick + Bollinger/MA/RSI/MACD",
    "  performance    - t_performance_slide(): per-instrument return / drawdown table",
    "  trades         - l_trades_slide(): trade listing",
    "  equity_curve   - g_equity_slide(): rebased price curve",
    "",
    "Then call trade_generate_slides(outfile=...) to assemble a .pptx,",
    "and optionally pptx_to_pdf(path=...) to convert it.",
    sep = "\n"
  )
}

fn_candle <- function(symbol, data_path) {
  df <- load_df(data_path, "eg_ohlc")
  if (nzchar(symbol)) df <- df[df$SYMBOL == symbol, ]
  stop_if(nrow(df) == 0L, sprintf("No rows for symbol '%s' in the data.", symbol))
  .state$outputs[["candle"]] <- g_candle_slide(df)
  sprintf("Candlestick chart built (%d rows). Call trade_generate_slides to render.", nrow(df))
}

fn_performance <- function(data_path) {
  df <- load_df(data_path, "eg_prices")
  .state$outputs[["performance"]] <- t_performance_slide(df)
  sprintf("Performance table built. Call trade_generate_slides to render.")
}

fn_trades <- function(data_path) {
  df <- load_df(data_path, "eg_trades")
  .state$outputs[["trades"]] <- l_trades_slide(df)
  sprintf("Trade listing built. Call trade_generate_slides to render.")
}

fn_equity <- function(data_path) {
  df <- load_df(data_path, "eg_prices")
  .state$outputs[["equity"]] <- g_equity_slide(df)
  sprintf("Equity curve built. Call trade_generate_slides to render.")
}

fn_generate_slides <- function(outfile, output_name) {
  require_outputs()
  stop_if(
    !output_name %in% names(.state$outputs),
    sprintf(
      "No output named '%s'. Built outputs: %s",
      output_name, paste(names(.state$outputs), collapse = ", ")
    )
  )
  outfile <- normalizePath(outfile, mustWork = FALSE)
  tryCatch(
    generate_slides(.state$outputs[[output_name]], outfile = outfile),
    error = function(e) stop("Slide generation error: ", e$message, call. = FALSE)
  )
  sprintf("Slides for '%s' written to: %s", output_name, outfile)
}

fn_pptx_to_pdf <- function(path, output_dir) {
  out <- pptx_to_pdf(path, if (nzchar(output_dir)) output_dir else NULL)
  sprintf("PDF(s) written: %s", paste(out, collapse = ", "))
}

fn_reset <- function() {
  .state$outputs <- NULL
  "Session state cleared."
}

# ---- register tools ---------------------------------------------------------

tools <- list(

  tool(
    fun = fn_list_outputs,
    name = "list_trade_outputs",
    description = paste(
      "List the trading output types this server can build and how to assemble",
      "them into slides. Call first to discover the render_* tools."
    ),
    arguments = list()
  ),

  tool(
    fun = fn_candle,
    name = "candle_chart",
    description = paste(
      "Build a candlestick chart with Bollinger Bands, moving averages, RSI and",
      "MACD for one or all trading codes (g_candle_slide). Adds it to the deck.",
      'Use data_path = "example" for the bundled OHLCV data, or an .rds path.'
    ),
    arguments = list(
      symbol = type_string(
        'Trading code to chart, e.g. "AIA.NZ". Empty string ("") charts all codes.'
      ),
      data_path = type_string(
        'Path to an .rds OHLCV data frame, or "example" for bundled eg_ohlc.'
      )
    )
  ),

  tool(
    fun = fn_performance,
    name = "performance_table",
    description = paste(
      "Build the per-instrument performance table (return, drawdown, first/last",
      "price) as a slide-ready rtables object (t_performance_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_prices.'
      )
    )
  ),

  tool(
    fun = fn_trades,
    name = "trades_listing",
    description = paste(
      "Build a trade listing (symbol, date, side, qty, price) as a slide-ready",
      "listing (l_trades_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds trades data frame, or "example" for bundled eg_trades.'
      )
    )
  ),

  tool(
    fun = fn_equity,
    name = "equity_curve",
    description = paste(
      "Build a rebased equity curve figure comparing instruments from a common",
      "starting point (g_equity_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_prices.'
      )
    )
  ),

  tool(
    fun = fn_generate_slides,
    name = "trade_generate_slides",
    description = paste(
      "Render one built output to a PowerPoint (.pptx) file and return its",
      "absolute path. Call a render_* tool first, then name the output to",
      'render (e.g. "candle", "performance", "trades", "equity").',
      "One output per file; call again for each deck you want."
    ),
    arguments = list(
      outfile = type_string('Output .pptx path, e.g. "/tmp/trade_deck.pptx".'),
      output_name = type_string(
        'Name of the built output to render: "candle", "performance", "trades" or "equity".'
      )
    )
  ),

  tool(
    fun = fn_pptx_to_pdf,
    name = "pptx_to_pdf",
    description = paste(
      "Convert a .pptx file to PDF (LibreOffice if available, else PowerPoint via",
      "Windows COM from WSL). Returns the PDF path(s)."
    ),
    arguments = list(
      path = type_string("Path to the .pptx file to convert."),
      output_dir = type_string(
        'Directory for the PDF. Empty string ("") writes next to the .pptx.'
      )
    )
  ),

  tool(
    fun = fn_reset,
    name = "trade_reset",
    description = "Clear the built outputs so a fresh deck can be assembled.",
    arguments = list()
  )

)

# ---- start server -----------------------------------------------------------

mcp_server(tools = tools)
