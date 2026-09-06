skip_if_no_converter <- function() {
  # Key off a *functional* backend, not mere presence: `soffice` can sit in PATH
  # but be unrunnable, in which case pptx_backend() falls back to PowerPoint.
  # If no backend can be used at all, skip rather than fail.
  be <- tryCatch(pptx_backend(), error = function(e) NULL)
  if (is.null(be)) {
    testthat::skip("No functional PDF converter available (no working soffice, no PowerShell/PowerPoint)")
  }
}

make_deck <- function(name) {
  out <- file.path(tempdir(), name)
  generate_slides(t_performance_slide(eg_prices), out)
  out
}

test_that("pptx_to_pdf converts a deck and returns the pdf path", {
  skip_if_no_converter()
  pptx <- make_deck("perf.pptx")

  pdf <- pptx_to_pdf(pptx)

  expect_true(file.exists(pdf))
  expect_equal(dirname(pdf), dirname(pptx))
  expect_equal(tools::file_path_sans_ext(basename(pdf)), "perf")
})

test_that("pptx_to_pdf honours output_dir", {
  skip_if_no_converter()
  pptx <- make_deck("perf_dir.pptx")
  dir.create(outdir <- file.path(tempdir(), "pdfs_out"), showWarnings = FALSE)

  pdf <- pptx_to_pdf(pptx, output_dir = outdir)

  expect_equal(dirname(normalizePath(pdf)), normalizePath(outdir))
  expect_true(file.exists(pdf))
})

test_that("pptx_to_pdf converts several files at once", {
  skip_if_no_converter()
  a <- make_deck("multi_a.pptx")
  b <- make_deck("multi_b.pptx")

  out <- pptx_to_pdf(c(a, b))

  expect_length(out, 2L)
  expect_true(all(file.exists(out)))
})

test_that("pptx_to_pdf rejects files that are not pptx", {
  txt <- tempfile(fileext = ".txt")
  writeLines("not a deck", txt)
  expect_error(pptx_to_pdf(txt), "Not \\.pptx")
})

test_that("pptx_to_pdf rejects missing files", {
  expect_error(pptx_to_pdf(file.path(tempdir(), "no_such_deck.pptx")))
})

test_that("the backend is reported as one of the supported converters", {
  skip_if_no_converter()
  expect_match(pptx_backend(), "^(libreoffice|powerpoint)$")
})

test_that("to_windows_path leaves paths untouched without wslpath", {
  skip_on_os("windows")
  skip_if_not(nzchar(Sys.which("wslpath")))

  expect_match(
    to_windows_path("/home/joezhu-hp"),
    "^\\\\\\\\wsl\\.localhost"
  )
})
