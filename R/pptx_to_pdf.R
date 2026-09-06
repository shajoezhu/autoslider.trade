#' Convert PowerPoint files to PDF
#'
#' Rendered slide decks are easier to share as PDF. `pptx_to_pdf()` converts
#' one or more `.pptx` files, choosing automatically between the two
#' converters it can find: LibreOffice (`soffice --headless`) or Microsoft
#' PowerPoint through Windows COM automation. The latter is what makes the
#' function usable from WSL, where no Linux converter is installed but a
#' Windows Office is reachable.
#'
#' @param path `character` vector of paths to `.pptx` files
#' @param output_dir `character` Directory to write the PDFs into. Defaults to
#'   the directory of each input file.
#'
#' @return A `character` vector of the created PDF paths, invisibly
#' @export
#'
#' @examples
#' if (interactive()) {
#'   generate_slides(t_performance_slide(eg_prices), "performance.pptx")
#'   pptx_to_pdf("performance.pptx")
#' }
#'
pptx_to_pdf <- function(path, output_dir = NULL) {
  assertthat::assert_that(is.character(path), length(path) >= 1L)
  if (!is.null(output_dir)) {
    assertthat::assert_that(assertthat::is.string(output_dir))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  paths <- normalizePath(path, mustWork = TRUE)
  is_pptx <- grepl("\\.pptx$", paths, ignore.case = TRUE)
  if (!all(is_pptx)) {
    stop(
      "Not .pptx file(s): ", paste(path[!is_pptx], collapse = ", "), ".",
      call. = FALSE
    )
  }

  backend <- pptx_backend()

  out <- vapply(
    paths,
    function(p) {
      out_dir <- if (is.null(output_dir)) dirname(p) else output_dir
      pptx_to_pdf_one(p, out_dir, backend)
    },
    character(1L)
  )
  names(out) <- NULL
  invisible(out)
}

#' Report which PDF converter is available
#'
#' LibreOffice is preferred because it is a single command and works on
#' Linux, macOS and Windows. PowerPoint is the fallback for machines with a
#' Windows Office but no LibreOffice, notably WSL.
#'
#' @return A `character` scalar, `"libreoffice"` or `"powerpoint"`
#' @noRd
# Is LibreOffice actually usable? `soffice` can be present in PATH but fail to
# launch (e.g. a broken stub), in which case we must fall back to PowerPoint
# rather than erroring deep inside system2().
soffice_available <- function() {
  nzchar(Sys.which("soffice")) &&
    is.numeric(r <- tryCatch(
      system2("soffice", "--version", stdout = FALSE, stderr = FALSE),
      error = function(e) -1L
    )) && r == 0L
}

pptx_backend <- function() {
  if (soffice_available()) {
    return("libreoffice")
  }
  if (!is.null(powershell_exe())) {
    return("powerpoint")
  }
  stop(
    "No PDF converter found. Install LibreOffice (so `soffice` is on the ",
    "PATH), or make a Windows PowerShell reachable so Microsoft PowerPoint ",
    "can be used through COM.",
    call. = FALSE
  )
}

#' Convert a single file with the chosen backend
#'
#' @param input `character` Absolute path to a `.pptx` file
#' @param out_dir `character` Directory to write the PDF into
#' @param backend `character` From `pptx_backend()`
#' @return A `character` scalar, the path of the created PDF
#' @noRd
pptx_to_pdf_one <- function(input, out_dir, backend) {
  out_file <- file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(input)), ".pdf")
  )

  # system2() quotes the command but splices `args` into a shell command line
  # unquoted, so every path has to be quoted here. Windows UNC paths would
  # otherwise lose their backslashes to shell escaping and PowerPoint would be
  # handed a path that does not exist.
  if (identical(backend, "libreoffice")) {
    status <- system2(
      "soffice",
      c(
        "--headless", "--convert-to", "pdf",
        "--outdir", shQuote(out_dir), shQuote(input)
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  } else {
    status <- system2(
      powershell_exe(),
      c(
        "-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
        "-File", shQuote(powerpoint_script()),
        "-InPath", shQuote(to_windows_path(input)),
        "-OutPath", shQuote(to_windows_path(out_file))
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  }

  if (!file.exists(out_file)) {
    stop(
      "Conversion failed for ", basename(input), ".\n",
      paste(status, collapse = "\n"),
      call. = FALSE
    )
  }
  out_file
}

#' Locate PowerShell, or `NULL` when it is not reachable
#'
#' @return A `character` scalar path, or `NULL`
#' @noRd
powershell_exe <- function() {
  candidates <- c(
    Sys.which("powershell.exe"),
    file.path(
      "/mnt/c/Windows/System32/WindowsPowerShell/v1.0", "powershell.exe"
    ),
    file.path(
      "/mnt/c/windows/System32/WindowsPowerShell/v1.0", "powershell.exe"
    )
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (!length(found)) NULL else found[[1L]]
}

#' Translate a WSL path into the UNC path Windows expects
#'
#' PowerPoint is a Windows process, so it can only open paths such as
#' `\\wsl.localhost\Ubuntu\home\...`. On native Windows the path is already
#' valid and is returned unchanged.
#'
#' @param path `character` Path to translate
#' @return A `character` scalar
#' @noRd
to_windows_path <- function(path) {
  if (!nzchar(Sys.which("wslpath"))) {
    return(path)
  }
  out <- system2("wslpath", c("-w", shQuote(path)), stdout = TRUE)
  paste(out, collapse = "")
}

#' Write the PowerShell script that drives PowerPoint over COM
#'
#' Kept in a file rather than inlined so that paths containing spaces or
#' non-ASCII characters survive the trip to PowerShell.
#'
#' @return A `character` scalar, path to a temporary `.ps1` file
#' @noRd
powerpoint_script <- function() {
  ps1 <- tempfile(fileext = ".ps1")
  writeLines(
    c(
      "param([string]$InPath, [string]$OutPath)",
      "$ErrorActionPreference = 'Stop'",
      "$app = New-Object -ComObject PowerPoint.Application",
      "try {",
      "  $pres = $app.Presentations.Open($InPath, $true, $false, $false)",
      "  $pres.SaveAs($OutPath, 32)",
      "  $pres.Close()",
      "} finally {",
      "  $app.Quit()",
      "}"
    ),
    ps1
  )
  ps1
}
