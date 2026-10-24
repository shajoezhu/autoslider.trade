#' Render a figure to a PNG file
#'
#' Figures are rasterized rather than handed to the slide as vector graphics,
#' because of how `autoslider.core` writes them: it draws them with
#' `grDevices::svg()`, whose cairo backend emits every glyph as a `<symbol>`
#' definition referenced by `<use>` instead of a `<text>` element. PowerPoint,
#' LibreOffice and Google Slides all ignore those glyph references, so a figure
#' arrives with its lines and areas intact and every axis label, legend entry and
#' annotation missing.
#'
#' The PNG device draws text with the real font instead, so what comes out of the
#' device is what the reader sees.
#'
#' @param p A `ggplot` object, or any object with a `print` method that draws it
#' @param file `character` path of the PNG to write
#' @param width,height `numeric` figure size in inches
#' @param dpi `integer` resolution of the raster
#' @return The path of the PNG written
#' @noRd
figure_png <- function(p, file, width = 9, height = 5, dpi = 300L) {
  assert_that(is.string(file), is.number(width), is.number(height), is.count(dpi))
  grDevices::png(
    filename = file, width = width, height = height, units = "in", res = dpi,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  file
}

#' Positions of the slides holding a figure
#'
#' `autoslider.core` writes figures as `.svg` media and nothing else does, so the
#' slides that reference an `.svg` are the figure slides. Positions are the
#' display order taken from `presentation.xml`, not the numbering of the slide
#' files, which need not match it.
#'
#' @param path `character` path of a `.pptx`
#' @return An `integer` vector of slide positions
#' @noRd
svg_slide_positions <- function(path) {
  z <- utils::unzip(path, list = TRUE)$Name
  slides <- sort(z[grepl("^ppt/slides/slide[0-9]+\\.xml$", z)])
  if (!length(slides)) {
    return(integer(0))
  }

  # rId -> slide file, then the order the slides are shown in. The presentation
  # also relates to masters and themes, so only slide targets are paired up.
  pres_rels <- paste(
    readLines(unz(path, "ppt/_rels/presentation.xml.rels"), warn = FALSE), collapse = ""
  )
  # A space, so the container element `<Relationships ...>` is not matched too.
  nodes <- regmatches(pres_rels, gregexpr("<Relationship [^>]*/?>", pres_rels))[[1L]]
  pairs <- vapply(nodes, function(n) {
    id <- regmatches(n, regexpr('Id="[^"]+"', n))
    target <- regmatches(n, regexpr('Target="[^"]+"', n))
    c(
      sub('Id="', "", sub('"$', "", id)),
      sub('Target="', "", sub('"$', "", target))
    )
  }, character(2L))
  slide_nodes <- pairs[, grepl("^slides/slide[0-9]+\\.xml$", pairs[2L, ]), drop = FALSE]
  by_id <- stats::setNames(paste0("ppt/", slide_nodes[2L, ]), slide_nodes[1L, ])

  pres <- paste(readLines(unz(path, "ppt/presentation.xml"), warn = FALSE), collapse = "")
  order <- regmatches(pres, gregexpr('<p:sldId[^>]*r:id="[^"]+"', pres))[[1L]]
  order <- regmatches(order, gregexpr('r:id="[^"]+"', order))
  order <- vapply(order, function(x) sub('r:id="', "", sub('"$', "", x)), character(1L))
  shown <- unname(by_id[order])

  hits <- vapply(shown, function(f) {
    rels <- paste0("ppt/slides/_rels/", sub("ppt/slides/", "", f), ".rels")
    if (!rels %in% z) {
      return(FALSE)
    }
    txt <- paste(readLines(unz(path, rels), warn = FALSE), collapse = "")
    grepl("\\.svg", txt)
  }, logical(1L))

  which(hits)
}

#' Replace the figure slides of a deck with rasterized ones
#'
#' Rebuilds each figure slide with `officer`, adding a new slide holding the PNG
#' and moving it into the position the figure already occupied, then dropping the
#' old one. Going through `add_slide()`/`move_slide()`/`remove_slide()` rather
#' than rewriting the media keeps the deck's own layout, master and title
#' placeholder intact, and keeps the surrounding slides where they are.
#'
#' @param path `character` path of the `.pptx` to rewrite in place
#' @param figures `list` of figure objects, in the order they appear in the deck
#' @param titles `character` slide titles, one per figure
#' @param width,height `numeric` figure size in inches
#' @param dpi `integer` resolution of the raster
#' @return The path of the deck
#' @noRd
replace_figure_slides <- function(path, figures, titles, width = 9, height = 5, dpi = 300L) {
  positions <- svg_slide_positions(path)
  if (!length(positions)) {
    return(path)
  }
  if (length(positions) != length(figures)) {
    stop(
      sprintf(
        "The deck holds %d figure slides but %d figures were given to redraw.",
        length(positions), length(figures)
      ),
      call. = FALSE
    )
  }

  ppt <- officer::read_pptx(path)
  summary <- officer::layout_summary(ppt)
  master <- summary$master[1L]
  layout <- if ("Title and Content" %in% summary$layout) {
    "Title and Content"
  } else {
    summary$layout[1L]
  }
  # Each figure slide is replaced in place, so the slide count holds steady.
  n <- length(grep("^ppt/slides/slide[0-9]+\\.xml$", utils::unzip(path, list = TRUE)$Name))

  for (k in seq_along(positions)) {
    png_file <- figure_png(figures[[k]], tempfile(fileext = ".png"), width, height, dpi)
    on.exit(unlink(png_file), add = TRUE)

    ppt <- officer::add_slide(ppt, layout = layout, master = master)
    ppt <- officer::ph_with(
      ppt, value = titles[[k]], location = officer::ph_location_type("title")
    )
    ppt <- officer::ph_with(
      ppt,
      value = officer::external_img(png_file, width = width, height = height),
      location = officer::ph_location_type("body"),
      use_loc_size = FALSE
    )
    ppt <- officer::move_slide(ppt, index = n + 1L, to = positions[k])
    ppt <- officer::remove_slide(ppt, index = positions[k] + 1L)
  }

  print(ppt, target = path)
  path
}
