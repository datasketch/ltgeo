lt_titles <- function(lt, opts_titles) {
  if (opts_titles$title_align == "center") {
    opts_titles$title_align <- "left"
  }

  if (opts_titles$subtitle_align == "center") {
    opts_titles$title_align <- "left"
  }

  if (opts_titles$caption_align == "center") {
    opts_titles$title_align <- "right"
  }

  lt |>
    addControl(
      html = make_heading("title", opts_titles),
      position = paste0("top", opts_titles$title_align),
      className = "map-title"
    ) |>
    addControl(
      html = make_heading("subtitle", opts_titles),
      position = paste0("top", opts_titles$subtitle_align),
      className = "map-subtitle"
    ) |>
    addControl(
      html = make_heading("caption", opts_titles),
      position = paste0("bottom", opts_titles$caption_align),
      className = "map-caption"
    )
}

make_heading <- function(heading, opts) {
  color <- opts[[paste0(heading, "_color")]] %||% opts$text_color
  font_family <- opts[[paste0(heading, "_family")]] %||% opts$text_family
  font_size <- opts[[paste0(heading, "_size")]]
  font_weight <- opts[[paste0(heading, "_weight")]]

  glue::glue(
    "<div style = 'font-family: {font_family}; font-size: {font_size}px; font-weight: {font_weight}; color: {color};'>",
    "{opts[[heading]]}",
    "</div>"
  )
}
