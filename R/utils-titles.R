lt_titles <- function(lt, opts_titles) {
  lt |>
    addControl(
      opts_titles$title,
      position = paste0("top", opts_titles$title_align),
      className = "map-title"
    ) |>
    addControl(
      opts_titles$subtitle,
      position = paste0("top", opts_titles$subtitle_align),
      className = "map-subtitle"
    ) |>
    addControl(
      opts_titles$caption,
      position = paste0("bottom", opts_titles$caption_align),
      className = "map-caption"
    )
}
