lt_add_zoom_control <- function(lt, zoom_control_align) {
  lt |>
    htmlwidgets::onRender(
      glue::glue(
        "function(el, x) {{
          L.control.zoom({{ position: '{zoom_control_align}' }}).addTo(this);
        }}"
      )
    )
}
