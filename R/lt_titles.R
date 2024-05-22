

lt_titles <- function(map, opts) {

  text_family <- opts$text_family
  title_color <- opts$title_color
  title_size <- opts$title_size
  title <- ifelse(opts$title == "NA", "", opts$title)
  subtitle_size <- opts$subtitle_size
  subtitle_color <- opts$subtitle_color
  subtitle <- ifelse(opts$subtitle == "NA", "", opts$subtitle)
  caption_color <- opts$caption_color
  caption_size <- opts$caption_size
  caption <- ifelse(opts$caption == "NA", "", opts$caption)

  css <- "
      .leaflet-map-pane {
        margin-top: 30px;
      }
      .leaflet-control-zoom {
        margin-top: 30px;
      }
      .leaflet-control.map-titles {
        transform: translate(-50%,0%);
        position: fixed !important;
        left: 50%;
        text-align: center;
        padding-left: 10px;
        padding-right: 10px;
        background: rgba(255,255,255,0.1);
      }
      .leaflet-control.map-titles h1{
              font-size: {{title_size}}px;
              color: {{title_color}};
              margin: 0;
      }
            .leaflet-control.map-titles h2{
              font-size: {{subtitle_size}}px;
              color: {{subtitle_color}};
              margin: 0;
      }
  "

  title_styles <- htmltools::tags$style(
    htmltools::HTML(
      glue::glue(css, .open = "{{", .close = "}}")
    )
  )
  titles_html <- htmltools::div(title_styles,
                                htmltools::h1(title),
                                htmltools::h2(subtitle)
  )
  caption <- glue::glue("<p style='margin-bottom:0px;font-family:{text_family};
             color:{caption_color};font-size:{caption_size}px;'>{caption}</p>")

  # legend_title <- HTML(paste0("<p style='font-family:", opts$text_family,
  #                             ';color:', opts$legend_color,
  #                             ';font-size:', opts$legend_size,"px;'>",
  #                             opts$title$legend_title %||% "","</p>"))

  map |>
    leaflet::addControl(titles_html,
                        position = "topleft",
                        className="map-titles") |>
    leaflet::addControl(htmltools::HTML(caption),
                        position = "bottomleft",
                        className="map-caption")

}
