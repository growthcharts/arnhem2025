plot_target <- function(target) {
  data <- target[["xyz"]]
  
  # add labels for legends and hovertext
  data <- add_labels(data)
  
  # create plot
  fig <- plot_ly(width = 1300, height = 800) %>%
    config(displayModBar = FALSE, displayLogo = FALSE) %>%
    layout(paper_bgcolor = "#F7F7F7F7",
           plot_bgcolor = "white",
           xaxis = xaxis,
           yaxis = yaxis,
           shapes = list(
             fillcolor = "#00AB6688",
             line = list(width = 0),
             opacity = 0.1,
             type = "rect",
             x0 = 0, x1 = 30,
             y0 = -2, y1 = 2
           ),
           legend = list(
             bgcolor = "transparent",
             x = 0.995,
             y = 1,
             xanchor = "right"
           )) |>
    add_trace(
      color = ~yname_label,
      colors = yname_colors,
      data = data,
      hoverinfo = 'text',
      line = list(width = 2.5),
      marker = list(size = 10),
      mode = "lines+markers",
      text = ~paste0('</br>', yname,
                     '</br>', round(y, digits = 1), ' ', yname_unit,
                     ' (', format(z, nsmall = 2),
                     '; P', pf, ')'),
      type = "scatter",
      x = ~age,
      y = ~z,
      xaxis = "x"
    )
  
  # define traces visible at startup
  visible <- list(hgt = TRUE, wgt = "legendonly", wfh = FALSE,
                  bmi = FALSE, hdc = "legendonly", dsc = "legendonly")
  fig <- set_trace_visibility(fig, visible)
  fig
}


add_labels <- function(data) {
  data <- data %>%
    mutate(yname = factor(.data$yname,
                          levels = c("hgt", "wgt", "wfh", "bmi", "hdc", "dsc")),
           yname_label = factor(.data$yname,
                                levels = c("hgt", "wgt", "wfh", "bmi", "hdc", "dsc"),
                                labels = c("Lengte",
                                           "Gewicht naar leeftijd",
                                           "Gewicht naar lengte",
                                           "Body mass index",
                                           "Hoofdomtrek",
                                           "D-score")),
           yname_unit = factor(.data$yname,
                               levels = c("hgt", "wgt", "wfh", "bmi", "hdc", "dsc"),
                               labels = c("cm", "kg", "kg", "kg/m^2", "cm", "D")),
           percentile = pnorm(.data$z) * 100,
           pf = ifelse(!is.na(.data$z) & (.data$z < -2 | .data$z > 2),
                       format(.data$percentile, digit = 1, nsmall = 1, trim = TRUE),
                       format(.data$percentile, digit = 1, nsmall = 0, trim = TRUE))) %>%
    group_by(.data$yname)
  return(data)
}

set_trace_visibility <- function(fig, visible) {
  fig <- plotly_build(fig)
  d <- plotly_data(fig)
  yname_lev <- levels(d$yname)
  label_lev <- levels(d$yname_label)
  for (i in 1L:length(fig$x$data)) {
    yname <- yname_lev[grep(fig$x$data[[i]]$name, label_lev)]
    fig$x$data[[i]]$visible <- visible[[yname]]
  }
  return(fig)
}
