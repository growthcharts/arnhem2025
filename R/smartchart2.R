#' Draw curves on smart growth chart
#'
#' Horizontal growth chart, with amplitude and flags
#' @param target A list with elements `"psn"` and `"xyz"`, e.g. as produced by
#' `bdsreader::read_bds()`
#' @param visible A named list specifying legend entries, e.g. as
#' `visible = list(hgt = TRUE, hdc = "legendonly")`. The three choices are:
#' `TRUE` (curve and legend shown), `FALSE` (curve and legend not shown)
#' and `"legendonly"` (curve not shown, legend dimmed). Unspecified entries are
#' set to `FALSE` and hence not shown. The default (`NULL`) sets curve and legend
#' visibility according to the `design` parameter.
#' @param design Either "A" (0-15m), "B" (0-4y), "C" (0-21y), "F" (0-24m)
#' @param ratio height/width ratio of the figure size. The default is 3/4.
#' @param \dots Passed down to plotting functions
#' @return A `plotly` object
#' @examples
#' \dontrun{
#' fn <- system.file("extdata", "bds_v3.0", "smocc", "Laura_S.json", package = "jamesdemodata")
#' target <- bdsreader::read_bds(fn)
#' data <- target[["xyz"]]
#' fig <- greenband_plotly(data)
#' fig
#' fig <- smartchart2(target, visible = list(hgt = "legendonly", dsc = TRUE, wgt = FALSE))
#' fig
#' }
#' @export
greenband_plotly <- function(data,
                             visible = NULL,
                             design = c("A", "B", "C", "F"),
                             language = c("dutch", "english"),
                             ratio = 3/4,
                             ...) {
  # argument handling
  if (!is.data.frame(data)) stop("Argument `data` is not a data.frame")
  required <- c("age", "yname", "y", "z" )
  present <- hasName(data, required)
  if (any(!present)) stop("Columns not found: ", required[!present])
  design <- match.arg(design)
  language <- match.arg(language)
  if (is.null(visible)) {
    visible <- switch(
      design,
      "A" = list(hgt = TRUE, wgt = TRUE, wfh = "legendonly",
                 bmi = "legendonly", hdc = "legendonly", dsc = "legendonly"),
      "B" = list(hgt = TRUE, wgt = "legendonly", wfh = TRUE,
                 bmi = "legendonly", hdc = "legendonly", dsc = "legendonly"),
      "C" = list(hgt = TRUE, wgt = "legendonly", wfh = TRUE,
                 bmi = "legendonly", hdc = "legendonly", dsc = "legendonly"),
      "F" = list(hgt = TRUE, wgt = TRUE, wfh = "legendonly",
                 bmi = "legendonly", hdc = "legendonly", dsc = "legendonly"))
  } else {
    nn <- setdiff(c("hgt", "wgt", "wfh", "bmi", "hdc", "dsc"), names(visible))
    visible[nn] <- FALSE
  }
  
  # add labels for legends and hovertext
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
  
  # define plot components
  yname_colors <- c("#FF0000BB", "#0072B2", "#56B4E9", "#763A7E", "#33B882", "#E69F00")
  yaxis_range <- list(-4, 4)
  yaxis_title <- switch(language,
                        "dutch" = "Standaard Deviatie Score (SDS)",
                        "english" = "Standard Deviation Score (SDS)",
                        "Standard Deviation Score (SDS)")
  a <- list(
    xaxis = list(
      constrain = "domain",
      constraintoward = "left",
      dtick = 1/12,
      range = list(-0.5/12, 15.5/12),
      rangemode = "tozero",
      scaleanchor = "y",
      scaleratio = 8 / ((16/12) * ratio),
      tick0 = 0,
      ticklabelstep = 1/12,
      ticktext = as.character(0:36),
      tickvals = (0:36)/12,
      title = "Leeftijd (maanden)",
      zeroline = FALSE
    )
  )
  b <- list(
    xaxis = list(
      constrain = "domain",
      constraintoward = "left",
      dtick = 0.25,
      range = list(-1/12, 4 + 1/12),
      rangemode = "tozero",
      scaleanchor = "y",
      scaleratio = 8 / ((4 + 1/6) *  ratio),
      tick0 = 0,
      ticklabelstep = 1,
      ticktext = as.character(paste0(rep(0:7, each = 4), rep(c("", "_3", "_6", "_9"), 8))),
      tickvals = seq(0, 7.75, 0.25),
      title = "Leeftijd (jaren_maanden)",
      zeroline = FALSE
    )
  )
  c <- list(
    xaxis = list(
      constrain = "domain",
      constraintoward = "left",
      dtick = 1,
      range = list(-0.5, 21.5),
      rangemode = "tozero",
      scaleanchor = "y",
      scaleratio = 8 / (22 * ratio),
      tick0 = 0,
      ticklabelstep = 2,
      title = "Leeftijd (jaren)",
      zeroline = FALSE
    )
  )
  f <- list(
    xaxis = list(
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE, 
      gridwidth = 1,
      range = list(-1/12, 31/12),
      rangemode = "tozero",
      scaleanchor = "y",
      scaleratio = 8 / ((32/12) * ratio),
      tick0 = 0,
      ticktext = as.character(seq(-9, 48, 3)),
      tickvals = seq(-9, 48, 3)/12,
      title = "Leeftijd (maanden)",
      zeroline = FALSE),
    # xaxis2 = list(
    #   overlaying = "x",
    #   gridcolor = "red",
    #   showgrid = TRUE,
    #   showline = FALSE,
    #   showticklabels = TRUE, 
    #   gridwidth = 2,
    #   range = list(-1/12, 31/12),
    #   rangemode = "tozero",
    #   scaleanchor = "y",
    #   scaleratio = 8 / ((32/12) * ratio),
    #   tick0 = 0,
    #   ticktext = as.character(seq(-9, 48, 3)),
    #   tickvals = seq(-9, 48, 3)/12,
    #   title = "Leeftijd (maanden)",
    #   zeroline = FALSE)
    # 
    xaxis2 = list(
      range = list(-1/12, 31/12),
      gridcolor = "blue",
      overlaying = "x",
      showgrid = TRUE,
      tickmode = "array",
      tickvals = to_list(for (i in ((-9:48)/12))  if (i %% 0.25) i),
      gridwidth = 1,
      showticklabels = FALSE)
    )
    # ,
    # xaxis2 = list( 
    #   gridcolor = "#a2a2a2",
    #   overlaying = "x",
    #   tickvals = to_list(for (i in ((-9:48)/12))  if (i %% 0.25) i), 
    #   showticklabels = FALSE
    # )
  # )
  
  browser()
  
  # create plot
  fig <- plot_ly() %>%
    config(toImageButtonOptions = list(
      format = 'svg',
      filename = paste(format(Sys.time(), "%Y%m%d-%H%M%S")),
      height = NULL,
      width = NULL),
      modeBarButtonsToRemove = c('select', 'lasso2d', 'zoomIn', 'zoomOut', 'autoScale'),
      displaylogo = FALSE,
      locale = "nl") %>%
    layout(paper_bgcolor = "#F7F7F7F7",
           plot_bgcolor = "#FFFFFFFF",
           xaxis = switch(
             design,
             "A" = a$xaxis,
             "B" = b$xaxis,
             "C" = c$xaxis,
             "F" = f$xaxis
           ),
           yaxis = list(
             constrain = "domain",
             dtick = 1,
             range = yaxis_range,
             scaleanchor = "x",
             scaleratio = ratio,
             tick0 = 0,
             ticklabelstep = 2,
             title = yaxis_title,
             zeroline = TRUE,
             zerolinecolor = "#00AB6688",
             zerolinewidth = 2
           ),
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
           ),
           updatemenus = list(
             list(type = "dropdown",
                  x = 0.03, xanchor = "left",
                  y = 0.023, yanchor = "bottom",
                  direction = "up",
                  active = switch(design, "A" = 0, "B" = 1, "C" = 2, "F" = 3),
                  buttons = list(
                    list(label = "0-15 maanden",
                         method = "update",
                         args = list("A", a)),
                    list(label = "0-4 jaar",
                         method = "update",
                         args = list("B", b)),
                    list(label = "0-21 jaar",
                         method = "update",
                         args = list("C", c)),
                    list(label = "0-30 maanden",
                         method = "update",
                         args = list("F", f))
                  )
             )
           )
    ) |>
    add_trace(
      data = data,
      x = ~age,
      y = ~z,
      color = ~yname_label,
      colors = yname_colors,
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 10),
      line = list(width = 2.5),
      hoverinfo = 'text',
      text = ~paste0('</br>', yname,
                     '</br>', round(y, digits = 1), ' ', yname_unit,
                     ' (', format(z, nsmall = 2),
                     '; P', pf, ')'),
      xaxis = "x"
    )
  # xaxis2 = list(
  #   gridcolor = "#a2a2a2",
  #   overlaying = "x",
  #   tickvals = to_list(for (i in ((-9:48)/12))  if (i %% 0.25) i), 
  #   showticklabels = FALSE
  # ),
  
  # # add minor gridlines
  # fig <- fig |> 
  #   add_trace(x = ~age,
  #             y = ~z,
  #             xaxis = "x2",
  #             marker = list(color = 'rgba(0,0,0,0)'))
  
  # initialize trace visibility
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

## https://stackoverflow.com/questions/52959023/using-plotly-in-r-update-visibility-of-shapes-and-plots
