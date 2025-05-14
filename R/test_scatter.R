library(plotly)

data <- cars

fig1 <- plot_ly(data, x = ~speed, y = ~dist, type = "scatter", 
                mode = "markers", marker = list(color = "transparent"),
                showlegend = FALSE) |> 
  layout(showlegend = FALSE,
         font = list(color = '#a2a2a2'),
         yaxis = list(
           title = "", 
           showgrid = TRUE,
           gridcolor = "green", 
           showline = FALSE, 
           showticklabels = TRUE,
           gridwidth = 2),
         yaxis2 = list(
           overlaying = "y",
           tickvals = seq(10, 110, 20), 
           ticktext = rep(" ", 6),
           showgrid = TRUE,
           gridwidth = 2),
         xaxis = list(
           title = "",
           zeroline = FALSE, 
           showline = TRUE,
           showticklabels = TRUE,
           tickangle = 45, 
           showgrid = FALSE)) |> 
  add_trace(x = ~speed, y = ~dist, mode = "markers", yaxis = "y2",
            marker = list(color = "blue"))

# using shapes
# https://stackoverflow.com/questions/54612142/reduce-number-of-gridlines-in-plotly-scatter-plots-with-log-scale-in-r-shiny
fig3 <- plot_ly(data, x = ~speed, y = ~dist, type = "scatter", 
                mode = "markers", showlegend = FALSE) |> 
  add_trace(x = ~speed, y = ~dist, mode = "markers", yaxis = "y2",
            marker = list(color = "blue")) |> 
  layout(showlegend = FALSE,
         font = list(color = '#a2a2a2'),
         yaxis = list(
           title = "", 
           showgrid = TRUE,
           gridcolor = "#a2a2a2", 
           showline = FALSE, 
           showticklabels = TRUE,
           gridwidth = 5),
         xaxis = list(
           title = "",
           zeroline = FALSE, 
           showline = TRUE,
           showticklabels = FALSE,
           tickangle = 45, 
           showgrid = FALSE),
         shapes = lapply(seq(10, 110, 20), line))


# ChatGPT
# Install and load the plotly package if not already installed
if (!require(plotly)) {
  install.packages("plotly")
  library(plotly)
}

# Create a sample data frame
df <- data.frame(
  x = 1:10,
  y = c(2, 4, 1, 6, 8, 9, 5, 7, 3, 10)
)

# Create a basic scatter plot
plot <- plot_ly(data = df, x = ~x, y = ~y, type = "scatter", mode = "lines")

# Create a sample data frame
df <- data.frame(
  x = 1:10,
  y = c(2, 4, 1, 6, 8, 9, 5, 7, 3, 10)
)

# Create a basic scatter plot
plot <- plot_ly(data = df, x = ~x, y = ~y, type = "scatter", mode = "lines")

# Define the positions and labels for minor ticks on the x-axis
minor_tickvals <- c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5)
minor_ticktext <- c("1.5", "2.5", "3.5", "4.5", "5.5", "6.5", "7.5", "8.5", "9.5")

# Customize the x-axis with minor gridlines
plot <- plot %>% 
  layout(xaxis = list(
    tickvals = minor_tickvals,
    ticktext = minor_ticktext,
    showgrid = TRUE,
    gridcolor = "lightgray",  # Color of major gridlines
    gridwidth = 5,            # Width of major gridlines
    showline = TRUE,
    linewidth = 3              # Width of x-axis line
  ))

# Show the plot
plot


# Create a sample data frame
df <- data.frame(
  x = 1:10,
  y = c(2, 4, 1, 6, 8, 9, 5, 7, 3, 10)
)

# Create a basic scatter plot
plot <- plot_ly(data = df, x = ~x, y = ~y, type = "scatter", mode = "lines")

# Define the positions and labels for minor ticks on the x-axis
minor_tickvals <- seq(1.5, 9.5, by = 1)  # Define minor tick positions
minor_ticktext <- seq(1.5, 9.5, by = 1)  # Define labels for minor ticks

# Customize the x-axis with both major and minor gridlines
plot <- plot %>% 
  layout(xaxis = list(
    tickvals = minor_tickvals,   # Major tick positions
    ticktext = minor_ticktext,   # Labels for major ticks
    dtick = 1,                   # Interval between major ticks
    showgrid = TRUE,             # Show major gridlines
    gridcolor = "lightgray",     # Color of major gridlines
    gridwidth = 2,               # Width of major gridlines
    showline = TRUE,             # Show x-axis line
    linewidth = 3,               # Width of x-axis line
    minorgridcount = 4,          # Number of minor gridlines per major interval
    minorticks = "outside"       # Place minor ticks outside the plot area
  ))

# Show the plot
plot

### Nieuwe poging

# Create a sample data frame
df <- data.frame(
  x = 1:10,
  y = c(2, 4, 1, 6, 8, 9, 5, 7, 3, 10)
)

# Create a basic scatter plot
plot <- plot_ly(data = df, x = ~x, y = ~y, type = "scatter", mode = "lines")

# Customize the x-axis with both major and minor gridlines
plot <- plot %>% 
  layout(xaxis = list(
    minor = list(
      dtick = 1,
      tick0 = 1.5,
      gridcolor = "green",
      showgrid = TRUE,
      gridwidth = 1),
    dtick = 1,                   # Interval between major ticks
    tick0 = 1,
    showgrid = TRUE,             # Show major gridlines
    gridcolor = "red",     # Color of major gridlines
    gridwidth = 2
    ))               # Width of major gridlines

# Show the plot
plot

