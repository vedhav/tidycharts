#' Use this function to create scatter plot with both x-axis and y-axis containing numeric variables
#' and with an optional color variable which is a factor variable
#'
#' @param  data.frame  plot_data        This is the data.frame which will be used for the plot
#' @param  Column      x_name           The column name of the variable in the x-axis, It has to be a numeric variable
#' @param  Column      y_name           The column name of the variable in the y-axis, It has to be a numeric variable
#' @param  Column      color_name       The column name of the variable in the color axis, It has to be a factor variable
#' @param  string      static_color     If the colour_name is not specified, this color will be filled for the plot
#' @param  Column      hover_name       The column name of the variable that will be present in the hovers, can also be a normal vector.
#' @param  num         plot_height      The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  num         plot_width       The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  boolean     show_legend      This will let you display or hide the grids in the x-axis, default shown
#' @param  boolean     show_x_axis_grid This will let you display or hide the grids in the x-axis, default hidden
#' @param  boolean     show_y_axis_grid This will let you display or hide the grids in the y-axis, default hidden
#' @param  num         marker_size      This is the size of the markers in the scatter plot, defaults to marker_default_size from constants.R
#' @param  string      plot_title       This is the title of the plot, defauts to NULL
#' @param  string      x_axis_title     This is the x-axis title of the plot, defaults to NULL
#' @param  string      y_axis_title     This is the y-axis title of the plot, defaults to NULL
#' @return plot                         The plotly plot object as plot which can be viewed in HTML
#'
#' @examples
#'
#' You can plot by just using a data.frame, x-axis and y-axis.
#' numeric_scatter_chart(iris, Sepal.Length, Petal.Length)
#'
#' You can also change the color of the plot by specifying `static_color`
#' numeric_scatter_chart(iris, Sepal.Length, Petal.Length, static_color = "#f55b96")
#'
#' You can pass a third variable to get a plot with the color axis
#' numeric_scatter_chart(iris, Sepal.Length, Petal.Length, color_name = Species)
#'
#' @import plotly
#' @importFrom magrittr %>%
#' @export
numeric_scatter_chart <- function(plot_data, x_name, y_name, color_name = NULL, static_color = "#4dbd5b",
                                  hover_name = NULL, plot_height = NULL, plot_width = NULL,
                                  show_legend = TRUE, show_x_axis_grid = FALSE, show_y_axis_grid = FALSE,
                                  marker_size = marker_default_size, plot_title = NULL,
                                  x_axis_title = NULL, y_axis_title = NULL) {
  x_name <- rlang::enquo(x_name)
  x_value <- plot_data[[rlang::quo_name(x_name)]]
  x_min <- min(x_value) - sd(x_value)/2
  x_max <- max(x_value) + sd(x_value)/2
  y_name <- rlang::enquo(y_name)
  y_value <- plot_data[[rlang::quo_name(y_name)]]
  color_name <- rlang::enquo(color_name)
  hover_name <- rlang::enquo(hover_name)
  if (rlang::quo_is_null(color_name)) {
    show_legend <- FALSE
    if (rlang::quo_is_null(hover_name)) {
      hover_value <- paste0(x_value, ", ", y_value)
    } else {
      hover_name_in_data <- plot_data[[rlang::quo_name(hover_name)]]
      if (is.null(hover_name_in_data)) {
        hover_value <- hover_name
      } else {
        hover_value <- hover_name_in_data
      }
    }
    plot <- plot_ly(
      plot_data,
      x = x_value,
      y = y_value,
      color = static_color,
      colors = static_color,
      type = 'scatter',
      height = plot_height,
      width = plot_width,
      mode = 'markers',
      marker = list(size = marker_size),
      hoverinfo = 'text',
      hovertext = hover_value,
      textposition = 'top center',
      cliponaxis = FALSE
    ) %>% layout(
      title = plot_title, showlegend = show_legend,
      xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid),
      yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
      legend = list(y = 0.5, yanchor = "center")
    )
    return(plot)
  }
  color_value <- plot_data[[rlang::quo_name(color_name)]]
  colors_value <- discrete_color_palette[1:length(unique(color_value))]
  if (rlang::quo_is_null(hover_name)) {
    hover_value <- paste0(
      "X: ", x_value, "<br>Y: ", y_value,
      "<br>Color: ", color_value
    )
  } else {
    hover_name_in_data <- plot_data[[rlang::quo_name(hover_name)]]
    if (is.null(hover_name_in_data)) {
      hover_value <- hover_name
    } else {
      hover_value <- hover_name_in_data
    }
  }
  plot <- plot_ly(
    plot_data,
    x = x_value,
    y = y_value,
    color = color_value,
    colors = colors_value,
    type = 'scatter',
    height = plot_height,
    width = plot_width,
    mode = 'markers',
    marker = list(size = marker_size),
    hoverinfo = 'text',
    hovertext = hover_value,
    textposition = 'top center',
    cliponaxis = FALSE
  ) %>% layout(
    title = plot_title, showlegend = show_legend,
    xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid),
    yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
    legend = list(y = 0.5, yanchor = "center")
  )
  return(plot)
}

