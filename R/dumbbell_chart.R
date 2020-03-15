#' Use this function to create a dumbbell chart to compare two numeric variables along a factor variable
#' @param  data.frame  plot_data        This is the data.frame which will be used for the plot
#' @param  Column      x1_name          The column name of one of the the variable in the x-axis, It has to be a numeric variable
#' @param  Column      x2_name          The column name of the other variable in the x-axis, It has to be a numeric variable
#' @param  Column      y_name           The column name of the the variable in the y-axis, It has to be a factor variable
#' @param  string      line_color       This is the color of the line between the two points, defaults to a grey shade
#' @param  string      x1_color         This is the color of the point made by x1_name variable, defaults to a bluish color
#' @param  string      x2_color         This is the color of the point made by x2_name variable, defaults to a version of lime green
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
#' This will load two data.frames `marks_data` and `gender_school_earnings`
#' data("tidychartsdata")
#'
#' You can plot a dumbbell chart using by just specifying data, x1_name, x2_name and y_name.
#' dumbbell_chart(gender_school_earnings, Men, Women, School)
#'
#' If you want to specify particular color, you can specify them to their respective arguments
#' dumbbell_chart(gender_school_earnings, Men, Women, School, line_color = "#6ba1e8", x1_color = "#4c7ced", x2_color = "#a7d1eb")
#'
#' @import plotly
#' @importFrom magrittr %>%
#' @export
dumbbell_chart <- function(plot_data, x1_name, x2_name, y_name, line_color = "#bdbdbd",
                           x1_color = "#5884d6", x2_color = "#bbe64e",
                           plot_height = NULL, plot_width = NULL, show_legend = TRUE,
                           show_x_axis_grid = FALSE, show_y_axis_grid = FALSE, marker_size = marker_default_size,
                           plot_title = NULL, x_axis_title = NULL, y_axis_title = NULL) {
  x1_name <- rlang::enquo(x1_name)
  x1_value <- plot_data[[rlang::quo_name(x1_name)]]
  x2_name <- rlang::enquo(x2_name)
  x2_value <- plot_data[[rlang::quo_name(x2_name)]]
  y_name <- rlang::enquo(y_name)
  y_value <- plot_data[[rlang::quo_name(y_name)]]
  if (mean(x1_value) > mean(x2_value)) {
    order_by_vector <- x1_value
  } else {
    order_by_vector <- x2_value
  }
  y_value <- factor(
    y_value,
    levels = y_value[order(order_by_vector)]
  )
  plot_ly(plot_data, color = I(line_color)) %>%
    add_segments(
      x = x2_value, xend = x1_value,
      y = y_value, yend = y_value, showlegend = FALSE) %>%
    add_markers(
      x = x1_value, y = y_value,
      name = rlang::quo_name(x1_name),
      color = I(x1_color),
      marker = list(size = marker_size)
    ) %>%
    add_markers(
      x = x2_value, y = y_value,
      name = rlang::quo_name(x2_name),
      color = I(x2_color),
      marker = list(size = marker_size)
    ) %>%
    layout(
      title = plot_title, showlegend = show_legend,
      xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid),
      yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
      legend = list(y = 0.5, yanchor = "center")
    )
}

