#' Lollipop Chart
#'
#' Use this function to create a lollipop chart to compare a numeric variables with a factor variable
#'
#' @author Vedha Viyash
#'
#' @param  plot_data         This is the data.frame which will be used for the plot
#' @param  x_name            The column name of one of the the variable in the x-axis, It has to be a numeric variable
#' @param  y_name            The column name of the the variable in the y-axis, It has to be a factor variable
#' @param  line_color        string. This is the color of the line between the two points, defaults to a grey shade
#' @param  x_color           string. This is the color of the point made by x_name variable, defaults to a bluish color
#' @param  plot_height       num. The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  plot_width        num. The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  show_legend       boolean. This will let you display or hide the grids in the x-axis, default hidden
#' @param  show_x_axis_grid  boolean. This will let you display or hide the grids in the x-axis, default hidden
#' @param  show_y_axis_grid  boolean. This will let you display or hide the grids in the y-axis, default hidden
#' @param  marker_size       num. This is the size of the markers in the scatter plot, defaults to marker_default_size from constants.R
#' @param  plot_title        string. This is the title of the plot, defauts to NULL
#' @param  x_axis_title      string. This is the x-axis title of the plot, defaults to NULL
#' @param  y_axis_title      string. This is the y-axis title of the plot, defaults to NULL
#' @return                   Returns a plotly plot object which can be rendered as HTML
#'
#' @examples
#'
#' ## This will load two data.frames `marks_data` and `gender_school_earnings`
#' data("tidychartsdata")
#'
#' ## You can plot a lollipop chart using by just specifying data, x_name and y_name.
#' lollipop_chart(gender_school_earnings, Men, School)
#'
#' ## If you want to specify particular color, you can specify them to their respective arguments
#' lollipop_chart(gender_school_earnings, Men, School, line_color = "#afdeb5", x_color = "#4dbd5b")
#'
#' @import plotly
#' @importFrom magrittr %>%
#' @export
lollipop_chart <- function(plot_data, x_name, y_name, line_color = "#bdbdbd",
                           x_color = "#5884d6", plot_height = NULL, plot_width = NULL,
                           show_legend = FALSE, show_x_axis_grid = FALSE, show_y_axis_grid = FALSE,
                           marker_size = marker_default_size, plot_title = NULL,
                           x_axis_title = NULL, y_axis_title = NULL) {
  x_name <- rlang::enquo(x_name)
  x1_value <- plot_data[[rlang::quo_name(x_name)]]
  y_name <- rlang::enquo(y_name)
  y_value <- plot_data[[rlang::quo_name(y_name)]]
  y_value <- factor(
    y_value,
    levels = y_value[order(x1_value)]
  )
  plot_ly(plot_data, color = I(line_color)) %>%
    add_segments(
      x = 0, xend = x1_value,
      y = y_value, yend = y_value, showlegend = FALSE) %>%
    add_markers(
      x = x1_value, y = y_value,
      name = rlang::quo_name(x_name),
      color = I(x_color),
      marker = list(size = marker_size)
    ) %>%
    layout(
      title = plot_title, showlegend = show_legend,
      xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid),
      yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
      legend = list(y = 0.5, yanchor = "center")
    )
}

