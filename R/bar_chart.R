#' Bar Chart
#'
#' Use this function to create bar chart using with 2 or 3 variables and at least one numeric variable.
#'
#' @author Vedha Viyash
#'
#' @param  plot_data                   The data which will be used for the plot
#' @param  x_name                      The column name of the variable in the x-axis, It can be a numeric or factor variable
#' @param  y_name                      The column name of the variable in the y-axis, It can be a factor or numeric variable
#' @param  color_name                  The column name of the variable in the color axis, It has to be a factor variable
#' @param  static_color                string. If the colour_name is not specified, this color will be filled for the plot
#' @param  border_line_color           string. The color of the border line around the bar, default is invisible
#' @param  border_line_width           num. The width of the border line
#' @param  stack                       boolean. This will convert your coloured plot to stacked bar instead of grouped bar
#' @param  sort_asc                    boolean. This will let you sort the plot ascending, the plot is sorted descending by default
#' @param  highlight                   char_vactor. A vector of values from y_name which will be highlighted. Optional, if not provided every y-axis variable will have same color
#' @param  plot_height                 num. The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  plot_width                  num. The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  show_legend                 boolean. This will let you display or hide the grids in the x-axis, default shown
#' @param  show_x_axis_grid            boolean. This will let you display or hide the grids in the x-axis, default hidden
#' @param  show_y_axis_grid            boolean. This will let you display or hide the grids in the y-axis, default shown
#' @param  sort_colors_alphabetically  boolean. This will sort the color legends alphabetically
#' @param  plot_title                  string. This is the title of the plot, defauts to NULL
#' @param  x_axis_title                string. This is the x-axis title of the plot, defaults to NULL
#' @param  y_axis_title                string. This is the y-axis title of the plot, defaults to NULL
#' @return                             Returns a plotly plot object which can be rendered as HTML
#'
#' @examples
#'
#' ## Creating a sample data for bar chart without color axis
#' library(dplyr)
#' data("tidychartsdata")
#' plot_data <- marks_data %>% group_by(name) %>% summarise(marks = mean(marks))
#'
#' ## All you need to plot a bar chart is the data, x_name and y_name
#' bar_chart(plot_data, name, marks)
#'
#' ## You can highlight values from your factor axis by specifying it as highlight parameter         . 
#' bar_chart(plot_data, name, marks, highlight = c("Danny", "Jon", "Tyrion"))
#'
#' ## To get a horizontal bar chart all you need to do is to pass the x and y variables accordingly
#' bar_chart(plot_data, marks, name)
#'
#' ## You can highlight variables by passing highlight and change the color using static_color
#' bar_chart(
#'   plot_data, marks, name,
#'   highlight = c("Arya", "Bran", "Sansa"), static_color = "#5884d6"
#' )
#'
#' ## You can pass a factor variable in color_name, this will add a grouped bar chart
#' bar_chart(marks_data, name, marks, subject)
#'
#' ## To flip the axis all you have to do is to interchange the x and y names
#' bar_chart(marks_data, marks, name, subject)
#'
#' ## If you'd like to have a stacked bar, just pass stack = TRUE
#' bar_chart(marks_data, name, marks, subject, stack = TRUE)
#'
#' ## stack = TRUE also works for horizontal grouped bar charts
#' bar_chart(marks_data, marks, name, subject, stack = TRUE)
#'
#' @import plotly
#' @importFrom magrittr %>%
#' @importFrom dplyr desc
#' @export
bar_chart <- function(plot_data, x_name, y_name, color_name = NULL, static_color = "#4dbd5b",
                      border_line_color = "#ffffff00", border_line_width = 2, stack = FALSE, sort_asc = FALSE,
                      highlight = NULL, plot_height = NULL, plot_width = NULL, show_legend = TRUE,
                      show_x_axis_grid = FALSE, show_y_axis_grid = FALSE, sort_colors_alphabetically = FALSE,
                      plot_title = "", x_axis_title = NULL, y_axis_title = NULL) {
  x_name <- rlang::enquo(x_name)
  y_name <- rlang::enquo(y_name)
  if (is.numeric(plot_data[[rlang::quo_name(x_name)]])) {
    x_value <- plot_data[[rlang::quo_name(x_name)]]
    if (sort_asc) {
      y_value <- stats::reorder(
        plot_data[[rlang::quo_name(y_name)]],
        desc(x_value)
      )
    } else {
      y_value <- stats::reorder(
        plot_data[[rlang::quo_name(y_name)]],
        x_value
      )
    }
  } else {
    y_value <- plot_data[[rlang::quo_name(y_name)]]
    if (sort_asc) {
      x_value <- stats::reorder(
        plot_data[[rlang::quo_name(x_name)]],
        y_value
      )
    } else {
      x_value <- stats::reorder(
        plot_data[[rlang::quo_name(x_name)]],
        desc(y_value)
      )
    }
  }
  if (rlang::quo_is_null(rlang::enquo(color_name))) {
    if (is.null(highlight)) {
      color_value <- static_color
    } else {
      if (is.numeric(y_value)) {
        color_value <- ifelse(
          x_value %in% highlight,
          static_color, colorspace::lighten(static_color, 0.8)
        )
      } else {
        color_value <- ifelse(
          y_value %in% highlight,
          static_color, colorspace::lighten(static_color, 0.8)
        )
      }
    }
    show_legend <- FALSE
    plot <- plot_ly(
      plot_data,
      x = x_value,
      y = y_value,
      type = "bar",
      height = plot_height,
      width = plot_width,
      marker = list(
        color = color_value,
        line = list(color = border_line_color, width = border_line_width)
      )
    ) %>% layout(
      title = plot_title, showlegend = show_legend,
      xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid),
      yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid)
    )
    return(plot)
  } else {
    color_name <- rlang::enquo(color_name)
    if (sort_colors_alphabetically) {
      color_value <- plot_data[[rlang::quo_name(color_name)]]
    } else {
      if (is.numeric(x_value)) {
        color_value <- stats::reorder(
          plot_data[[rlang::quo_name(color_name)]],
          x_value
        )
      } else {
        color_value <- stats::reorder(
          plot_data[[rlang::quo_name(color_name)]],
          desc(y_value)
        )
      }
    }
    colors_value <- discrete_color_palette[1:length(unique(color_value))]
    chart_mode <- ifelse(stack, "stack", "group")
    plot <- plot_ly(
      plot_data,
      x = x_value,
      y = y_value,
      color = color_value,
      colors = colors_value,
      type = "bar",
      height = plot_height,
      width = plot_width,
      marker = list(line = list(color = border_line_color, width = border_line_width))
    ) %>% layout(
      title = plot_title, showlegend = show_legend, barmode = chart_mode,
      xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid),
      yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
      legend = list(y = 0.5, yanchor = "center")
    )
    return(plot)
  }
}

