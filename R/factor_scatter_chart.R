#' Use this function to create chart with a factor variable in y-axis,
#' a numeric variable in x-axis and an optional color variable
#'
#' @param  data.frame  plot_data        This is the data.frame which will be used for the plot
#' @param  Column      x_name           The column name of the variable in the x-axis, It has to be a numeric variable
#' @param  Column      y_name           The column name of the variable in the y-axis, It has to be a factor variable
#' @param  Column      color_name       The column name of the variable in the color axis, It has to be a factor variable
#' @param  string      static_color     If the colour_name is not specified, this color will be filled for the plot
#' @param  char_vactor highlight        A vector of values from y_name which will be highlighted. Optional, if not provided every y-axis variable will have same color
#' @param  Column      hover_name       The column name of the variable that will be present in the hovers, can also be a normal vector.
#' @param  num         plot_height      The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  num         plot_width       The height of the plot, If not provided the plot will take the whole area available in the UI
#' @param  boolean     show_legend      This will let you display or hide the grids in the x-axis, default shown
#' @param  boolean     show_x_axis_grid This will let you display or hide the grids in the x-axis, default hidden
#' @param  boolean     show_y_axis_grid This will let you display or hide the grids in the y-axis, default shown
#' @param  num         marker_size      This is the size of the markers in the scatter plot, defaults to marker_default_size from constants.R
#' @param  num         y_text_wrap      This will break the y-axis text by the number of characters specified, defaults to 60 characters
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
#' You can plot by just using a data.frame, x-axis and y-axis.
#' factor_scatter_chart(marks_data, marks, name)
#'
#' You can also change the color of the plot by specifying `static_color`
#' factor_scatter_chart(marks_data, marks, name, static_color = "#705bf5")
#'
#' You can pass a third variable to get a plot with the color axis
#' factor_scatter_chart(marks_data, marks, name, subject)
#'
#' @import plotly
#' @importFrom magrittr %>%
#' @importFrom stringr str_wrap str_replace_all
#' @export
factor_scatter_chart <- function(plot_data, x_name, y_name, color_name = NULL, static_color = "#4dbd5b",
                                 highlight = NULL, hover_name = NULL, plot_height = NULL, plot_width = NULL,
                                 show_legend = TRUE, show_x_axis_grid = FALSE, show_y_axis_grid = TRUE,
                                 marker_size = marker_default_size, y_text_wrap = 60,
                                 plot_title = NULL, x_axis_title = NULL, y_axis_title = NULL) {
  x_name <- rlang::enquo(x_name)
  x_value <- plot_data[[rlang::quo_name(x_name)]]
  x_min <- min(x_value) - sd(x_value)/2
  x_max <- max(x_value) + sd(x_value)/2
  y_name <- rlang::enquo(y_name)
  y_value <- str_wrap(plot_data[[rlang::quo_name(y_name)]], y_text_wrap) %>%
    str_replace_all("\n", "<br>")
  color_name <- rlang::enquo(color_name)
  hover_name <- rlang::enquo(hover_name)
  if (rlang::quo_is_null(color_name) & is.null(highlight)) {
    show_legend <- FALSE
    if (rlang::quo_is_null(hover_name)) {
      hover_value <- paste0(
        y_value, "<br><br><b>",
        x_value, "</b>"
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
      y = reorder(y_value, x_value),
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
      xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid, range = c(x_min, x_max)),
      yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
      legend = list(y = 0.5, yanchor = "center")
    )
    return(plot)
  }
  if (rlang::quo_is_null(color_name)) {
    color_value <- y_value
    colors_value <- ifelse(
      str_replace_all(unique(sort(y_value)), "<br>", " ") %in% highlight,
      static_color, colorspace::lighten(static_color, 0.8)
    )
    show_legend <- FALSE
  } else {
    color_value <- plot_data[[rlang::quo_name(color_name)]]
    colors_value <- discrete_color_palette[1:length(unique(color_value))]
  }
  if (rlang::quo_is_null(hover_name)) {
    if (is.null(plot_data[[rlang::quo_name(color_name)]])) {
      hover_value <- paste0(
        y_value, "<br><br><b>",
        x_value, "</b>"
      )
    } else {
      hover_value <- paste0(
        y_value, "<br><br>",
        color_value, ": <b>",
        x_value, "</b>"
      )
    }
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
    y = reorder(y_value, x_value),
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
    xaxis = list(title = x_axis_title, showgrid = show_x_axis_grid, range = c(x_min, x_max)),
    yaxis = list(title = y_axis_title, showgrid = show_y_axis_grid),
    legend = list(y = 0.5, yanchor = "center")
  )
  return(plot)
}

