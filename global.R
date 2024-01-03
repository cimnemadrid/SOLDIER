library(magrittr)

relative_influence <- NULL
prediction <- NULL

#------------------------------------------------------------------------------#
#-----------------------------Draw plot functions------------------------------#
#------------------------------------------------------------------------------#

# Auxiliary function to plot a vertical line in a plot_ly plot
vline <- function(
  x = 0,
  y0 = 0,
  y1 = 1,
  color = "grey",
  dash = "dot",
  width = 2
) {
  list(
    type = "line",
    x0 = x,
    x1 = x,
    y0 = y0,
    y1 = y1,
    yref = "paper",
    line = list(color = color, dash = dash, width = width)
  )
}

# Function to calculate time series plot
generate_time_plot <- function(
  data_sort,
  wid,
  vars,
  y_labels,
  vars2,
  y_labels2,
  colours2
) {
  set_color <- RColorBrewer::brewer.pal(8, "Dark2")
  back_color <- "white"

  if (colours2) {
    set_color <- rainbow(8)
    back_color <- "darkgrey"
  }

  date <- format(zoo::index(data_sort), format = "%Y-%m-%d %H:%M:%OS")
  graph_data <- as.data.frame(data_sort)

  time_plot <- plotly::plot_ly()

  if (length(vars) == 1) {
    time_plot <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = graph_data[, c(vars)],
      type = "scatter",
      mode = "lines+markers",
      connectgaps = FALSE,
      p = time_plot,
      line = list(color = set_color[1], width = 3),
      marker = list(color = set_color[1], symbol = "circle", size = 4),
      name = vars,
      hovertemplate = "(%{x}, %{y:.2f})"
    )
  } else {
    for (i in seq_along(graph_data[, c(vars)])) {
      time_plot <- plotly::add_trace(
        data = graph_data,
        x = date,
        y = graph_data[, c(vars)][[i]],
        type = "scatter",
        mode = "lines+markers",
        connectgaps = FALSE,
        p = time_plot,
        line = list(color = set_color[i], width = 3),
        marker = list(color = set_color[i], symbol = "circle", size = 4),
        name = vars[[i]],
        hovertemplate = "(%{x}, %{y:.2f})"
      )
    }
  }

  # Graph for right variables
  if (length(vars2) > 0) { # Add right variables
    secondary_y <- list(
      overlaying = "y",
      side = "right",
      title = y_labels2,
      zeroline = FALSE,
      showgrid = TRUE,
      titlefont = list(size = 18),
      tickfont = list(size = 18),
      mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
      ticks = "outside",
      showline = TRUE,  # Show the x-axis line
      linewidth = 1,    # Line width
      linecolor = "black"  # Line color
    )

    if (length(vars2) == 1) {
      time_plot <- plotly::add_trace(
        data = graph_data,
        x = date,
        y = graph_data[, c(vars2)],
        yaxis = "y2",
        type = "scatter",
        mode = "lines+markers",
        connectgaps = FALSE,
        p = time_plot,
        line = list(color = set_color[1 + length(vars)], width = 3),
        marker = list(
          color = set_color[1 + length(vars)],
          symbol = "circle",
          size = 4
        ),
        name = vars2,
        hovertemplate = "(%{x}, %{y:.2f})"
      )
    } else {
      for (j in seq_along(vars2)) {
        time_plot <- plotly::add_trace(
          data = graph_data,
          x = date,
          y = graph_data[, c(vars2)][[j]],
          yaxis = "y2",
          type = "scatter",
          mode = "lines+markers",
          connectgaps = FALSE,
          p = time_plot,
          line = list(color = set_color[j + length(vars)], width = 3),
          marker = list(
            color = set_color[j + length(vars)],
            symbol = "circle",
            size = 4
          ),
          name = vars2[[j]],
          hovertemplate = "(%{x}, %{y:.2f})"
        )
      }
    }

    time_plot <- time_plot %>%
    plotly::layout(
      plot_bgcolor = back_color,
      yaxis2 = secondary_y,
      xaxis = list(
        title = "Date",
        type = "date",
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis = list(
        title = y_labels,
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      legend = list(
        font = list(size = 14),
        x = 1.05
      )
    )
  } else {
    time_plot <- time_plot %>%
    plotly::layout(
      plot_bgcolor = back_color,
      xaxis = list(
        title = "Date",
        type = "date",
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis = list(
        title = y_labels,
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      legend = list(
        font = list(size = 14)
      )
    )
  }

  return(time_plot)
}

# Function to calculate time series plot for observed and predicted data
generate_time_plot_prediction <- function(
  data_sort,
  results,
  start_train,
  end_train,
  end_test,
  wid,
  colours2,
  confidence_interval
) {

  set_color <- RColorBrewer::brewer.pal(8, "Dark2")
  back_color <- "white"

  if (colours2) {
    set_color <- rainbow(8)
    back_color <- "darkgrey"
  }

  date <- format(zoo::index(data_sort), format = "%Y-%m-%d %H:%M:%OS")
  graph_data <- as.data.frame(data_sort)

  min_y <- min(
    min(graph_data[, c("Observation")]),
    min(graph_data[, c("Prediction")])
  )

  time_plot_pred <- plotly::plot_ly()

  time_plot_pred <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = graph_data[, c("Observation")],
    type = "scatter",
    mode = "lines+markers",
    connectgaps = FALSE,
    p = time_plot_pred,
    line = list(color = set_color[1], width = 2),
    marker = list(color = set_color[1], size = 5),
    name = "Observation",
    layout,
    hovertemplate = "(%{x}, %{y:.2f})"
  )

  time_plot_pred <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = graph_data[, c("Prediction")],
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    p = time_plot_pred,
    line = list(color = set_color[2], width = 2, dash = "dash"),
    name = "Prediction",
    hovertemplate = "(%{x}, %{y:.2f})"
  )

  if (confidence_interval) {
    # Calculate the upper and lower bounds of the confidence interval
    sd_train_error <- sd(results$residual_train)
    upper_bound_2 <- graph_data$Prediction + 2 * sd_train_error
    lower_bound_2 <- graph_data$Prediction - 2 * sd_train_error

    upper_bound_3 <- graph_data$Prediction + 3 * sd_train_error
    lower_bound_3 <- graph_data$Prediction - 3 * sd_train_error

    # Add the transparent confidence interval traces
    time_plot_pred <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = upper_bound_2,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_pred,
      line = list(color = set_color[2], width = 0),
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )

    time_plot_pred <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = lower_bound_2,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_pred,
      line = list(color = set_color[2], width = 0),
      fill = "tonexty",  # Fill the area between the lines
      fillcolor = "rgba(0,0,0,0.2)",  # Transparent fill color
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )

    time_plot_pred <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = upper_bound_3,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_pred,
      line = list(color = set_color[2], width = 0),
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )

    time_plot_pred <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = lower_bound_3,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_pred,
      line = list(color = set_color[2], width = 0),
      fill = "tonexty",  # Fill the area between the lines
      fillcolor = "rgba(0,0,0,0.2)",  # Transparent fill color
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )
  }

  time_plot_pred <- time_plot_pred %>%
  plotly::layout(
    xaxis = list(type = "date"),
    shapes = list(
      vline(x = format(start_train, format = "%Y-%m-%d %H:%M:%OS")),
      vline(x = format(end_train, format = "%Y-%m-%d %H:%M:%OS")),
      vline(x = format(end_test, format = "%Y-%m-%d %H:%M:%OS"))
    )
  ) %>%
  plotly::add_annotations(
    showlegend = FALSE,
    x = c(format(start_train, format = "%Y-%m-%d %H:%M:%OS"),
          format(end_train, format = "%Y-%m-%d %H:%M:%OS"),
          format(end_test, format = "%Y-%m-%d %H:%M:%OS")),
    y = c(min_y, min_y),
    xref = "x",
    yref = "y",
    text = c("Start of training", "End of training", "End of testing"),
    xanchor = "right",
    showarrow = FALSE,
    textangle = 270
  )

  min_y_err <- min(graph_data[, c("Error")])

  time_plot_err <- plotly::plot_ly()

  time_plot_err <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = graph_data[, c("Error")],
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    p = time_plot_err,
    line = list(color = set_color[3], width = 2),
    showlegend = FALSE,
    hovertemplate = "(%{x}, %{y:.2f})"
  )

  if (confidence_interval) {
    # Calculate the upper and lower bounds of the confidence interval
    upper_bound_err_2 <- + 2 * sd_train_error
    lower_bound_err_2 <- - 2 * sd_train_error

    upper_bound_err_3 <- + 3 * sd_train_error
    lower_bound_err_3 <- - 3 * sd_train_error

    # Add the transparent confidence interval trace
    time_plot_err <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = upper_bound_err_2,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_err,
      line = list(color = set_color[2], width = 0),
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )

    time_plot_err <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = lower_bound_err_2,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_err,
      line = list(color = set_color[2], width = 0),
      fill = "tonexty",  # Fill the area between the lines
      fillcolor = "rgba(0,0,0,0.2)",  # Transparent fill color
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )

    time_plot_err <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = upper_bound_err_3,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_err,
      line = list(color = set_color[2], width = 0),
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )

    time_plot_err <- plotly::add_trace(
      data = graph_data,
      x = date,
      y = lower_bound_err_3,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      p = time_plot_err,
      line = list(color = set_color[2], width = 0),
      fill = "tonexty",  # Fill the area between the lines
      fillcolor = "rgba(0,0,0,0.2)",  # Transparent fill color
      showlegend = FALSE,
      hovertemplate = "(%{x}, %{y:.2f}, <extra></extra>)"
    )
  }

  time_plot_err <- time_plot_err %>%
  plotly::layout(
    xaxis = list(type = "date"),
    shapes = list(
      vline(x = format(start_train, format = "%Y-%m-%d %H:%M:%OS")),
      vline(x = format(end_train, format = "%Y-%m-%d %H:%M:%OS")),
      vline(x = format(end_test, format = "%Y-%m-%d %H:%M:%OS"))
    )
  ) %>%
  plotly::add_annotations(
    showlegend = FALSE,
    x = c(format(start_train, format = "%Y-%m-%d %H:%M:%OS"),
          format(end_train, format = "%Y-%m-%d %H:%M:%OS"),
          format(end_test, format = "%Y-%m-%d %H:%M:%OS")),
    y = c(min_y_err, min_y_err),
    xref = "x",
    yref = "y",
    text = c("Start of training", "End of training", "End of testing"),
    xanchor = "right",
    showarrow = FALSE,
    textangle = 270
  )

  time_plot <- plotly::subplot(
    time_plot_err,
    time_plot_pred,
    nrows = 2,
    shareX = TRUE
  ) %>%
  plotly::layout(
    plot_bgcolor = back_color,
    xaxis = list(
      title = "Date",
      zeroline = FALSE,
      showgrid = TRUE,
      mirror = "all",  # axis lines mirrored to the opposite side of the plotting area
      ticks = "", # TODO: "outside". Now, it draws tick lines in secondary axis of plot 1
      showline = TRUE,  # Show the x-axis line
      linewidth = 1,    # Line width
      linecolor = "black"  # Line color
    ),
    yaxis = list(
      title = "Error",
      zeroline = FALSE,
      showgrid = TRUE,
      mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
      ticks = "outside",
      showline = TRUE,  # Show the x-axis line
      linewidth = 1,    # Line width
      linecolor = "black"  # Line color
    ),
    yaxis2 = list(
      title = "Observ. and Pred.",
      zeroline = FALSE,
      showgrid = TRUE,
      mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
      ticks = "outside",
      showline = TRUE,  # Show the x-axis line
      linewidth = 1,    # Line width
      linecolor = "black"  # Line color
    ),
      legend = list(
        font = list(size = 14),
        y = 0.425
      )
  )

  return(time_plot)
}

# Function to calculate fitting plot
generate_fitting_plot <- function(graph_data, n_model, predict, ini, end, text, mae) {
  graph_data[, 3] <- 0

  # Calculate mean of predictions for all the models
  for (i in seq_len(n_model)) {
    graph_data[, 3] <- graph_data[, 3] + predict[, i]
  }
  graph_data[, 3] <- graph_data[, 3] / n_model

  min_x <- min(graph_data[, 2], na.rm = TRUE)
  max_x <- max(graph_data[, 2], na.rm = TRUE)
  min_y <- min(graph_data[, 3], na.rm = TRUE)
  max_y <- max(graph_data[, 3], na.rm = TRUE)

  x0 <- min_x - 0.02 * (max_x - min_x)
  x1 <- max_x + 0.02 * (max_x - min_x)
  y0 <- min_y - 0.02 * (max_y - min_y)
  y1 <- max_y + 0.02 * (max_y - min_y)

  graph_data <- graph_data[ini:end, ]

  # Create a Plotly scatter plot
  sca_plot <- plotly::plot_ly(data = graph_data[, 2:3],
                              x = ~graph_data[, 2],
                              y = ~graph_data[, 3],
                              type = "scatter",
                              mode = "markers",
                              marker = list(
                                symbol = "circle",
                                color = "rgb(99,184,255)",
                                line = list(color = "black", width = 1),
                                size = 8
                              ),
                              hovertemplate = paste(
                                "Observation: %{x:.2f}<br>",
                                "Prediction: %{y:.2f}",
                                "<extra></extra>" # Removes trace0
                              ),
                              showlegend = FALSE)

  # Customize the layout
  sca_plot <- sca_plot %>%
    plotly::layout(
      xaxis = list(
        title = text,
        titlefont = list(size = 18),
        standoff = 50,
        tickfont = list(size = 18),
        range = c(x0, x1)
      ),
      yaxis = list(
        title = "Prediction",
        titlefont = list(size = 18),
        standoff = 50,
        tickfont = list(size = 18),
        range = c(y0, y1)
      )
    )

  # Create a dashed line (abline) on the Plotly plot
  sca_plot <- plotly::add_trace(
      x = c(x0, x1),
      y = c(x0, x1),
      p = sca_plot,
      type = "scatter",
      mode = "lines",
      line = list(color = "royalblue"),
      marker = NULL,
      showlegend = FALSE
    )

  sca_plot <- plotly::add_trace(
      x = c(x0 - mae, x1 - mae),
      y = c(x0, x1),
      p = sca_plot,
      type = "scatter",
      mode = "lines",
      line = list(dash = "dash", color = "royalblue"),
      marker = NULL,
      showlegend = FALSE
    )

  sca_plot <- plotly::add_trace(
      x = c(x0 + mae, x1 + mae),
      y = c(x0, x1),
      p = sca_plot,
      type = "scatter",
      mode = "lines",
      line = list(dash = "dash", color = "royalblue"),
      marker = NULL,
      showlegend = FALSE
    )

  return(sca_plot)
}

# Funtion to generate bar plot
generate_bar_plot <- function(var_inf, min_var, max_var) {
  # Create a Plotly bar plot
  rel_influence_plot <- plotly::plot_ly(
    data = var_inf[min_var:max_var, ],
    x = ~relative_influence,
    y = ~var,
    type = "bar",
    marker = list(
      color = "rgb(99,184,255)",
      line = list(color = "black", width = 1)
    ),
    hovertemplate = paste(
      "Variable: %{y}<br>",
      "Influence: %{x:.2f}",
      "<extra></extra>" # Removes trace0
    )
  )

  # Customize the layout
  rel_influence_plot <- rel_influence_plot %>%
    plotly::layout(
      xaxis = list(
        title = "Relative Influence %",
        titlefont = list(size = 18),
        standoff = 50,
        tickfont = list(size = 18),
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis = list(
        title = "",
        titlefont = list(size = 18),
        standoff = 3,
        autorange = "reversed",
        tickfont = list(size = 18),
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the y-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      font = list(family = "Arial"),  # Set the font family
      showlegend = FALSE  # To hide the legend
    )

  rel_influence_plot <- plotly::config(
    p = rel_influence_plot,
    displayModeBar = FALSE  # To hide the mode bar
  )

  return(rel_influence_plot)
}

# Function to calculate bars plot for relative influence for new model
generate_bar_plot_new_model <- function(influmean) {
  mean_influence_ordered <- influmean[order(influmean[, 2]), ]
  var_inf <- data.frame(
    var = mean_influence_ordered[, 1],
    relative_influence = mean_influence_ordered[, 2]
  )

  # Group variables with the same first 3 letters
  group_names <- unique(substr(var_inf$var, 1, 3))
  groups <- lapply(group_names, function(name) var_inf[grep(name, var_inf$var), ])

  # Calculate group sizes
  group_sizes <- sapply(groups, function(group) nrow(group))

  # Create data frame for grouped influences
  var_inf_grouped <- data.frame(
    var = mapply(
      function(group_name, group) {
        if (group_sizes[group_name] > 1) {
          paste0(substr(group$var[1], 1, 3), "_group")
        } else {
          group$var[1]
        }
      },
      seq_along(group_names),
      groups
    ),
    relative_influence = mapply(
      function(group_name, group) {
        if (group_sizes[group_name] > 1) {
          sum(group$relative_influence, na.rm = TRUE)
        } else {
          group$relative_influence
        }
      },
      seq_along(group_names),
      groups
    )
  )

  var_inf_grouped <- var_inf_grouped[order(var_inf_grouped$relative_influence), ]

  vars_plot <- min(20, nrow(var_inf_grouped), na.rm = TRUE)
  max_var <- nrow(var_inf_grouped)
  min_var <- max_var + 1 - vars_plot

  # Generate and customize bar plot
  rel_influence_plot <- generate_bar_plot(var_inf_grouped, min_var, max_var)

  return(rel_influence_plot)
}


# Function to calculate partial dependence 1D plot
generate_pdp1d_plot <- function(
  model_res_fit,
  data_type,
  points_pd2,
  x_dp3,
  x_dp2,
  target
) {
  brt_model <- model_res_fit$model

  # Check if there is any data
  if (is.null(brt_model) || is.null(points_pd2)) {
    return(NULL)
  }

  # Draw one-variable PDP
  if (x_dp3 == "None") {
    base_plot <- plot(
      brt_model,
      i.var = match(x_dp2, brt_model$var.names),
      n.trees = brt_model$n.trees,
      continuous.resolution = points_pd2,
      return.grid = TRUE
    )

    print("Calculating 1D PDP graph")

    pd_plot <- plotly::plot_ly()

    # Calculate line plot for variable x_dp2
    pd_plot <- plotly::add_trace(
      data = base_plot,
      x = ~base_plot[, 1],
      y = ~base_plot[, 2],
      p = pd_plot,
      type = "scatter",
      mode = "markers+lines",
      line = list(color = "black", width = 2),
      marker = list(
        symbol = "circle",
        color = "rgb(99,184,255)",
        line = list(color = "black", width = 1),
        size = 8
      ),
      name = x_dp2,  # Set the legend label to x_dp2
      hovertemplate = paste(
        x_dp2, ": %{x:.2f}<br>",
        target, ": %{y:.2f}",
        "<extra></extra>"
      )
    )

    pd_plot <- pd_plot %>%
    plotly::layout(
      xaxis = list(
        title = x_dp2,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis = list(
        title = target,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        zeroline = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the y-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.35,
        y = -0.2,
        font = list(size = 14)
      ),
      font = list(family = "Arial"),
      margin = list(l = 100, r = 30, t = 70, b = 70)
    )
  } else { # Draw 2 one-variable PDP
    base_plot <- plot(
      brt_model,
      i.var = match(x_dp2, brt_model$var.names),
      n.trees = brt_model$n.trees,
      continuous.resolution = points_pd2,
      return.grid = TRUE
    )

    base_plot2 <- plot(
      brt_model,
      i.var = match(x_dp3, brt_model$var.names),
      n.trees = brt_model$n.trees,
      continuous.resolution = points_pd2,
      return.grid = TRUE
    )

    pd_plot <- plotly::plot_ly()

    # Calculate line plot for variable x_dp2
    pd_plot <- plotly::add_trace(
      data = base_plot,
      x = ~base_plot[, 1],
      y = ~base_plot[, 2],
      p = pd_plot,
      type = "scatter",
      mode = "markers+lines",
      line = list(color = "black", width = 2),
      marker = list(
        symbol = "circle",
        color = "rgb(99,184,255)",
        line = list(color = "black", width = 1),
        size = 8
      ),
      name = x_dp2,  # Set the legend label to x_dp2
      hovertemplate = paste(
        x_dp2, ": %{x:.2f}<br>",
        target, ": %{y:.2f}",
        "<extra></extra>"
      )
    )

    # Calculate line plot for variable x_dp3
    pd_plot <- plotly::add_trace(
      data = base_plot2,
      x = ~base_plot2[, 1],
      y = ~base_plot2[, 2],
      p = pd_plot,
      xaxis = "x2",
      type = "scatter",
      mode = "markers+lines",
      line = list(color = "black", width = 2),
      marker = list(
        symbol = "circle",
        color = "rgb(99,184,255)",
        line = list(color = "black", width = 1),
        size = 16
      ),
      name = x_dp3,  # Set the legend label to x_dp2
      hovertemplate = paste(
        x_dp3, ": %{x:.2f}<br>",
        target, ": %{y:.2f}",
        "<extra></extra>"
      )
    )

    pd_plot <- pd_plot %>%
    plotly::layout(
      xaxis = list(
        title = x_dp2,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis = list(
        title = target,
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the y-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      xaxis2 = list(
        title = x_dp3,
        overlaying = "x",
        side = "top",
        titlefont = list(size = 18),
        tickfont = list(size = 18),
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis2 = list(
        showticklabels = FALSE,
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the y-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.35,
        y = -0.2,
        font = list(size = 14)
      ),
      font = list(family = "Arial"),
      margin = list(l = 100, r = 30, t = 70, b = 70)
    )
  }

  return(pd_plot)
}

# Function to filter train data
filter_train_data <- function(data_type, model_res_fit) {
  min_train <- model_res_fit$train_y[1]
  max_train <- model_res_fit$train_y[2]

  if (is.null(data_type) || data_type != 2) {
    train_data <- model_res_fit$data_in[
      model_res_fit$data_in[, 1] >= min_train & model_res_fit$data_in[, 1] <= max_train,
    ]
  } else {
    train_data <- model_res_fit$data_in[-model_res_fit$positions, ]
  }

  train_data
}

# Helper function to calculate heat_p
calculate_heat_p <- function(brt_model, train_data, points_pd2, x_dp2, x_dp3) {
  n_points <- points_pd2 + 1
  heat_p <- pdp::partial(
    brt_model,
    pred.var = c(x_dp2, x_dp3),
    train = train_data,
    plot = FALSE,
    chull = TRUE,
    n.trees = brt_model$n.trees,
    grid.resolution = n_points
  )

  heat_p
}

# Tick labels for partial dependence plots
create_tick_labels <- function(heat_p_row) {
  min_val <- min(heat_p_row)
  max_val <- max(heat_p_row)

  text <- c(
    toString(round(min_val + 0.25 * (max_val - min_val), digits = 1)),
    toString(round(min_val + 0.50 * (max_val - min_val), digits = 1)),
    toString(round(min_val + 0.75 * (max_val - min_val), digits = 1))
  )

  text
}

# Create plot data for partial dependence plots
create_data_pdp_plot <- function(columns, heat_p, heat_data_1, heat_data_3) {
  for (j in seq_along(columns)) {
    end <- if (j == length(columns)) {
      length(heat_p[, 2]) - columns[j] + 1
    } else {
      columns[j + 1] - columns[j]
    }
    heat_data_1[1:end, j] <- heat_p[columns[j]:(end + columns[j] - 1), 1]
    heat_data_3[1:end, j] <- heat_p[columns[j]:(end + columns[j] - 1), 3]
  }

  min_row <- apply(heat_data_1, 1, FUN = min, na.rm = TRUE)
  heat_data_1[heat_data_1 > min_row] <- NA
  heat_data_3[heat_data_1 > min_row] <- NA

  plot_data <- heat_data_3

  plot_data
}

# Plotly partial dependence plot in 2D
generate_pdp2d_plotly_plot <- function(
  plot_data,
  target,
  x_dp2,
  x_dp3,
  n_cols,
  n_rows,
  text_x,
  text_y
) {
  plot_2d <- plotly::plot_ly(
    z = plot_data,
    showscale = TRUE,
    hovertemplate = paste(
      target, ": %{z:.2f}",
      "<extra></extra>"
    ),
    hoverlabel = list(font = list(size = 12)),
    colorbar = list(title = paste(target)),
    type = "contour"
  )

  plot_2d <- plot_2d %>%
    plotly::layout(
      title = NULL,
      xaxis = list(
        title = x_dp3,
        tickvals = c(
          (n_cols - 1) * 0.25,
          (n_cols - 1) * 0.50,
          (n_cols - 1) * 0.75
        ),
        ticktext = text_x,
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the x-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      ),
      yaxis = list(
        title = x_dp2,
        tickvals = c(
          (n_rows - 1) * 0.25,
          (n_rows - 1) * 0.50,
          (n_rows - 1) * 0.75
        ),
        ticktext = text_y,
        zeroline = FALSE,
        showgrid = FALSE,
        mirror = TRUE,  # axis lines mirrored to the opposite side of the plotting area
        ticks = "outside",
        showline = TRUE,  # Show the y-axis line
        linewidth = 1,    # Line width
        linecolor = "black"  # Line color
      )
    )

  plot_2d
}

# Function to calculate PDP 2D plot
generate_pdp2d_plot <- function(
  data_type,
  model_res_fit,
  points_pd2,
  x_dp2,
  x_dp3,
  target
) {
  # Filter train data
  train_data <- filter_train_data(data_type, model_res_fit)

  # Check if there is any data
  if (is.null(model_res_fit$model)) {
    return(NULL)
  }

  heat_p <- calculate_heat_p(model_res_fit$model, train_data, points_pd2, x_dp2, x_dp3)

  # Order the values for plotting in a matrix
  n_rows <- length(unique(heat_p[, 1]))
  n_cols <- length(unique(heat_p[, 2]))
  heat_data_1 <- matrix(NA, n_rows, n_cols)
  heat_data_3 <- matrix(NA, n_rows, n_cols)
  columns <- match(unique(heat_p[, 2]), heat_p[, 2])

  plot_data <- create_data_pdp_plot(columns, heat_p, heat_data_1, heat_data_3)

  text_x <- create_tick_labels(heat_p[, 2])
  text_y <- create_tick_labels(heat_p[, 1])

  # Draw partial dependence plot 2D
  plot_2d <- generate_pdp2d_plotly_plot(
    plot_data,
    target,
    x_dp2,
    x_dp3,
    n_cols,
    n_rows,
    text_x,
    text_y
  )

  return(plot_2d)
}

generate_pdp3d_plotly_plot <- function(
  plot_data,
  target,
  x_dp2,
  x_dp3,
  n_cols,
  n_rows,
  text_x,
  text_y
) {
  plot_3d <- plotly::plot_ly(
    z = plot_data,
    showscale = FALSE,
    hovertemplate = paste(
      target, ": %{z:.2f}",
      "<extra></extra>"
    ),
    hoverlabel = list(font = list(size = 12)),
  )
  plot_3d <- plot_3d %>%
    plotly::add_surface(
      lighting = list(
        roughness = 0.3,
        ambient = 0.8,
        diffuse = 0.1
      ),
      colorscale = list(
        c(0, 0.5, 1),
        c("rgb(0, 0, 150)",
        "rgb(60, 220, 200)",
        "rgb(240, 255, 220)")
      )
    ) %>%
    plotly::layout(
      title = NULL,
      scene = list(
        xaxis = list(
          title = x_dp3,
          tickvals = c(
            (n_cols - 1) * 0.25,
            (n_cols - 1) * 0.50,
            (n_cols - 1) * 0.75
          ),
          ticktext = text_x
        ),
        yaxis = list(
          title = x_dp2,
          tickvals = c(
            (n_rows - 1) * 0.25,
            (n_rows - 1) * 0.50,
            (n_rows - 1) * 0.75
          ),
          ticktext = text_y
        ),
        zaxis = list(title = target)
      )
    )

  plot_3d
}

# Function to calculate PDP 3D plot
generate_pdp3d_plot <- function(
  model_res_fit,
  data_type,
  points_pd2,
  x_dp2,
  x_dp3,
  target
) {
  # Filter train data
  train_data <- filter_train_data(data_type, model_res_fit)

  if (is.null(model_res_fit$model)) {
    return(NULL)
  } # Check if there is any data

  # Calculate values for plotting
  heat_p <- calculate_heat_p(model_res_fit$model, train_data, points_pd2, x_dp2, x_dp3)

  # Order the values for plotting in a matrix
  n_rows <- length(unique(heat_p[, 1]))
  n_cols <- length(unique(heat_p[, 2]))
  heat_data_1 <- matrix(NA, n_rows, n_cols)
  heat_data_3 <- matrix(NA, n_rows, n_cols)
  columns <- match(unique(heat_p[, 2]), heat_p[, 2])

  plot_data <- create_data_pdp_plot(columns, heat_p, heat_data_1, heat_data_3)

  # Spin the matrix for better plotting
  max_pos <- which(plot_data == max(plot_data, na.rm = TRUE))[1]
  max_col <- round(0.49 + (max_pos / n_rows))
  max_row <- max_pos - n_rows * (max_col - 1)

  text_x <- create_tick_labels(heat_p[, 2])
  text_y <- create_tick_labels(heat_p[, 1])

  # If the maximum is in the wrong half, spin all the data
  if (max_row >= n_rows * 0.50) {
    plot_data_aux <- plot_data

    for (j in seq_len(n_cols)) {
      i <- 1:n_rows
      plot_data[i, j] <- plot_data_aux[1 + n_rows - i, j]
    }

    min_y <- min(heat_p[, 1])
    max_y <- max(heat_p[, 1])

    text_y <- c(
      toString(round(min_y + 0.75 * (max_y - min_y), digits = 1)),
      toString(round(min_y + 0.50 * (max_y - min_y), digits = 1)),
      toString(round(min_y + 0.25 * (max_y - min_y), digits = 1))
    )
  }

  # If the maximum is in the wrong half, spin all the data
  if (max_col >= n_cols * 0.50) {
    plot_data_aux <- plot_data

    for (i in seq_len(n_rows)) {
      j <- 1:n_cols
      plot_data[i, j] <- plot_data_aux[i, 1 + n_cols - j]
    }

    min_x <- min(heat_p[, 2])
    max_x <- max(heat_p[, 2])

    text_x <- c(
      toString(round(min_x + 0.75 * (max_x - min_x), digits = 1)),
      toString(round(min_x + 0.50 * (max_x - min_x), digits = 1)),
      toString(round(min_x + 0.25 * (max_x - min_x), digits = 1))
    )
  }

  # Draw partial dependence plot 3D
  plot_3d <- generate_pdp3d_plotly_plot(
    plot_data,
    target,
    x_dp2,
    x_dp3,
    n_cols,
    n_rows,
    text_x,
    text_y
  )

  return(plot_3d)
}

#------------------------------------------------------------------------------#
#-----------------------------Set values functions-----------------------------#
#------------------------------------------------------------------------------#
# Function to select prediction variables
select_prediction_variables <- function(classes, datum, target, groups) {
  date_class <- which("Date" == classes)
  pos_class <- which("POSIXct" == classes)

  # Check if there is any data
  if (
    (length(c(date_class, pos_class)) > 0) && !is.na(c(date_class, pos_class))
  ) {
    items <- sort(names(datum)[-c(date_class, pos_class)])
  } else {
    items <- sort(names(datum))
  }

  # Don't allow to choose target as input
  groups_list <- create_variables_groups(classes, datum)
  mat_mo <- groups_list[[1]]
  items <- groups_list[[2]]
  target_num <- match(target, items)
  items <- items[-target_num]
  return(list(mat_mo, items))
}

# Function to form variable groups
create_variables_groups <- function(classes, datum) {
  num_class <- which("numeric" == classes)
  items <- sort(names(datum)[num_class])
  groups <- as.data.frame(matrix(
    data = NA,
    nrow = length(items),
    ncol = length(items)
  ))

  pos <- 0
  j <- 1

  # Search the first 3 letters of each varaible name
  for (i in seq_along(items)) {
    first_3_letters <- substr(items[i], 1, 3)
    same_letters <- grep(first_3_letters, substr(items, 1, 3))
    if (length(same_letters) > 1) {
      # Assign to a group the variables with the same letters
      for (k in seq_along(same_letters)) {
        groups[k, j] <- same_letters[k]
      }
      colnames(groups)[j] <- first_3_letters
      j <- j + 1
    }
  }

  # Search the amount of variables in the biggest group
  for (i in seq_len(nrow(groups))) {
    if (!all(is.na(groups[i, ]))) pos <- i
  }

  mat_gr <- 0

  if (pos > 0) {
    groups <- groups[1:pos, unique(colnames(groups))]
    groups <- groups[, !is.na(groups[1, ])]
    mat_gr <- groups
    items <- c(paste(colnames(groups), "group"), items)
  }

  return(list(mat_gr, items))
}

# Function to match groups of variables
select_variables <- function(datum, selected_vars) {
  classes <- identify_classes(datum)
  pos_numeric_classes <- which("numeric" == classes)
  pos_factor_classes <- which("factor" == classes)
  sorted_vars <- sort(names(datum)[c(pos_numeric_classes, pos_factor_classes)])

  selected_individual_vars <- selected_vars

  pos_group_vars <- grep("group", selected_individual_vars)

  if (length(pos_group_vars) > 0) {
    # Split individual and group variables
    selected_individual_vars <- selected_individual_vars[-pos_group_vars]
    selected_group_vars <- selected_vars[pos_group_vars]

    # Collect only the first three letters of group variables
    for (i in seq_along(selected_group_vars)) {
      selected_group_vars[i] <- gsub(" group", "", selected_group_vars[i])
    }

    # Add individual vars from groups to selected_individual_vars
    for (j in seq_along(selected_group_vars)) {
      for (k in seq_along(sorted_vars)) {
        if (substr(sorted_vars[k], 1, 3) == selected_group_vars[j]) {
          selected_individual_vars <- append(
            selected_individual_vars,
            sorted_vars[k]
          )
        }
      }
    }
  }

  return(selected_individual_vars)
}

#------------------------------------------------------------------------------#
#-------------------------------Other functions--------------------------------#
#------------------------------------------------------------------------------#

# Function to calculate the influence of the variables
calculate_variables_influence <- function(
  influ,
  iter
) {
  influ$mean[, 1] <- influ$inf[, 1]
  influ$mean[, 2] <- 0

  # Calculate mean of influences for all the models
  for (i in seq_len(iter)) {
    influ$mean[, 2] <- influ$mean[, 2] + influ$inf[, i + 1]
  }

  influ$mean[, 2] <- influ$mean[, 2] / iter

  return(influ$mean)
}

# Create a function to calculate min and max values based on given conditions
calculate_min_max <- function(data, input_index, min_train, max_train, min_test, max_test) {
  train_max <- max(data[data[, 1] >= min_train & data[, 1] <= max_train, input_index], na.rm = TRUE)
  test_max <- max(data[data[, 1] >= min_test & data[, 1] <= max_test, input_index], na.rm = TRUE)
  train_min <- min(data[data[, 1] >= min_train & data[, 1] <= max_train, input_index], na.rm = TRUE)
  test_min <- min(data[data[, 1] >= min_test & data[, 1] <= max_test, input_index], na.rm = TRUE)

  return(
    list(train_max = train_max, test_max = test_max, train_min = train_min, test_min = test_min)
  )
}

# Check the ranges of time-independent data
check_ranges_time_indep_data <- function(
  inputs_num,
  values,
  positions,
  exc_vars,
  inputs,
  target,
  min_train,
  max_train,
  min_test,
  max_test
) {
  aux_vars <- exc_vars

  # Check range of inputs not factor
  for (i in seq_along(inputs_num)) {
    if ("factor" != class(values$dat[, inputs_num[i]])) {
      train_max <- max(values$dat[-positions, inputs_num[i]], na.rm = TRUE)
      test_max <- max(values$dat[positions, inputs_num[i]], na.rm = TRUE)
      train_min <- min(values$dat[-positions, inputs_num[i]], na.rm = TRUE)
      test_min <- min(values$dat[positions, inputs_num[i]], na.rm = TRUE)

      if ((train_max < test_max) || (test_min < train_min)) {
        aux_vars <- paste(aux_vars, inputs[i], sep = " ")
      }
    }
  }

  # Check range of target value
  target_num <- match(target, colnames(values$dat))

  # Calculate min and max values
  train_max <- max(values$dat[-positions, target_num], na.rm = TRUE)
  test_max <- max(values$dat[positions, target_num], na.rm = TRUE)
  train_min <- min(values$dat[-positions, target_num], na.rm = TRUE)
  test_min <- min(values$dat[positions, target_num], na.rm = TRUE)

  # Check range of target variable
  if ((train_max < test_max) || (test_min < train_min)) {
    aux_vars <- paste(aux_vars, target, sep = " ")
  }

  return(aux_vars)
}
# Helper function to check if a variable's range is exceeded
check_range_exceeded <- function(data, input_num, min_train, max_train, min_test, max_test) {
  min_max_values <- calculate_min_max(data, input_num, min_train, max_train, min_test, max_test)
  train_max <- min_max_values$train_max
  test_max <- min_max_values$test_max
  train_min <- min_max_values$train_min
  test_min <- min_max_values$test_min

  return(train_max < test_max || test_min < train_min)
}

# Check ranges for time-dependent data
check_ranges_time_dep_data <- function(
  inputs_num,
  input_name,
  values,
  exc_vars,
  inputs,
  target,
  min_train,
  max_train,
  min_test,
  max_test
) {
  aux_vars <- exc_vars

  # Check range of inputs not factor (except "Year")
  for (i in seq_along(inputs_num)) {
    if ("factor" != class(values$dat[, inputs_num[i]]) && "Year" != input_name[inputs_num[i]]) {
      if (
        check_range_exceeded(values$dat, inputs_num[i], min_train, max_train, min_test, max_test)
      ) {
        aux_vars <- paste(aux_vars, inputs[i], sep = " ")
      }
    }
  }

  # Check range of target value
  target_num <- match(target, colnames(values$dat))
  if (check_range_exceeded(values$dat, target_num, min_train, max_train, min_test, max_test)) {
    aux_vars <- paste(aux_vars, target, sep = " ")
  }

  return(aux_vars)
}

# Check the ranges of test and train data
check_test_train_range <- function(
  data_type,
  values,
  positions,
  target_num,
  target,
  inputs,
  min_train,
  max_train,
  min_test,
  max_test
) {
  text <- NULL
  input_name <- colnames(values$dat)
  inputs_num <- match(inputs, input_name)
  exc_vars <- NULL

  if ((!is.null(data_type) && data_type == 2)) {
    aux_vars <- check_ranges_time_indep_data(
      inputs_num,
      values,
      positions,
      exc_vars,
      inputs,
      target,
      min_train,
      max_train,
      min_test,
      max_test
    )
    exc_vars <- aux_vars
  } else {
    aux_vars <- check_ranges_time_dep_data(
      inputs_num,
      input_name,
      values,
      exc_vars,
      inputs,
      target,
      min_train,
      max_train,
      min_test,
      max_test
    )
    exc_vars <- aux_vars
  }

  text <- paste(
    "Warning: the range for test data of some variables (",
    exc_vars,
    ") exceeds the range for input training data"
  )

  if (is.null(exc_vars)) {
    text <- NULL
  }
  return(text)
}

# Function to change character variables to factor variables
change_char_to_factor <- function(dat) {
  datum <- dat
  for (i in seq_len(ncol(datum))) {
    if (class(datum[, i])[1] == "character") {
      factor_var <- as.factor(datum[, i])
      names_vars <- names(datum)
      if (i == 1) {
        datum <- cbind(factor_var, datum[, (i + 1):ncol(datum)])
      } else if (i == ncol(datum)) {
        datum <- cbind(datum[, 1:(i - 1)], factor_var)
      } else {
        datum <- cbind(datum[, 1:(i - 1)], factor_var, datum[, (i + 1):ncol(datum)])
      }
      names(datum) <- names_vars
    }
  }
  return(datum)
}

# Function to build model
build_model <- function(
  influ,
  input,
  results,
  model_p,
  model_e,
  data_type,
  iter,
  model_formula,
  i_num_trees,
  i_int_depth,
  i_shrinkage,
  i_bag_fraction,
  test_data,
  train_data,
  target,
  positions,
  min_train,
  max_train,
  min_test,
  max_test,
  dat,
  test_perc_2
) {
  # If more than one model, change seed
  set.seed(iter)

  # Save brt_model out of the eventReactive
  brt_model <- gbm::gbm(
    formula = model_formula,
    distribution = "gaussian",
    data = train_data,
    n.trees = i_num_trees,
    interaction.depth = i_int_depth,
    n.minobsinnode = 5,
    shrinkage = i_shrinkage,
    bag.fraction = i_bag_fraction,
    verbose = FALSE
  )

  # Compute predictions and extract observations
  pred_test <- predict(
    brt_model,
    newdata = test_data,
    n.trees = brt_model$n.trees
  )

  pred_train <- predict(
    brt_model,
    newdata = train_data,
    n.trees = brt_model$n.trees
  )

  prediction <- c(pred_train, pred_test)

  obs_train <- train_data[, target]
  obs_test <- test_data[, target]
  observation <- c(obs_train, obs_test)

  # Compute errors and complete dataframe
  error_train <- c(pred_train - obs_train)
  error_test <- c(pred_test - obs_test)
  first <- c(train_data[, 1], test_data[, 1])
  error <- c(prediction - observation)
  data_out <- data.frame(first, observation, prediction, error)

  mae_train <- round(
    (generics::accuracy(pred_train, obs_train)[3]),
    digits = 2
  )

  mae_test <- round(
    (generics::accuracy(pred_test, obs_test)[3]),
    digits = 2
  )

  r2_train <- round(
    1 - sum((obs_train - pred_train)^2) / sum((obs_train - mean(obs_train))^2),
    digits = 2
  )

  r2_test <- round(
    1 - sum((obs_test - pred_test)^2) / sum((obs_test - mean(obs_train))^2),
    digits = 2
  )

  # Save relative influence of variables (sorted numerically and alphabetically)
  var_inf_num <- summary(brt_model, plotit = FALSE)
  var_inf <- var_inf_num[order(var_inf_num[, 1]), ]
  influ$inf[, 1] <- data.frame(var = rownames(var_inf))
  influ$inf[, iter + 1] <- data.frame(relative_influence = var_inf[, 2])

  if (input$data_type != 2) {
    data_sort_residual_train <- xts::xts(error_train, order.by = train_data[, 1])
  } else {
    data_sort_residual_train <- error_train
  }
  names(data_sort_residual_train) <- "Error train"
  results$residual_train <- data_sort_residual_train

  if (input$data_type != 2) {
    data_sort_residual_test <- xts::xts(error_test, order.by = test_data[, 1])
  } else {
    data_sort_residual_test <- error_test
  }
  names(data_sort_residual_test) <- "Error test"
  results$residual_test <- data_sort_residual_test

  results$error[1, iter] <- mae_train
  results$error[2, iter] <- mae_test
  results$error[3, iter] <- r2_train
  results$error[4, iter] <- r2_test

  model_p$pre[, iter] <- prediction
  model_e$error[, iter] <- error

  # Save all the model parameters
  model_res <- list(
    model = brt_model,
    data_out = data_out,
    mae.train = mae_train,
    R2.train = r2_train,
    mae.test = mae_test,
    R2.test = r2_test,
    positions = positions,
    train_y = c(min_train, max_train),
    test_y = c(min_test, max_test),
    data_in = dat,
    train_perc = test_perc_2,
    prediction = prediction,
    influ = influ$inf
  )

  model_res_fit <- model_res

  return(list(model_res_fit, brt_model))
}

# Function to calculate classes for columns
identify_classes <- function(datum) {
  # Don't allow to choose class Date variables
  classes <- vector("character", length = ncol(datum))
  classes[1] <- class(datum[, 1])[1]

  # Check class of each variable
  for (i in 2:ncol(datum)) {
    classes[i] <- class(datum[, i])
  }

  return(classes)
}

# Function to search for wrongly classified columns
search_wrong_class_columns <- function(datum, classes) {
  char_classes <- which("character" == classes)

  for (i in char_classes) {
    old_na <- sum(is.na(datum[, i]))
    numbers <- suppressWarnings(as.numeric(gsub(",", ".", datum[, i])))
    new_na <- sum(is.na(numbers))
    if ((new_na - old_na) == 0) {
      datum[, i] <- numbers
    }
  }

  return(datum)
}

### Function to create "Month" and "Year" (if "Month" doesn't exist)
generate_month_year <- function(datum, n_columns) {
  if (!any(!is.na(match(names(datum), "Year")))) {
    datum[, (n_columns + 1)] <- datum[, n_columns]
    datum[, 3:n_columns] <- datum[, 2:(n_columns - 1)]
    names(datum)[2:(n_columns + 1)] <- names(datum)[1:n_columns]
    names(datum)[2] <- c("Year")
    if (as.numeric(datum[1, 1]) > 100000) {
      datum[, 2] <- round(1970 + (as.numeric(datum[, 1]) / (3600 * 24 * 365.25)), digits = 2)
    } else {
      datum[, 2] <- round(1970 + (as.numeric(datum[, 1]) / (365.25)), digits = 2)
    }
    n_columns <- n_columns + 1
  }

  if (!any(!is.na(match(names(datum), "Month")))) {
    datum[, (n_columns + 1)] <- datum[, n_columns]
    datum[, 3:n_columns] <- datum[, 2:(n_columns - 1)]
    names(datum)[2:(n_columns + 1)] <- names(datum)[1:n_columns]
    names(datum)[2] <- c("Month")
    datum[, 2] <- as.numeric(datum[, 1], format = "%m", tz = "GMT")
  }

  return(datum)
}