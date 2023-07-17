
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

  date <- as.Date(index(data_sort))
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
      name = vars
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
        name = vars[[i]]
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
      tickfont = list(size = 16)
    )

    if (length(vars2) == 1) {
      time_plot <- add_trace(
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
        name = vars2
      )
    } else {
      for (j in seq_along(vars2)) {
        time_plot <- add_trace(
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
          name = vars2[[j]]
        )
      }
    }

    time_plot <- time_plot %>%
    plotly::layout(
      plot_bgcolor = back_color,
      yaxis2 = secondary_y,
      xaxis = list(
        title = "Date",
        rangeslider = list(type = "date"),
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),
      yaxis = list(
        title = y_labels,
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),
      legend = list(
        font = list(size = 14),
      x = 0.95,
      y = 0.95
    )
    )
  } else {
    time_plot <- time_plot %>%
    plotly::layout(
      plot_bgcolor = back_color,
      xaxis = list(
        title = "Date",
        rangeslider = list(type = "date"),
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),
      yaxis = list(
        title = y_labels,
        zeroline = FALSE,
        showgrid = TRUE,
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),
      legend = list(
        font = list(size = 14),
        x = 0.95,
        y = 0.95
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
  colours2
) {

  set_color <- RColorBrewer::brewer.pal(8, "Dark2")
  back_color <- "white"

  if (colours2) {
    set_color <- rainbow(8)
    back_color <- "darkgrey"
  }

  date <- as.Date(index(data_sort))
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
    layout
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
    name = "Prediction"
  )

  # Calculate the upper and lower bounds of the confidence interval
  sd_train_error = sd(results$residual_train)
  upper_bound <- graph_data$Prediction + 2 * sd_train_error
  lower_bound <- graph_data$Prediction - 2 * sd_train_error

  # Add the transparent confidence interval trace
  time_plot_pred <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = upper_bound,
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    p = time_plot_pred,
    line = list(color = set_color[2], width = 0),
    showlegend = FALSE
  )

  time_plot_pred <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = lower_bound,
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    p = time_plot_pred,
    line = list(color = set_color[2], width = 0),
    fill = "tonexty",  # Fill the area between the lines
    fillcolor = "rgba(0,0,0,0.2)",  # Transparent fill color
    showlegend = FALSE
  )

  time_plot_pred <- time_plot_pred %>%
  plotly::layout(
    shapes = list(
      vline(x = as.Date(start_train)),
      vline(x = as.Date(end_train)),
      vline(x = as.Date(end_test))
    )
  ) %>%
  plotly::add_annotations(
    showlegend = FALSE,
    x = c(as.Date(start_train), as.Date(end_train), as.Date(end_test)),
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
    name = "Error"
  )

  # Calculate the upper and lower bounds of the confidence interval
  upper_bound_err <- + (2 * sd(graph_data$Error))
  lower_bound_err <- - (2 * sd(graph_data$Error))

  # Add the transparent confidence interval trace
  time_plot_err <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = upper_bound_err,
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    p = time_plot_err,
    line = list(color = set_color[2], width = 0),
    showlegend = FALSE
  )

  time_plot_err <- plotly::add_trace(
    data = graph_data,
    x = date,
    y = lower_bound_err,
    type = "scatter",
    mode = "lines",
    connectgaps = FALSE,
    p = time_plot_err,
    line = list(color = set_color[2], width = 0),
    fill = "tonexty",  # Fill the area between the lines
    fillcolor = "rgba(0,0,0,0.2)",  # Transparent fill color
    showlegend = FALSE
  )

  time_plot_err <- time_plot_err %>%
  plotly::layout(
    shapes = list(
      vline(x = as.Date(start_train)),
      vline(x = as.Date(end_train)),
      vline(x = as.Date(end_test))
    )
  ) %>%
  plotly::add_annotations(
    showlegend = FALSE,
    x = c(as.Date(start_train), as.Date(end_train), as.Date(end_test)),
    y = c(min_y_err, min_y_err),
    xref = "x",
    yref = "y",
    text = c("Start of training", "End of training", "End of testing"),
    xanchor = "right",
    showarrow = FALSE,
    textangle = 270
  )

  time_plot <- plotly::subplot(
    time_plot_pred,
    time_plot_err,
    nrows = 2,
    shareX = TRUE
  ) %>%
  plotly::layout(
    plot_bgcolor = back_color,
    xaxis = list(
      title = "Date",
      rangeslider = list(type = "date"),
      zeroline = FALSE,
      showgrid = TRUE
    ),
    yaxis = list(
      title = "Observ. and Pred.",
      zeroline = FALSE,
      showgrid = TRUE
    ),
    yaxis2 = list(
      title = "Error",
      zeroline = FALSE,
      showgrid = TRUE
    )
  )
  return(time_plot)
}

# Function to calculate fitting plot
fit_fun <- function(graph_data, n_model, predict, ini, end, text) {
  graph_data[, 3] <- 0

  # Calculate mean of predictions for all the models
  for (i in seq_len(n_model)) {
    graph_data[, 3] <- graph_data[, 3] + predict[, i]
  }
  graph_data[, 3] <- graph_data[, 3] / n_model

  min2 <- min(graph_data[, 2], na.rm = TRUE)
  max2 <- max(graph_data[, 2], na.rm = TRUE)
  min3 <- min(graph_data[, 3], na.rm = TRUE)
  max3 <- max(graph_data[, 3], na.rm = TRUE)
  graph_data <- graph_data[ini:end, ]
  y_points <- ggplot2::aes(y = prediction)

  sca_plot <- line_fun_ggplot2(
    graph_data[, 2:3],
    text,
    "Prediction",
    graph_data[, 2],
    y_points,
    c(min2, max2),
    c(min3, max3)
  )

  sca_plot <- sca_plot +
    ggplot2::geom_abline(linetype = "dashed", color = "royalblue")

  return(sca_plot)
}

# Function to calculate line plot
line_fun_ggplot2 <- function(
  base_plot,
  x_dp2,
  response_name,
  x_var_scat,
  y_points,
  lim_x,
  lim_y
) {
  pd_plot <- ggplot2::ggplot(
    data = base_plot,
    ggplot2::aes(x = x_var_scat, y = NULL)
  ) +
  ggplot2::geom_point(y_points, shape = 21, fill = "steelblue1") +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, vjust = 3)) +
  ggplot2::labs(x = x_dp2, y = response_name) +
  ggplot2::coord_cartesian(xlim = lim_x, ylim = lim_y) +
  pre_theme()

  return(pd_plot)
}

# Function to calculate box plot for relative influence
box_fun <- function(influ, n_model) {
  # Sort means numerically
  mean_influence_ordered <- influ$mean[order(influ$mean[, 2]), ]
  vars_plot <- min(10, nrow(mean_influence_ordered), na.rm = TRUE)
  box_var <- vector("character", length = vars_plot)
  box_values <- c(0, 0)

  # Save names of the variables with more influence (last ones)
  for (i in seq_len(vars_plot)) {
    box_var[i] <- as.character(
      mean_influence_ordered[i + nrow(mean_influence_ordered) - vars_plot, 1]
    )
  }

  # Save row numbers for most influence variables
  row_numbers <- match(box_var, influ$inf[, 1])
  data_plot <- as.data.frame(
    matrix(
      NA,
      nrow = ncol(influ$inf) - 1,
      ncol = vars_plot
    )
  )

  # Sort numerically influences of each model changing rows for columns
  for (i in seq_len(nrow(data_plot))) {
    for (j in seq_len(ncol(data_plot))) {
      data_plot[i, j] <- influ$inf[row_numbers[j], i + 1]
    }
  }

  # Change matrix into vector
  for (i in seq_len(vars_plot)) {
    ini <- (n_model * (i - 1)) + 1
    end <- n_model * i
    box_values[ini:end] <- c(data_plot[, i])
  }
  box_data <- data.frame(
    var = factor(rep(box_var, each = n_model)),
    rel.inf = box_values
  )

  min_var <- 1
  max_var <- nrow(box_data)
  box <- bars_fun(box_data, min_var, max_var) # Calculate bars plots
  box <- box +
    ggplot2::geom_boxplot(fill = "steelblue1")
  return(box)
}

# Funtion to calculate bars plots
bars_fun <- function(var_inf, min_var, max_var) {
  rel_influence_plot <- ggplot2::ggplot(
    var_inf[min_var:max_var, ],
    ggplot2::aes(x = var, y = rel.inf)
  )

  rel_influence_plot <- rel_influence_plot +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, vjust = 3)) +
    ggplot2::labs(x = NULL, y = "Relative Influence %") +
    pre_theme() +
    theme(text = element_text(family = "Arial"))
  return(rel_influence_plot)
}

# Function to calculate bars plot for relative influence for new model
bars_new_fun <- function(influmean) {
  mean_influence_ordered <- influmean[order(influmean[, 2]), ]
  var_inf <- data.frame(
    var = mean_influence_ordered[, 1],
    rel.inf = mean_influence_ordered[, 2]
  )

  # Search the first 3 letters of each variable name
  groups <- as.data.frame(
    matrix(
      data = NA,
      nrow = nrow(var_inf),
      ncol = nrow(var_inf)
    )
  )

  all_groups <- 0
  j <- 1
  for (i in seq_len(nrow(var_inf))) {
    first_3_letters <- substr(var_inf[i, 1], 1, 3)
    same_name <- grep(first_3_letters, substr(var_inf[, 1], 1, 3))
    if (length(same_name) > 1) {
      all_groups <- c(all_groups, same_name)

      # Assign to a group the variables with the same letters
      for (k in seq_along(same_name)) {
        groups[k, j] <- same_name[k]
      }

      names(groups)[j] <- as.character(first_3_letters)
      j <- j + 1
    }
  }

  # Search the amount of variables in the biggest group
  pos <- 0
  for (i in seq_len(nrow(groups))) {
    if (!all(is.na(groups[i, ]))) pos <- i
  }

  # Replace variable influences for group influences
  if (pos > 0) {
    mat_im <- 0
    base_na <- colnames(groups)[1]
    groups <- groups[1:pos, unique(colnames(groups))]
    if (is.null(nrow(groups))) {
      return(NULL)
    }
    groups <- groups[, !is.na(groups[1, ])]
    if (!is.null(ncol(groups))) {
      base_na <- colnames(groups)
    }
    mat_im <- groups
    names(mat_im) <- paste(base_na, "group")
    if (length(nrow(mat_im)) > 0) {
      for (i in seq_len(nrow(mat_im))) {
        for (j in seq_len(ncol(mat_im))) {
          mat_im[i, j] <- var_inf[mat_im[i, j], 2]
        }
      }
      colu <- ncol(mat_im)
    } else {
      for (i in seq_along(mat_im)) {
        mat_im[i] <- var_inf[mat_im[i], 2]
      }
      colu <- 1
    }
    singles <- c(1:nrow(var_inf))[-all_groups[-1]]
    inf_group <- as.data.frame(
      matrix(
        data = NA,
        nrow = length(singles) + colu, ncol = 2
      )
    )

    for (i in seq_along(singles)) {
      inf_group[i, 1] <- as.character(var_inf[singles, 1][i])
      inf_group[i, 2] <- var_inf[singles, 2][i]
    }

    for (i in seq_len(colu)) {
      inf_group[length(singles) + i, 1] <- as.character(names(mat_im)[i])
      if (length(nrow(mat_im)) > 0) {
        inf_group[length(singles) + i, 2] <- sum(mat_im[, i], na.rm = TRUE)
      } else {
        inf_group[length(singles) + i, 2] <- sum(mat_im, na.rm = TRUE)
      }
    }

    names(inf_group) <- names(var_inf)
    var_inf <- inf_group[order(inf_group[, 2]), ]
  }

  vars_plot <- min(20, nrow(var_inf), na.rm = TRUE)

  # The last variables are the ones with more influence
  max_var <- nrow(var_inf)
  min_var <- max_var + 1 - vars_plot

  # Calculate and show bars plots
  rel_influence_plot <- bars_fun(
    var_inf,
    min_var,
    max_var
  )

  rel_influence_plot <- rel_influence_plot +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue1", colour = "black")

  return(rel_influence_plot)
}

# Function to calculate PDP 1D plot
pdp1d_fun <- function(
  model_res_fit,
  data_type,
  points_pd2,
  x_dp3,
  x_dp2,
  target
) {
  brt_model <- model_res_fit$model
  min_train <- model_res_fit$train_y[1]
  max_train <- model_res_fit$train_y[2]
  base_data <- model_res_fit$data_in

  if ((is.null(data_type) || data_type != 2)) {
    train_data <- base_data[base_data[, 1] >= min_train & base_data[, 1] <= max_train, ]
  } else {
    train_data <- base_data[-model_res_fit$positions, ]
  }

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
        x_dp2, ": %{x}<br>",
        target, ": %{y}",
        "<extra></extra>"
      )
    )

    pd_plot <- pd_plot %>%
    plotly::layout(
      xaxis = list(title = x_dp2, zeroline = FALSE),
      yaxis = list(
        title = target,
        zeroline = FALSE
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.25, y = -0.2),
      font = list(family = "Arial", size = 20),
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
        x_dp2, ": %{x}<br>",
        target, ": %{y}",
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
        size = 20
      ),
      name = x_dp3,  # Set the legend label to x_dp2
      hovertemplate = paste(
        x_dp3, ": %{x}<br>",
        target, ": %{y}",
        "<extra></extra>"
      )
    )

    pd_plot <- pd_plot %>%
    plotly::layout(
      xaxis = list(
        title = x_dp2,
        zeroline = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = target,
        zeroline = FALSE
      ),
      xaxis2 = list(
        title = x_dp3,
        overlaying = "x",
        side = "top",
        zeroline = FALSE,
        showgrid = FALSE
      ),
      yaxis2 = list(
        showticklabels = FALSE,
        zeroline = FALSE
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.33, y = -0.2),
      font = list(family = "Arial", size = 20),
      margin = list(l = 100, r = 30, t = 70, b = 70)
    )
  }

  return(pd_plot)
}

# Function to calculate PDP 2D plot
pdp2d_fun <- function(
  data_type,
  model_res_fit,
  points_pd2,
  x_dp2,
  x_dp3,
  target
) {
  brt_model <- model_res_fit$model
  min_train <- model_res_fit$train_y[1]
  max_train <- model_res_fit$train_y[2]
  base_data <- model_res_fit$data_in

  if ((is.null(data_type) || data_type != 2)) {
    train_data <- base_data[
      base_data[, 1] >= min_train & base_data[, 1] <= max_train,
    ]
  } else {
    train_data <- base_data[-model_res_fit$positions, ]
  }

  # Check if there is any data
  if (is.null(brt_model)) {
    return(NULL)
  }

  # Calculate values for plotting partial dependence plot 2D
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

  # Order the values for plotting in a matrix
  n_rows <- length(unique(heat_p[, 1]))
  n_cols <- length(unique(heat_p[, 2]))
  heat_data_1 <- matrix(NA, n_rows, n_cols)
  heat_data_3 <- matrix(NA, n_rows, n_cols)
  columns <- match(unique(heat_p[, 2]), heat_p[, 2])

  # Tranform colums 1 and 3 in two matrices
  for (j in seq_along(columns)) { # Tranform colums 1 and 3 in two matrices
    if (j == length(columns)) { # Set the length of this column
      end <- length(heat_p[, 2]) - columns[j] + 1
    } else {
      end <- columns[j + 1] - columns[j]
    }
    heat_data_1[1:end, j] <- heat_p[columns[j]:(end + columns[j] - 1), 1]
    heat_data_3[1:end, j] <- heat_p[columns[j]:(end + columns[j] - 1), 3]
  }

  # Move down columns if they are not in the correct position
  for (i in seq_len(nrow(heat_data_3))) {
    min_row <- min(heat_data_1[i, ], na.rm = TRUE)
    for (j in seq_len(ncol(heat_data_3))) {
      if (!is.na(heat_data_1[i, j]) && (heat_data_1[i, j] > min_row)) {
        heat_data_1[
          2:nrow(heat_data_3), j
        ] <- heat_data_1[
          1:nrow(heat_data_3) - 1, j
        ]
        heat_data_1[1, j] <- NA
        heat_data_3[
          2:nrow(heat_data_3), j
        ] <- heat_data_3[
          1:nrow(heat_data_3) - 1, j
        ]
        heat_data_3[1, j] <- NA
      }
    }
  }

  plot_data <- heat_data_3

  min_x <- min(heat_p[, 2])
  max_x <- max(heat_p[, 2])
  min_y <- min(heat_p[, 1])
  max_y <- max(heat_p[, 1])

  text_x <- c(
    toString(round(min_x + 0.25 * (max_x - min_x), digits = 1)),
    toString(round(min_x + 0.50 * (max_x - min_x), digits = 1)),
    toString(round(min_x + 0.75 * (max_x - min_x), digits = 1))
  )
  text_y <- c(
    toString(round(min_y + 0.25 * (max_y - min_y), digits = 1)),
    toString(round(min_y + 0.50 * (max_y - min_y), digits = 1)),
    toString(round(min_y + 0.75 * (max_y - min_y), digits = 1))
  )

  # Draw partial dependence plot 2D
  plot_2d <- plotly::plot_ly(
    z = plot_data,
    showscale = TRUE,
    hoverinfo = "z",
    hoverlabel = list(bgcolor = "white", font = list(size = 10)),
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
      )
    )

  return(plot_2d)
}

# Function to calculate PDP 3D plot
pdp3d_fun <- function(
  model_res_fit,
  data_type,
  points_pd2,
  x_dp2,
  x_dp3,
  target
) {
  brt_model <- model_res_fit$model
  min_train <- model_res_fit$train_y[1]
  max_train <- model_res_fit$train_y[2]
  base_data <- model_res_fit$data_in

  if ((is.null(data_type) || data_type != 2)) {
    train_data <- base_data[
      base_data[, 1] >= min_train & base_data[, 1] <= max_train,
      ]
  } else {
    train_data <- base_data[-model_res_fit$positions, ]
  }

  if (is.null(brt_model)) {
    return(NULL)
  } # Check if there is any data

  # Calculate values for plotting
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

  # Order the values for plotting in a matrix
  n_rows <- length(unique(heat_p[, 1]))
  n_cols <- length(unique(heat_p[, 2]))
  heat_data_1 <- matrix(NA, n_rows, n_cols)
  heat_data_3 <- matrix(NA, n_rows, n_cols)
  columns <- match(unique(heat_p[, 2]), heat_p[, 2])
  for (j in seq_along(columns)) { # Tranform colums 1 and 3 in two matrices
    if (j == length(columns)) { # Set the length of this column
      end <- length(heat_p[, 2]) - columns[j] + 1
    } else {
      end <- columns[j + 1] - columns[j]
    }
    heat_data_1[1:end, j] <- heat_p[columns[j]:(end + columns[j] - 1), 1]
    heat_data_3[1:end, j] <- heat_p[columns[j]:(end + columns[j] - 1), 3]
  }

  # Move down columns if they are not in the correct position
  for (i in seq_len(nrow(heat_data_3))) {
    min_row <- min(heat_data_1[i, ], na.rm = TRUE)
    for (j in seq_len(ncol(heat_data_3))) {
      if (!is.na(heat_data_1[i, j]) && (heat_data_1[i, j] > min_row)) {
        heat_data_1[
          2:nrow(heat_data_3), j
        ] <- heat_data_1[
          1:nrow(heat_data_3) - 1, j
        ]
        heat_data_1[1, j] <- NA
        heat_data_3[
          2:nrow(heat_data_3), j
        ] <- heat_data_3[
          1:nrow(heat_data_3) - 1, j
        ]
        heat_data_3[1, j] <- NA
      }
    }
  }

  # Spin the matrix for better plotting
  max_pos <- which(heat_data_3 == max(heat_data_3, na.rm = TRUE))[1]
  max_col <- round(0.49 + (max_pos / n_rows))
  max_row <- max_pos - n_rows * (max_col - 1)
  plot_data <- heat_data_3

  min_x <- min(heat_p[, 2])
  max_x <- max(heat_p[, 2])
  min_y <- min(heat_p[, 1])
  max_y <- max(heat_p[, 1])

  text_x <- c(
    toString(round(min_x + 0.25 * (max_x - min_x), digits = 1)),
    toString(round(min_x + 0.50 * (max_x - min_x), digits = 1)),
    toString(round(min_x + 0.75 * (max_x - min_x), digits = 1))
  )
  text_y <- c(
    toString(round(min_y + 0.25 * (max_y - min_y), digits = 1)),
    toString(round(min_y + 0.50 * (max_y - min_y), digits = 1)),
    toString(round(min_y + 0.75 * (max_y - min_y), digits = 1))
  )

  # If the maximum is in the wrong half, spin all the data
  if (max_row >= n_rows * 0.50) {
    for (j in seq_len(n_cols)) {
      i <- 1:n_rows
      plot_data[i, j] <- heat_data_3[1 + n_rows - i, j]
    }
    text_y <- c(
      toString(round(min_y + 0.75 * (max_y - min_y), digits = 1)),
      toString(round(min_y + 0.50 * (max_y - min_y), digits = 1)),
      toString(round(min_y + 0.25 * (max_y - min_y), digits = 1))
    )
    heat_data_3 <- plot_data
  }

  # If the maximum is in the wrong half, spin all the data
  if (max_col >= n_cols * 0.50) {
    for (i in seq_len(n_rows)) {
      j <- 1:n_cols
      plot_data[i, j] <- heat_data_3[i, 1 + n_cols - j]
    }
    text_x <- c(
      toString(round(min_x + 0.75 * (max_x - min_x), digits = 1)),
      toString(round(min_x + 0.50 * (max_x - min_x), digits = 1)),
      toString(round(min_x + 0.25 * (max_x - min_x), digits = 1))
    )
  }

  # Draw partial dependence plot 3D
  plot_3d <- plotly::plot_ly(
    z = plot_data,
    showscale = FALSE,
    hoverinfo = "z",
    hoverlabel = list(bgcolor = "white", font = list(size = 10))
  )
  plot_3d <- plot_3d %>%
    add_surface(
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
  return(plot_3d)
}

# Function for ggplot2 plots pre-loaded theme
pre_theme <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n = 9)

  color_background <- "white"
  color_grid_major <- palette[3]
  color_axis_text <- palette[6]
  color_axis_title <- palette[7]
  color_title <- palette[9]

  # Start construction of chart
  ggplot2::theme_bw(base_size = 9) +

    # Set the entire chart region to a light gray color
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = color_background,
        color = color_background
      )
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = color_background,
        color = color_background
      )
    ) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        color = color_background
      )
    ) +

    # Format the grid
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = color_grid_major,
        size = .25
      )
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::theme(
      axis.ticks = element_blank()
    ) +

    # Format the legend, but hide by default
    ggplot2::theme(
      legend.background = ggplot2::element_rect(
        fill = color_background
      )
    ) +
    ggplot2::theme(
      legend.text = ggplot2::element_text(
        size = 16,
        color = color_axis_title
      )
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        size = 16,
        color = color_axis_title
      )
    ) +
    ggplot2::theme(
      legend.key.height = unit(2, "cm")
    ) +

    # Set title and axis labels, and format these and tick marks
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = color_title,
        size = 10,
        vjust = 1.25
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 16,
        color = color_axis_text
      )
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = 16,
        color = color_axis_text
      )
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        size = 20,
        color = color_axis_title,
        vjust = 0
      )
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = 20,
        color = color_axis_title,
        vjust = 1.25
      )
    ) +

    # Plot margins
    ggplot2::theme(
      plot.margin = unit(c(0.35, 1, 0.3, 0.35), "cm")
    )
}

#------------------------------------------------------------------------------#
#-----------------------------Set values functions-----------------------------#
#------------------------------------------------------------------------------#
# Function to select prediction variables
select_pred_variables <- function(classes, datum, target, groups) {
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
  target_nu <- match(target, items)
  items <- items[-target_nu]
  groups_list <- create_variables_groups(classes, datum)
  mat_mo <<- groups_list[[1]]
  items <- groups_list[[2]]
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

# Function to change character variables to factor variables
change_char_to_factor <- function(
  influ,
  iter,
  data_type,
  values,
  positions,
  target_nu,
  target,
  inputs,
  min_train,
  max_train,
  min_test,
  max_test
) {
  influ$mean[, 1] <- influ$inf[, 1]
  influ$mean[, 2] <- 0

  # Calculate mean of influences for all the models
  for (i in seq_len(iter)) {
    influ$mean[, 2] <- influ$mean[, 2] + influ$inf[, i + 1]
  }

  influ$mean[, 2] <- influ$mean[, 2] / iter
  text <- NULL
  inputNam <- colnames(values$dat)
  inputsNu <- match(inputs, inputNam)
  excVars <- NULL

  if ((!is.null(data_type) && data_type == 2)) {
    train_max <- max(values$dat[-positions, target_nu], na.rm = TRUE)
    test_max <- max(values$dat[positions, target_nu], na.rm = TRUE)
    train_min <- min(values$dat[-positions, target_nu], na.rm = TRUE)
    test_min <- min(values$dat[positions, target_nu], na.rm = TRUE)
  } else {
    # Check range of inputs not factor (except "Year")
    for (i in seq_along(inputsNu)) {
      if (("factor" != class(values$dat[, inputsNu[i]])) && ("Year" != inputNam[inputsNu[i]])) {
        train_max <- max(values$dat[values$dat[, 1] >= min_train & values$dat[, 1] <= max_train, inputsNu[i]], na.rm = TRUE)
        test_max <- max(values$dat[values$dat[, 1] >= min_test & values$dat[, 1] <= max_test, inputsNu[i]], na.rm = TRUE)
        train_min <- min(values$dat[values$dat[, 1] >= min_train & values$dat[, 1] <= max_train, inputsNu[i]], na.rm = TRUE)
        test_min <- min(values$dat[values$dat[, 1] >= min_test & values$dat[, 1] <= max_test, inputsNu[i]], na.rm = TRUE)

        if ((!is.null(data_type) && data_type == 2)) {
          train_max <- max(values$dat[-positions, inputsNu[i]], na.rm = TRUE)
          test_max <- max(values$dat[positions, inputsNu[i]], na.rm = TRUE)
          train_min <- min(values$dat[-positions, inputsNu[i]], na.rm = TRUE)
          test_min <- min(values$dat[positions, inputsNu[i]], na.rm = TRUE)
        }

        if ((train_max < test_max) || (test_min < train_min)) {
          excVars <- paste(excVars, inputs[i], sep = " ")
        }
      }
    }
    target_nu <- match(target, colnames(values$dat))
    train_max <- max(values$dat[values$dat[, 1] >= min_train & values$dat[, 1] <= max_train, target_nu], na.rm = TRUE)
    test_max <- max(values$dat[values$dat[, 1] >= min_test & values$dat[, 1] <= max_test, target_nu], na.rm = TRUE)
    train_min <- min(values$dat[values$dat[, 1] >= min_train & values$dat[, 1] <= max_train, target_nu], na.rm = TRUE)
    test_min <- min(values$dat[values$dat[, 1] >= min_test & values$dat[, 1] <= max_test, target_nu], na.rm = TRUE)
  }

  # Check range of target variable
  if ((train_max < test_max) || (test_min < train_min)) {
    excVars <- paste(excVars, target, sep = " ")
  }
  text <- paste("Warning: the range for test data of some variables (", excVars, ") exceeds the range for input training data")
  if (is.null(excVars)) {
    text <- NULL
  }
  return(list(influ$mean, text))
}

# Function to change character variables to factor variables
factFun <- function(dat) {
  datum <- dat
  for (i in seq_len(ncol(datum))) {
    if (class(datum[, i])[1] == "character") {
      factorVar <- as.factor(datum[, i])
      namesVars <- names(datum)
      if (i == 1) {
        datum <- cbind(factorVar, datum[, (i + 1):ncol(datum)])
      } else if (i == ncol(datum)) {
        datum <- cbind(datum[, 1:(i - 1)], factorVar)
      } else {
        datum <- cbind(datum[, 1:(i - 1)], factorVar, datum[, (i + 1):ncol(datum)])
      }
      names(datum) <- namesVars
    }
  }
  return(datum)
}

# Function to build model
model_fun <- function(
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
  testPerc2
) {
  if ((is.null(data_type) || data_type != 2)) {
    set.seed(iter)
  }

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
    (generics::accuracy(pred_train,obs_train)[3]),
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
  influ$inf[, iter + 1] <- data.frame(rel.inf = var_inf[, 2])

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
    test_perc = testPerc2,
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
    datum[, 2] <- as.numeric(format(datum[, 1], "%m", tz = "GMT"))
  }

  return(datum)
}