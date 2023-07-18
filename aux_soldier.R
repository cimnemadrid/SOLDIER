
column(
  12,

  # Save columns of variables for plotting time series
  if (aux_soldier == "vars_time_sieries_plot") {
    vars_left <- select_variables(datum, input$vars_left)
    vars_left <<- vars_left
    sel_vars <- c(vars_left, input$vars_right)
    var_cols <- which(c(names(values$dat), "Residual") %in% sel_vars)
  },

  # Generate graph output for time series
  if (aux_soldier == "generate_time_series_plot") {
    # Save variables to plot in the left axis
    vars <- paste(vars_left)

    # Save names to show. If there are less than 5 names store all
    if (length(vars_left) < 5) {
    y_labels <- paste(vars_left)
    } else {
      y_labels <- paste(
        vars_left[1],
        vars_left[2],
        vars_left[3],
        "...",
        sep = ", "
      )
    }

    # Save variables to plot in the right axis
    vars2 <- paste(input$vars_right)

    # Save names to show. If there are less than 5 names store all
    if (length(input$vars_right) < 5) {
    y_labels2 <- paste(input$vars_right)
    } else {
      y_labels2 <- paste(
        input$vars_right[1],
        input$vars_right[2],
        input$vars_right[3],
        "...",
        sep = ", "
      )
    }

    # Graph for left variables
    print("Calculating time-series graph")

    # Calculate time series plot
    time_plot <- generate_time_plot(
      data_sort,
      1200,
      vars,
      y_labels,
      vars2,
      y_labels2,
      input$colours2
    )
  },

  # Menu for selecting plot type
  if (aux_soldier == "menu_plot_type") {
    option <- list(
      "Show scatterplot" = 2,
      "Show scatterplot 4D" = 3,
      "Show time series plot" = 1
    )

    radioButtons(
      inputId = "plotType",
      label = NULL,
      choices = option,
      selected = 0
    )
  },

  # Warning message
  if (aux_soldier == "warning_msg") {
    # Check if the first variable has the adequate class
    if (class(values$dat[, 1])[1] != "Date") {
      if (class(values$dat[, 1])[1] != "POSIXct") {
        showModal(
          modalDialog(
            title = "Warning",
            "First variable must be date type",
            size = c("s"),
            easyClose = TRUE
          )
        )
      }
    }
  },

  # Graph for residual of model fitting (date-data)
  if (aux_soldier == "model_fit_graph") {
    if (is.null(values$dat)) {
      return(NULL)
    } # Check if there is any data
    if (compatible == FALSE) {
      return(NULL)
    }

    if (is.null(input$train_test) || input$train_test == 2) {
      # Set test/train periods by percentage
      if (is.null(input$test_perc)) {
        i_test_perc <- c(75, 100)
      } else {
        i_test_perc <- input$test_perc
      }

      min_date <- values$dat[, 1][1]
      max_date <- values$dat[, 1][length(values$dat[, 1])]
      date_diff <- max_date - min_date
      test_perc_diff <- i_test_perc[2] - i_test_perc[1]
      train_days <- round(date_diff * i_test_perc[1] / 100) - 2
      test_days <- round(date_diff * test_perc_diff / 100) - 2

      # Set test period before or after train period
      if (i_test_perc[1] >= (100 - i_test_perc[2])) {
        min_train <- min_date
        max_train <- min_date + train_days
        max_test <- max_train + test_days
      } else {
        min_train <- min_date
        max_train <- max_date
        max_test <- min_date + train_days + test_days
      }
    } else {
      min_train <- input$train_years[1]
      max_train <- input$train_years[2]
      max_test <- input$test_years[2]
    }

    start_train <- min_train
    end_train <- max_train
    end_test <- max_test

    # "model_res_fit": model with parameters
    graph_data <- model_res_fit()$data_out

    graph_data[, 3] <- 0
    graph_data[, 4] <- 0

    # Calculate mean of errors for all the models
    for (i in seq_len(models$num)) {
      graph_data[, 3] <- graph_data[, 3] + model_p$pre[, i]
      graph_data[, 4] <- graph_data[, 4] + model_e$error[, i]
    }
    graph_data[, 3] <- graph_data[, 3] / models$num
    graph_data[, 4] <- graph_data[, 4] / models$num

    names(graph_data)[1] <- "Date"
    names(graph_data)[2] <- "Observation"
    names(graph_data)[3] <- "Prediction"
    names(graph_data)[4] <- "Error"

    data_sort <- xts::xts(graph_data[, 2:4], order.by = graph_data[, 1])

    data_sort_resi <- xts::xts(graph_data[, 4], order.by = graph_data[, 1])
    names(data_sort_resi) <- "Error"
    results$residual <- data_sort_resi

    # Show graph
    print("Calculating model fitting and residual graph")

    vars <- as.character(c("Observation", "Prediction", "Error"))

    # Show graph
    print("Calculating model fitting and residual graph")

    # Calculate time series plot
    res_graph <- generate_time_plot_prediction(
      data_sort,
      results,
      start_train,
      end_train,
      end_test,
      400,
      FALSE
    )
  },

  # Menu for choosing test/train options for the new model
  if (aux_soldier == "train_test_options") {
    radioButtons(
      "train_test",
      label = NULL,
      choices = list(
        "Choose test data by date" = 1,
        "Choose test data by percentage" = 2
      ),
      selected = 1
    )
  },

  # Menu for choosing train periods for the new model
  if (aux_soldier == "train_periods") {
    # "values": dataframe with new data
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) || is.null(input$train_test)) {
      return(NULL)
    }

    # Adapt options to the user choices
    if ((input$train_test != 1) || input$info1) {
      return(NULL)
    }

    dates <- as.Date(format(datum[, 1], "%Y-%m-%d"), origin = lubridate::origin)

    # Initial testing size
    base_test <- 0.25 * (max(dates, na.rm = TRUE) - min(dates, na.rm = TRUE))
    end_date_train <- max(dates, na.rm = TRUE) - base_test
    label_years <- h5("Training and testing period")
    initial_date_train <- min(dates)
    target_nu <- match(input$target, colnames(values$dat))

    # Search the first no NA in the target variable
    if ((!is.na(target_nu)) && any(is.na(values$dat[, target_nu]))) {
      pos <- match(NA, match(values$dat[, target_nu], NA))
      initial_date_train <- dates[pos]
    }

    train_start_date <- reactive({
      if (
        is.null(
          input$train_years[1]
        ) || !lubridate::is.Date(
          input$train_years[1]
        ) || input$train_years[1] < initial_date_train
      ) {
        initial_date_train
      } else {
        input$train_years[1]
      }
    })

    train_end_date <- reactive({
      if (is.null(input$train_years[2]) || !lubridate::is.Date(input$train_years[2])) {
        end_date_train
      } else {
        input$train_years[2]
      }
    })

    dateRangeInput(
      inputId = "train_years",
      label = label_years,
      start = train_start_date(),
      end = train_end_date(),
      min = initial_date_train,
      max = max(dates)
    )
  },

  # Menu for choosing test period for the new model
  if (aux_soldier == "test_period") {
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) || is.null(input$train_test)) {
      return(NULL)
    }

    # Adapt options to the user choices
    if ((input$train_test != 1) || input$info1) {
      return(NULL)
    }

    dates <- as.Date(format(datum[, 1], "%Y-%m-%d"), origin = lubridate::origin)

    # Initial testing size
    base_test <- 0.25 * (max(dates, na.rm = TRUE) - min(dates, na.rm = TRUE))
    initial_date_test <- max(dates, na.rm = TRUE) - base_test + 1

    test_start_date <- reactive({
      if (is.null(input$test_years[1]) || !is.Date(input$test_years[1])) {
        initial_date_test
      } else {
        input$test_years[1]
      }
    })

    test_end_date <- reactive({
      if (
        is.null(
          input$test_years[2]
        ) || !lubridate::is.Date(
          input$test_years[2]
        ) || input$test_years[2] > max(dates)) {
        max(dates)
      } else {
        input$test_years[2]
      }
    })

    dateRangeInput(
      inputId = "test_years",
      label = NULL,
      start = test_start_date(),
      end = test_end_date(),
      min = min(dates),
      max = max(dates)
    )
  },

  # Menu for choosing percentage test period for the new model
  if (aux_soldier == "test_period_perc") {
    if (is.null(values$dat) || is.null(input$train_test)) {
      return(NULL)
    } # Check if there is any data
    if ((input$train_test == 1) || input$info1) {
      return(NULL)
    } # Adapt options to the user choices
    sliderInput(
      "test_perc",
      label = h5("Testing period:"),
      min = 0,
      max = 100,
      value = c(75, 100)
    )
  },

  # Check train/test data
  if (aux_soldier == "check_train_test") {
    print("Checking train/test data")
    if (is.null(input$test_perc)) { # Set the test/train percentages
      i_test_perc <- c(75, 100)
    } else {
      i_test_perc <- input$test_perc
    }
    if (!is.null(input$test_years[1])) { # Check if test/train periods overlap
      if ((input$train_years[1] < input$test_years[1]) &&
          (input$train_years[2] >= input$test_years[1])) {
        showModal(
          modalDialog(
            title = "Warning",
            "Testing period overlap training period",
            size = c("s"),
            easyClose = TRUE
          )
        )

        return(NULL)
      }

      if ((input$test_years[1] < input$train_years[1]) &&
          (input$test_years[2] >= input$train_years[1])) {
        showModal(
          modalDialog(
            title = "Warning",
            "Testing period overlap training period",
            size = c("s"),
            easyClose = TRUE
          )
        )

        return(NULL)
      }
    }

    # Set test/train periods by percentage
    if (is.null(input$trainTest) || input$trainTest == 2) {
      min_date <- datum[, 1][1]
      max_date <- datum[, 1][length(datum[, 1])]
      date_range <- max_date - min_date
      first_days <- round(date_range * (i_test_perc[1]) / 100)
      second_days <- round(date_range * (i_test_perc[2] - i_test_perc[1]) / 100)

      # Set test period before or after train period
      if (i_test_perc[1] >= (100 - i_test_perc[2])) {
        min_train <- datum[1, 1]

        row1 <- which(
          as.Date(
            min_date + first_days,
            origin = lubridate::origin
          ) <= as.Date(
            format(datum[, 1]),
            "%Y-%m-%d",
            origin = lubridate::origin
          )
        )[1]

        max_train <- datum[row1, 1]
        min_test <- datum[row1 + 1, 1]

        row2 <- which(
          as.Date(
            min_date + first_days + second_days,
            origin = lubridate::origin
          ) <= as.Date(
            format(datum[, 1]),
            "%Y-%m-%d",
            origin = lubridate::origin
          )
        )[1]

        max_test <- datum[row2, 1]
      } else {
        row1 <- which(
          as.Date(
            min_date + first_days + second_days,
            origin = lubridate::origin
          ) <= as.Date(
            format(datum[, 1]),
            "%Y-%m-%d",
            origin = lubridate::origin
          ))[1] + 1

        min_train <- datum[row1, 1]

        max_train <- datum[length(datum[, 1]), 1]

        row2 <- which(
          as.Date(
            min_date + first_days,
            origin = lubridate::origin
          ) <= as.Date(
            format(datum[, 1]),
            "%Y-%m-%d",
            origin = lubridate::origin
          ))[1]

        min_test <- datum[row2, 1]
        max_test <- datum[row1 - 1, 1]
      }
    } else { # Set test/train periods by selected dates
      row1 <- which(
        as.Date(input$train_years[1]) <= as.Date(format(datum[, 1]),
        "%Y-%m-%d",
        origin = lubridate::origin)
      )[1]

      min_train <- datum[row1, 1]

      row2 <- which(
        as.Date(input$train_years[2]) >= as.Date(format(datum[, 1]),
        "%Y-%m-%d",
        origin = lubridate::origin)
      )[1]

      row2 <- row2[length(row2)]

      max_train <- datum[row2, 1]

      row1 <- which(
        as.Date(input$test_years[1]) <= as.Date(format(datum[, 1]),
        "%Y-%m-%d",
        origin = lubridate::origin)
      )[1]

      min_test <- datum[row1, 1]

      row2 <- which(
        as.Date(input$test_years[2]) <= as.Date(format(datum[, 1]),
        "%Y-%m-%d",
        origin = lubridate::origin)
      )[1]

      max_test <- datum[row2, 1]
    }

    positions <- NULL
    train_data <- datum[datum[, 1] >= min_train & datum[, 1] <= max_train, ]
    test_data <- datum[datum[, 1] >= min_test & datum[, 1] <= max_test, ]
    scat$ini <- which(datum[, 1] == min_train)
    scat$end <- which(datum[, 1] == max_train)

    # Check if there is a valid enddate for train period
    if (length(scat$end) < 1) {
      showModal(
        modalDialog(
          title = "Warning",
          "End date for training is not valid",
          size = c("s"),
          easyClose = TRUE
        )
      )

      return(NULL)
    }
  }
)