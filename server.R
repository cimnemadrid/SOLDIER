
#------------------------------------------------------------------------------#
#--------------------------Start shiny and variables---------------------------#
#------------------------------------------------------------------------------#

# Size limit of the files that can be loaded
options(shiny.maxRequestSize = 50 * 1024^2)

shiny::shinyServer(function(input, output, session) {
  aux_soldier <- file.exists("aux_soldier.R")

  # Start reactive variables
  values <- reactiveValues(
    dat = NULL,
    only_train_hull = FALSE,
    back_colour_scatter_plot = FALSE,
    ignore = FALSE,
    points_pd2 = 10,
    train_data = NULL,
    test_data = NULL
  )

  scat <- reactiveValues(
    ini = NULL,
    end = NULL
  )

  influ <- reactiveValues(
    inf = NULL,
    less = NULL,
    mean = NULL
  )

  model_p <- reactiveValues(pre = NULL)

  model_e <- reactiveValues(error = NULL)

  results <- reactiveValues(
    obb = NULL,
    error = NULL,
    residual = NULL,
    train_residual = NULL,
    test_residual = NULL
  )

  models <- reactiveValues(num = NULL)

  old_names <- NULL

  compatible <- "Checking"

  #----------------------------------------------------------------------------#
  #---------------------TabItem 1: Select files and images---------------------#
  #----------------------------------------------------------------------------#

  output$i_data_type <- renderUI({
    if (aux_soldier) {
      radioButtons(
        inputId = "data_type",
        label = NULL,
        choices = list("Time series data" = 1, "Other data" = 2),
        selected = character(0)
      )
    } else {
      return(NULL)
    }
  })

  # Menu for selecting plot type
  output$iPlot <- renderUI({
    # Adapt options to the existence of "aux_soldier.R"
    if (aux_soldier && (is.null(input$data_type) || input$data_type != 2)) {
      aux_soldier <- "menu_plot_type"
      source("aux_soldier.R", local = TRUE)$value
    } else {
      radioButtons(
        inputId = "plotType",
        label = NULL,
        choices = list("Show scatterplot" = 2, "Show scatterplot 4D" = 3),
        selected = 0
      )
    }
  })

  # Load new data file
  observeEvent(input$file_new_data, {
    # "file_new_data": file with new data
    in_file <- input$file_new_data

    # Check if there is any data
    if (is.null(in_file)) {
      return(NULL)
    }

    # Search for correct extension
    csv_ext <- max(0, grep("csv", in_file$datapath))
    rds_ext <- max(0, grep("rds", in_file$datapath))
    excel <- max(
      0,
      grep("xlsx", in_file$datapath),
      grep("xls", in_file$datapath)
    )

    print("Loading file")

    if (csv_ext == 1) { # Check if the file is CSV
      values$dat <- read.csv(in_file$datapath)

      # Check if the CSV file has the adequate class
      if (class(values$dat[]) != "data.frame") {
        showModal(
          modalDialog(
            title = "Warning",
            "Loaded file is not a data frame. Close the app and run it again",
            size = c("s"),
            easyClose = TRUE
          )
        )
        quit(save = "default")
      }

      values$dat <- as.data.frame(values$dat)

      if (input$data_type == 1) { ### Adapt options to the date based data
        values$dat[, 1] <- as.POSIXct(
          values$dat[, 1],
          tz = "UTC",
          format = "%d/%m/%Y %H:%M"
        )
      }

      if ((is.null(input$data_type) || input$data_type != 2)) {
        # Warning message
        aux_soldier <- "warning_msg"
        source("aux_soldier.R", local = TRUE)$value
      }
    }else if (rds_ext == 1) { # Check if the file is RDS
      values$dat <- readRDS(in_file$datapath)

      # Check if the RDS file has the adequate class
      if (class(values$dat[]) != "data.frame") {
        showModal(
          modalDialog(
            title = "Warning",
              "Loaded file is not a data frame. Close the app and run it again",
              size = c("s"),
              easyClose = TRUE
            )
          )
        quit(save = "default")
      }

      values$dat <- as.data.frame(values$dat)

      if ((is.null(input$data_type) || input$data_type != 2)) {
        # Warning message
        aux_soldier <- "warning_msg"
        source("aux_soldier.R", local = TRUE)$value
      }
    } else if (excel == 1) { # Check if the file is EXCEL
      values$dat <- as.data.frame(
        read_excel(
          in_file$datapath,
          col_names = TRUE,
          guess_max = 20000000
        )
      )
      classes <- identify_classes(values$dat)

      # Search for wrongly classified columns
      values$dat <- search_wrong_class_columns(values$dat, classes)
      if ((is.null(input$data_type) || input$data_type != 2)) {
        # Warning message
        aux_soldier <- "warning_msg"
        source("aux_soldier.R", local = TRUE)$value
      }
    } else {
      showModal(
        modalDialog(
          title = "Warning",
          "Must be a .CSV, .RDS, XLSX or .XLS file",
          size = c("s"),
          easyClose = TRUE
        )
      )
    }

    if (input$data_type == 1) {
      # Create "Month" and "Year" columns (if "Month" doesn't exist)
      n_initial_columns <- ncol(values$dat)
      datum <- values$dat
      datum <- generate_month_year(datum, n_initial_columns)
      values$dat <- datum
    }

    # Change character variables to factor variables
    datum <- factFun(values$dat)

    # Initialize results$residual
    results$residual <- vector("numeric", length = nrow(datum))

    values$dat <- datum
  })

  # Load front image and help image (for both kinds of plots)
  output$frontImage <- renderImage(
    {
      return(list(
        src = "www/FrontImage.jpg",
        contentType = "image/jpg",
        alt = "frontImage",
        width = 850,
        height = 520
      ))
    },
    deleteFile = FALSE
  )

  output$image1 <- renderImage(
    {
      in_file <- input$fileImg
      if (is.null(in_file)) {
        return(list(
          src = "www/FrontImage.jpg",
          contentType = "image/jpg",
          alt = "image",
          width = 1,
          height = 1
        ))
      }
      return(list(
        src = in_file$datapath,
        contentType = "image/*",
        alt = "image",
        width = 890
      ))
    },
    deleteFile = FALSE
  )

  output$image2 <- renderImage(
    {
      in_file <- input$fileImg2
      if (is.null(in_file)) {
        return(list(
          src = "www/FrontImage.jpg",
          contentType = "image/jpg",
          alt = "image",
          width = 1,
          height = 1
        ))
      }
      return(list(
        src = in_file$datapath,
        contentType = "image/*",
        alt = "image",
        width = 890
      ))
    },
    deleteFile = FALSE
  )

  output$image3 <- renderImage(
    {
      in_file <- input$fileImg3
      if (is.null(in_file)) {
        return(list(
          src = "www/FrontImage.jpg",
          contentType = "image/jpg",
          alt = "image",
          width = 1,
          height = 1
        ))
      }
      return(list(
        src = in_file$datapath,
        contentType = "image/*",
        alt = "image",
        width = 890
      ))
    },
    deleteFile = FALSE
  )

  output$image4 <- renderImage(
    {
      in_file <- input$fileImg4
      if (is.null(in_file)) {
        return(list(
          src = "www/FrontImage.jpg",
          contentType = "image/jpg",
          alt = "image",
          width = 1,
          height = 1
        ))
      }
      return(list(
        src = in_file$datapath,
        contentType = "image/*",
        alt = "image",
        width = 890
      ))
    },
    deleteFile = FALSE
  )

  #----------------------------------------------------------------------------#
  #------------------------TabItem 1: Time series plot-------------------------#
  #----------------------------------------------------------------------------#

  # Menus for selecting left variables
  output$i_variables_left <- renderUI({
    datum <- values$dat
    # Check if there is any data
    if (is.null(datum) || is.null(input$plotType)) {
      return(HTML("Please load some data file and select plot"))
    }
    if (aux_soldier) {
      # Calculate classes for columns
      classes <- identify_classes(datum)
      num_class <- which("numeric" == classes)
      items <- sort(names(datum)[num_class])

      # Find groups of variables
      groups_list <- create_variables_groups(classes, datum)
      mat_gr <<- groups_list[[1]]
      items <- groups_list[[2]]

      if (sum(results$residual[!is.na(results$residual)]) != 0) {
        items <- c(items, "Residual")
      }

      pickerInput(
        "vars_left",
        "Left Axis",
        items,
        multiple = TRUE,
        options = list(
          "actions-box" = TRUE
        )
      )
    }
  })

  # Menus for selecting right and colour variables
  output$i_back_colour_time_plot <- renderUI({
    if (is.null(values$dat) || is.null(input$plotType)) {
      return(NULL)
    } # Check if there is any data
    if (input$plotType != 1) {
      return(NULL)
    }
    if (aux_soldier) {
      checkboxInput("colours2", label = "Alternative color", value = FALSE)
    } else {
      return(NULL)
    }
  })
  output$i_variables_right <- renderUI({
    # "values": dataframe with new data
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum)) {
      return(NULL)
    }

    if (aux_soldier) {
      items <- sort(names(datum))

      if (sum(results$residual[!is.na(results$residual)]) != 0) {
        items <- c(items, "Residual")
      }

      pickerInput(
        "vars_right",
        "Right Axis",
        items[-1],
        multiple = TRUE, options = list(`actions-box` = TRUE))
    }
  })

  # Save columns of variables for plotting time series
  var_cols <- eventReactive(input$refresh5, {
    showModal(
      modalDialog(
        title = "Drawing time series plot",
        footer = "Click anywhere to close",
        size = c("s"),
        easyClose = TRUE
      )
    )
    datum <- values$dat
    if (is.null(datum)) {
      return(NULL)
    } # Check if there is any data
    if (aux_soldier) {
      aux_soldier <- "vars_time_sieries_plot"
      source("aux_soldier.R", local = TRUE)$value
      var_cols
    }
  })

  # Generate graph output for time series
  output$time_graph <- renderPlotly({
    if (is.null(values$dat)) {
      return(NULL)
    } # Check if there is any data
    if (aux_soldier) {
      input$refresh5
      datum <- cbind(values$dat, as.data.frame(results$residual))
      names(datum)[ncol(datum)] <- "Residual"

      # Sort selected data by first variable
      data_sort <- xts::xts(datum[, var_cols()], order.by = datum[, 1])

      # Fix names when there is only one variable
      if (length(var_cols()) == 1) {
        names(data_sort) <- vars_left
      }
      isolate({
        # Search groups of variables
        vars_left <- select_variables(values$dat, input$vars_left)
        unique_vars <- unique(c(vars_left, input$vars_right))
        if (length(vars_left) < 1) {
          showModal(
            modalDialog(
              title = "Warning",
              "There are no variables on the left axis.",
              size = c("s")
            )
          )
        }
        if (length(unique_vars) > 8) {
          showModal(
            modalDialog(
              title = "Warning",
              "Too many variables, the number of variables that can be plotted
              simultaneously is limited to 8",
              size = c("s")
            )
          )
          time_plot <- NULL
        } else {
          aux_soldier <- "generate_time_series_plot"
          source("aux_soldier.R", local = TRUE)$value
        }
        time_plot
      })
    }
  })

  #----------------------------------------------------------------------------#
  #---------------------------TabItem 1: Scatterplot---------------------------#
  #----------------------------------------------------------------------------#

  # Let select horizontal, vertical and color variables for scatterplot
  output$x_var_scat <- renderUI({
    # "values": dataframe with new data
    datum <- values$dat

    # Check if there is any data
    if (
      is.null(datum) || is.null(input$plotType)
    ) {
      return(HTML("Please load some data file and select plot"))
    }

    nam <- names(datum)
    items <- sort(nam)

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    first <- which(items == nam[1])
    selectInput("x_scat", "Horizontal Axis", items, items[first])
  })

  output$y_var_scat <- renderUI({
    datum <- values$dat

    # Check if there is any data
    if (
      is.null(datum) || is.null(input$plotType) || (input$plotType == 4)
    ) {
      return(NULL)
    }

    # Calculate classes for columns and only add factor and numeric classes
    classes <- identify_classes(datum)
    num_class <- which("numeric" == classes)
    factor_class <- which("factor" == classes)
    items <- sort(names(datum)[c(num_class, factor_class)])

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    selectInput("y_scat", "Vertical Axis", items)
  })

  # Variable to colour the scatter plot
  output$i_color_scat <- renderUI({
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) || is.null(input$plotType)) {
      return(NULL)
    }

    if (input$plotType == 4) {
      return(NULL)
    }

    # Calculate classes for columns and only add factor and numeric classes
    classes <- identify_classes(datum)
    num_class <- which("numeric" == classes)
    items <- sort(names(datum)[num_class])

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    selectInput("color", "Colors", items)
  })

  # Only train option
  output$i_only_train_hull <- renderUI({
    if (!aux_soldier) {
      return(NULL)
    }

    datum <- values$dat

    ## Check if there is any data
    if (is.null(datum) || is.null(input$plotType) || input$plotType != 2) {
      return(NULL)
    }

    # Check if there is any train data
    if (is.null(values$train_data)) {
      return(NULL)
    }

    checkboxInput(
      inputId = "only_train_hull",
      label = "Convex hull only for training data",
      value = FALSE
    )
  })

  observeEvent(input$only_train_hull, {
    values$only_train_hull <- input$only_train_hull
  })

  # Background colour of the scatter plot
  output$i_back_colour_scatter_plot <- renderUI({
    # Check if there is any data
    if (is.null(values$dat) || is.null(input$plotType)) {
      return(NULL)
    }

    if (input$plotType != 2) {
      return(NULL)
    }

    if (aux_soldier) {
      checkboxInput(
        inputId = "back_colour_scatter_plot",
        label = "Alternative color",
        value = FALSE
      )
    } else {
      return(NULL)
    }
  })

  observeEvent(input$back_colour_scatter_plot, {
    values$back_colour_scatter_plot <- input$back_colour_scatter_plot
  })

  # Write message for scatterplot
  output$i_scat_message <- renderUI({
    # Check if there is any data
    if (is.null(values$dat)) {
      return(NULL)
    }

    scat_message <- mess_fun(input$color, values$dat)
    scat_message
  })


  # Drawing buttons
  output$i_draw_scat <- renderUI({
    # Check if there is any data
    if (is.null(values$dat)) {
      return(NULL)
    }

    if (is.null(input$plotType) || (input$plotType != 2)) {
      return(NULL)
    }

    actionButton("refresh6", label = "Draw/Refresh", icon = icon("signal"))
  })

  ref_plot6 <- eventReactive(input$refresh6, {
    showModal(modalDialog(title = "Drawing scatterplot",
                          footer = "Click anywhere to close",
                          size = c("s"),
                          easyClose = TRUE))
    refresh <- TRUE
  })

  # Plotting scatterplot
  output$scatter_plot <- renderPlotly({
    # Check if there is any data
    if (is.null(values$dat)) {
      return(NULL)
    }

    back_color <- "white"

    if (values$back_colour_scatter_plot) {
      back_color <- "darkgrey"
    }

    datum <- cbind(values$dat, as.data.frame(results$residual))
    names(datum)[ncol(datum)] <- "Residual"

    ini <- 1
    end <- nrow(datum)

    refresh <- ref_plot6()
    col_nam <- colnames(datum)
    point_size <- 8

    if (nrow(datum) > 2000) {
      point_size <- 6
    }

    # Check if it is the same dataset than for the last scatterplot
    if (!is.null(old_names)) {
      if (old_names[length(old_names)] != names(values$dat[ncol(values$dat)])) {
        old_names <<- names(values$dat)
        return(NULL)
      }
    }

    old_names <<- names(values$dat)

    x <- datum[, match(input$x_scat, col_nam)]
    y <- datum[, match(input$y_scat, col_nam)]

    # Draw plot
    isolate({
      plot <- plot_ly()

      if (values$only_train_hull) {
        datum_train <- cbind(
          values$train_data,
          as.data.frame(results$residual_train)
        )
        names(datum_train)[ncol(datum_train)] <- "Residual"
        x_hull <- datum_train[, match(input$x_scat, col_nam)]
        y_hull <- datum_train[, match(input$y_scat, col_nam)]

        # Combine x and y coordinates into a matrix
        points <- cbind(x_hull, y_hull)

        # Remove rows with NaN values
        points <- points[complete.cases(points), ]

        if (nrow(points) >= 3) {
          # Calculate the convex hull indices
          hull <- chull(points)

          plot <- add_trace(
            x = x_hull[hull],
            y = y_hull[hull],
            p = plot,
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "blue, 0.5",
            hovertemplate = "none"
          )
        }
      }

      plot <- add_trace(
        data = datum[ini:end, ],
        x = x,
        y = y,
        p = plot,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = point_size,
          color = datum[, match(input$color, col_nam)],
          colorbar = list(
            title = paste(input$color),
            titlefont = list(size = 18),
            tickfont = list(size = 14)
          ),
          colorscale = "Rainbow",
          showscale = TRUE
        ),
        hovertemplate = paste(
          input$x_scat, ": %{x}<br>",
          input$y_scat, ": %{y}<br>",
          input$color, ": ", datum[, match(input$color, col_nam)],
          "<extra></extra>"
        )
      )

      if (class(datum[, match(input$i_color_scat, col_nam)]) == "factor") {
        showModal(modalDialog(
          title = "Colors doesn't work with factor variables",
          NULL,
          size = c("s")
        ))
      }

      plot <- plot %>%
        layout(
          xaxis = list(
            title = input$x_scat,
            zeroline = FALSE,
            showgrid = TRUE,
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          yaxis = list(
            title = input$y_scat,
            zeroline = FALSE,
            showgrid = TRUE,
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2),
          plot_bgcolor = back_color,
          showlegend = FALSE
        )
    })

    # Display the plot
    plot
  })

  #----------------------------------------------------------------------------#
  #-------------------------TabItem 1: 4D scatterplot--------------------------#
  #----------------------------------------------------------------------------#

  # Let select horizontal, vertical and color variables for scatterplot
  output$x_var_scat4d <- renderUI({
     datum <- values$dat # "values": dataframe with new data

    # Check if there is any data
    if (is.null(datum) || is.null(input$plotType)) {
      return(HTML("Please load some data file and select plot"))
    }

    nam <- names(datum)
    items <- sort(nam)

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    first <- which(items == nam[1])
    selectInput("x_scat4d", "X Axis", items, items[first])
  })

  x_var <- eventReactive(input$refresh3, {
    x_var <- input$x_scat4d
    x_var
  })

  output$y_var_scat4d <- renderUI({
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) ||
        is.null(input$plotType) ||
        (input$plotType == 4)) {
      return(NULL)
    }

    # Calculate classes for columns
    classes <- identify_classes(datum)
    num_class <- which("numeric" == classes)
    items <- sort(names(datum)[num_class])

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    selectInput("y_scat4d", "Y Axis", items)
  })

  y_var <- eventReactive(input$refresh3, {
    y_var <- input$y_scat4d
    y_var
  })

  # Let select z axis variables for scatterplot 4D
  output$z_var_scat4d <- renderUI({
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) ||
        is.null(input$plotType) ||
        (input$plotType == 4)) {
      return(NULL)
    }

    # Calculate classes for columns
    classes <- identify_classes(datum)
    num_class <- which("numeric" == classes)
    items <- sort(names(datum)[num_class])

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    selectInput("z_scat4d", "Z Axis", items)
  })

  z_var <- eventReactive(input$refresh3, {
    z_var <- input$z_scat4d
    z_var
  })

  output$i_color_scat4d <- renderUI({
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) || is.null(input$plotType)) {
      return(NULL)
    }

    if (input$plotType == 4) {
      return(NULL)
    }

    # Calculate classes for columns
    classes <- identify_classes(datum)
    num_class <- which("numeric" == classes)
    items <- sort(names(datum)[num_class])

    if (sum(results$residual[!is.na(results$residual)]) != 0) {
      items <- c(items, "Residual")
    }

    selectInput("color4d", "Colors", items)
  })

  col_var <- eventReactive(input$refresh3, {
    col_var <- input$color4d
    col_var
  })

  # Write message for scatterplot 4D
  output$i_scat_message <- renderUI({
    # Check if there is any data
    if (is.null(values$dat)) {
      return(NULL)
    }

    scat_message <- mess_fun(input$color4d, values$dat)
    scat_message
  })

  # Drawing buttons
  output$i_draw_scat4d <- renderUI({
    # Check if there is any data
    if (is.null(values$dat)) {
      return(NULL)
    }

    if (is.null(input$plotType) || (input$plotType != 3)) {
      return(NULL)
    }

    actionButton("refresh3", label = "Draw/Refresh", icon = icon("signal"))
  })

  ref_plot3 <- eventReactive(input$refresh3, {
    showModal(modalDialog(title = "Drawing scatterplot 4D",
                          footer = "Click anywhere to close",
                          size = c("s"),
                          easyClose = TRUE))
    refresh <- TRUE
  })

  # Plotting scatterplot 4D
  output$scatter_plot4d <- renderPlotly({
    # Check if there is any data
    if (is.null(values$dat)) {
      return(NULL)
    }

    datum <- cbind(values$dat, as.data.frame(results$residual))
    names(datum)[ncol(datum)] <- "Residual"
    refresh <- ref_plot3()
    col_nam <- colnames(datum)
    point_size <- 4

    if (nrow(datum) > 2000) {
      point_size <- 3
    }

    # Check if it is the same dataset than the last time a scatterplot was drawn
    if (!is.null(old_names)) {
      if (old_names[length(old_names)] != names(values$dat[ncol(values$dat)])) {
        old_names <<- names(values$dat)
        return(NULL)
      }
    }
    old_names <<- names(values$dat)

    # Draw plot
    isolate({
      plot4d <- plot_ly(
        data = datum,
        x = datum[, match(input$x_scat4d, col_nam)],
        y = datum[, match(input$y_scat4d, col_nam)],
        z = datum[, match(input$z_scat4d, col_nam)],
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = point_size,
          color = datum[, match(input$color4d, col_nam)],
          colorbar = list(
            title = paste(title = paste(input$color4d)),
            titlefont = list(size = 18),
            tickfont = list(size = 14)
          ),
          colorscale = "Rainbow",
          showscale = TRUE
        ),
        hovertemplate = paste(
          input$x_scat4d, ": ", datum[, match(input$x_scat4d, col_nam)], "<br>",
          input$y_scat4d, ": ", datum[, match(input$y_scat4d, col_nam)], "<br>",
          input$z_scat4d, ": ", datum[, match(input$z_scat4d, col_nam)], "<br>",
          input$color, ": ", datum[, match(input$color4d, col_nam)],
          "<extra></extra>" # Removes trace0
        )
      )
      plot4d <- plot4d %>%
      layout(
        scene = list(
          xaxis = list(
            title = input$x_scat4d,
            titlefont = list(size = 18),
            tickfont = list(size = 14)
          ),
          yaxis = list(
            title = input$y_scat4d,
            titlefont = list(size = 18),
            tickfont = list(size = 14)
          ),
          zaxis = list(
            title = input$z_scat4d,
            titlefont = list(size = 18),
            tickfont = list(size = 14)
          )
        )
      )
      if (class(datum[, match(input$i_color_scat, col_nam)]) == "factor") {
        showModal(modalDialog(
          title = "Colors doesn't work with factor variables",
          NULL,
          size = c("s")
          )
        )
      }
    })
    plot4d
  })

  #----------------------------------------------------------------------------#
  #---------------------TabItem 2: Loaded model variables----------------------#
  #----------------------------------------------------------------------------#

  # Show target and input variables for loaded model
  output$targetTitle <- renderPrint({
    HTML("<b>Target: </b>")
  })

  output$inputTitle <- renderPrint({
    HTML("<b>Predictors: </b>")
  })

  # Show model parameters for loaded model
  output$paramsTitle <- renderPrint({
    HTML("<b>Model parameters: </b>")
  })

  # Show training and testing years for loaded model
  output$yearsTitle <- renderPrint({
    title <- HTML("<b>Testing data percentage: </b>")
    if ((is.null(input$data_type) || input$data_type != 2)) {
      title <- HTML("<b>Model training and testing period: </b>")
    }
    title
  })

  #----------------------------------------------------------------------------#
  #-----------------------TabItem 2: New model variables-----------------------#
  #----------------------------------------------------------------------------#

  # Menu for choosing target and predictor variables for the new model
  output$targetVar <- renderUI({
    datum <- values$dat # "values": dataframe with new data
    if (is.null(datum)) { # Check if there is any data
      return(HTML("Please load some data file"))
    }
    classes <- identify_classes(datum) # Calculate classes for columns
    num_class <- which("numeric" == classes)
    items <- sort(names(datum)[num_class])
    sele <- NULL
    if (!is.null(input$target)) {
      sele <- input$target
    }
    selectInput("target", "Target variable", items, sele)
  })

  output$inputVars <- renderUI({
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum) || is.null(input$target)) {
      return(NULL)
    }

    # Calculate classes for columns
    classes <- identify_classes(datum)

    # Select prediction variables
    selected_pred_vars_list <- select_pred_variables(
      classes,
      datum,
      input$target,
      groups
    )

    mat_mo <<- selected_pred_vars_list[[1]]
    items <- selected_pred_vars_list[[2]]
    sele <- NULL

    if (!is.null(input$inputs)) {
      sele <- input$inputs
    }

    pickerInput(
      inputId = "inputs",
      label = "Predictor variables",
      choices = items,
      options = list(
        "actions-box" = TRUE
      ),
      selected = sele,
      multiple = TRUE
    )
  })

  # Menu for choosing test/train periods for the new model
  output$iTrainTest <- renderUI({
    # Adapt options to the user choices
    if (((!is.null(input$data_type) && input$data_type == 2)) || input$info1) {
      return(NULL)
    }

    aux_soldier <- "train_test_options"
    source("aux_soldier.R", local = TRUE)$value
  })

  output$iTrainYears <- renderUI({
    if ((!is.null(input$data_type) && input$data_type == 2)) {
      return(NULL)
    }
    aux_soldier <- "train_periods"
    source("aux_soldier.R", local = TRUE)$value
  })

  output$iTestYears <- renderUI({
    if ((!is.null(input$data_type) && input$data_type == 2)) {
      return(NULL)
    }
    aux_soldier <- "test_period"
    source("aux_soldier.R", local = TRUE)$value
  })

  output$iTestPerc1 <- renderUI({
    if ((!is.null(input$data_type) && input$data_type == 2)) {
      return(NULL)
    }
    aux_soldier <- "test_period_perc"
    source("aux_soldier.R", local = TRUE)$value
  })

  output$i_random_data <- renderUI({
    # Adapt options to the user choices
    if (input$info1) {
      return(NULL)
    }

    # Check if there is any data
    if (is.null(values$dat) || is.null(input$data_type)) {
      return(NULL)
    }

    if ((is.null(input$data_type) || input$data_type != 2)) {
      return(NULL)
    }

    checkboxInput(
      "random_data",
      label = "Random train/test data",
      value = TRUE
    )
  })

  output$iTestPerc2 <- renderUI({
    if (input$info1) {
      return(NULL)
    } # Adapt options to the user choices
    if (is.null(values$dat) || is.null(input$data_type)) {
      return(NULL)
    } # Check if there is any data
    if ((is.null(input$data_type) || input$data_type != 2)) {
      return(NULL)
    }
    sliderInput(
      "testPerc2",
      label = h5("Percentage of testing data:"),
      min = 0,
      max = 50,
      value = 25
    )
  })

  # Menu for choosing parameters for the new model
  output$i_shrinkage <- renderUI({
    # Adapt options to the user choices
    if (input$info1) {
      return(NULL)
    }

    if (aux_soldier) {
      val <- 0.01

      numericInput(
        "shrinkage",
        label = "Shrinkage",
        min = 0.0001,
        max = 0.1,
        value = val,
        step = 0.005
      )
    } else {
      return(NULL)
    }
  })
  output$i_int_depth <- renderUI({
    # Adapt options to the user choices
    if (input$info1) {
      return(NULL)
    }

    if (aux_soldier) {
      val <- 2

      sliderInput(
        "intDepth",
        label = "Interaction depth",
        min = 1,
        max = 5,
        value = val,
        step = 1
      )
    } else {
      return(NULL)
    }
  })
  output$i_bag_fraction <- renderUI({
    if (input$info1) {
      return(NULL)
    } # Adapt options to the user choices
    if (aux_soldier) {
      val <- 0.5

      sliderInput(
        "bagFraction",
        label = "Bag fraction",
        min = 0.2,
        max = 1,
        value = val,
        step = 0.1
      )
    } else {
      return(NULL)
    }
  })
  output$i_num_trees <- renderUI({
    if (input$info1) {
      return(NULL)
    } # Adapt options to the user choices
    if (aux_soldier) {
      val <- 500

      numericInput(
        "numTree",
        label = "Number of trees",
        min = 250,
        max = 10000,
        value = val,
        step = 500
      )
    } else {
      return(NULL)
    }
  })
  output$iDefault <- renderUI({
    if (is.null(values$dat)) {
      return(NULL)
    }

    # Adapt options to the user choices
    if (input$info1) {
      return(NULL)
    }

    if (!aux_soldier) {
      HTML(
        "<b>Shrinkage:</b> 0.01,
        <b>Interaction depth:</b> 2,
        <b>Bag fraction:</b> 0.5,
        <b>Number of trees:</b> 500"
      )
    } else {
      return(NULL)
    }
  })

  output$iInfo1 <- renderUI({
    if (is.null(values$dat)) {
      return(NULL)
    }
    if (!input$info1) {
      return(NULL)
    } # Adapt options to the user choices
    HTML(
      "<b>Hint:</b> Try with default values and check the error curve in the tab
      <u>Out of bag estimation of the optimal number of boosting iterations</u>.
      The error curve (solid) shall be mostly horizontal at the intersection
      with the blue dotted vertical line. Increase the number of trees if that
      is not the case. The blue dotted line indicates the optimal number of
      trees. See the manual for further info."
    )
  })

  #----------------------------------------------------------------------------#
  #-----------------------------TabItem 2: Predict-----------------------------#
  #----------------------------------------------------------------------------#

  model_predict <- eventReactive(input$predi, {
    refresh <- TRUE
  })

  #----------------------------------------------------------------------------#
  #-----------------------TabItem 2: Calculate new model-----------------------#
  #----------------------------------------------------------------------------#

  # Let start model calculation
  output$buildTitle <- renderPrint({
    HTML("<b>Number of models</b>")
  })

  output$iBuild <- renderUI({
    models$num <- 1

    actionButton("build", label = "Calculate", icon = icon("cog"))
  })

  # Build model
  model_res_fit <- eventReactive(input$build, {
    datum <- values$dat

    # Check if there is any data
    if (is.null(datum)) {
      return(NULL)
    }
    # Don't allow to choose target as input
    target_nu <- match(input$target, names(datum))
    datum <- datum[-target_nu]

    # Match groups of variables
    inputs <- unique(select_variables(datum, input$inputs))
    target_po <- which(inputs == input$target)

    if (length(target_po) > 0) {
      inputs <- inputs[-which(inputs == input$target)]
    }

    if (!is.null(influ$less) && values$ignore) {
      remov <- match(influ$less, inputs)
      inputs <- inputs[-remov]
    }

    # Check train/test data
    datum <- values$dat

    if ((is.null(input$data_type) || input$data_type != 2)) {
      aux_soldier <- "check_train_test"
      source("aux_soldier.R", local = TRUE)$value
    } else {
      indices <- vector("numeric", nrow(values$dat))
      for (i in seq_along(indices)) {
        indices[i] <- i
      }
      min_train <- NULL
      max_train <- NULL
      min_test <- NULL
      max_test <- NULL
      if (input$random_data) {
        positions <- sample(indices, length(indices) * input$testPerc2 / 100)
      } else {
        positions <- tail(indices, n = length(indices) * input$testPerc2 / 100)
      }
      train_data <- values$dat[-positions, ]
      test_data <- values$dat[positions, ]
    }

    if (nrow(train_data) < 23) {
      showModal(
        modalDialog(
          title = "Warning",
          "Not enough training data for building a prediction model",
          size = c("s"),
          easyClose = TRUE
        )
      )

      return(NULL)
    }

    # Check parameters, inputs and target
    print("Checking other parameters")

    # Set value for shrinkage
    if (is.null(input$shrinkage)) {
      i_shrinkage <- 0.01
    } else {
      i_shrinkage <- input$shrinkage
      # Check value for shrinkage
      if (
        !is.numeric(i_shrinkage) || i_shrinkage < 0.0001 || i_shrinkage > 0.1
      ) {
        showModal(
          modalDialog(
            title = "Warning",
            "Shrinkage out of range [0.0001-0.1]",
            size = c("s"),
            easyClose = TRUE
          )
        )
        return(NULL)
      }
    }

    # Set value for int depth
    if (is.null(input$intDepth)) {
      i_int_depth <- 2
    } else {
      i_int_depth <- input$intDepth
      # Check value for int depth
      if (!is.numeric(i_int_depth) || i_int_depth < 1 || i_int_depth > 5) {
        showModal(
          modalDialog(
            title = "Warning",
            "Interaction depth out of range [1-5]",
            size = c("s"),
            easyClose = TRUE
          )
        )
        return(NULL)
      }
    }

    # Set value for bag fraction
    if (is.null(input$bagFraction)) {
      i_bag_fraction <- 0.5
    } else {
      i_bag_fraction <- input$bagFraction
      # Check value for bag fraction
      if (
        !is.numeric(i_bag_fraction) || i_bag_fraction < 0.2 || i_bag_fraction > 1
      ) {
        showModal(
          modalDialog(
            title = "Warning",
            "Bag fraction out of range [0.2-1]",
            size = c("s"),
            easyClose = TRUE
          )
        )
        return(NULL)
      }
    }

    if (is.null(input$numTree)) { # Set value for number of trees
      i_num_trees <- 500
    } else {
      i_num_trees <- input$numTree
      # Check value for number of trees
      if (
        !is.numeric(i_num_trees) || i_num_trees < 250 || i_num_trees > 10000
      ) {
        showModal(
          modalDialog(
            title = "Warning",
            "Number of trees out of range [250-10000]",
            size = c("s"),
            easyClose = TRUE
          )
        )

        return(NULL)
      }
    }

    # Check if there is any data
    if (is.null(inputs)) {
      showModal(
        modalDialog(
          title = "Warning",
          "There are not predictor variables",
          size = c("s"),
          easyClose = TRUE
        )
      )

      return(NULL)
    }
    target_nu <- match(input$target, colnames(values$dat))

    # Asign values to variables needed
    print("Asigning values to variables")
    for (i in seq_along(inputs)) { # Store names of inputs
      if (i == 1) {
        name_vars <- inputs[i]
      } else {
        name_vars <- paste(name_vars, "+", inputs[i])
      }
    }

    datum <- datum[!is.na(datum[, target_nu]), ]
    train_data <- train_data[!is.na(train_data[, target_nu]), ]
    test_data <- test_data[!is.na(test_data[, target_nu]), ]

    # Remove columns without values
    all_miss_cols <- sapply(train_data, function(x) all(is.na(x)))
    if (any(all_miss_cols)) {
      cols_with_all_miss <- names(all_miss_cols[all_miss_cols == TRUE])
      train_data <- select(train_data, -tidyselect::all_of(cols_with_all_miss))
      test_data <- select(test_data, -tidyselect::all_of(cols_with_all_miss))
      for (element_to_remove in cols_with_all_miss) {
        inputs <- inputs[inputs != element_to_remove]
      }
    }

    # Asign values to variables needed
    print("Asigning values to variables")
    var_names <- character(0)
    for (i in seq_along(inputs)) {
      var_names <- paste(var_names, "+", inputs[i])
    }

    model_formula <- as.formula(paste(input$target, "~", var_names))
    influ$inf <- as.data.frame(matrix(NA, length(inputs), 1 + models$num))
    influ$mean <- as.data.frame(matrix(NA, length(inputs), 2))
    results$error <- as.data.frame(matrix(NA, 4, models$num))
    rows_num <- nrow(train_data) + nrow(test_data)
    model_p$pre <- as.data.frame(matrix(NA, rows_num, models$num))
    model_e$error <- as.data.frame(matrix(NA, rows_num, models$num))

    # Start "n" calculations (letting know the user each 10 models)
    showModal(
      modalDialog(
        title = "Calculating models",
        footer = "This may take a while...",
        size = c("s"),
        easyClose = FALSE
      )
    )

    withProgress(message = "Training models", detail = NULL, value = 0, {
      # Calculate n BRT models
      for (iter in seq_len(models$num)) {
        incProgress(1 / models$num)
        if ((iter / 10) == round(iter / 10)) {
          showModal( # Trace progress
            modalDialog(
              title = paste("Calculating models", iter, "-", iter + 9),
              footer = "This may take a while...",
              size = c("s"),
              easyClose = FALSE
            )
          )
        }

        # Create BRT model
        print("Creating BRT model")
        model_list <- model_fun(
          influ,
          input,
          results,
          model_p,
          model_e,
          input$data_type,
          iter,
          model_formula,
          i_num_trees,
          i_int_depth,
          i_shrinkage,
          i_bag_fraction,
          test_data,
          train_data,
          input$target,
          positions,
          min_train,
          max_train,
          min_test,
          max_test,
          values$dat,
          input$testPerc2
        )
        model_res_fit <- model_list[[1]]
        brt_model <- model_list[[2]]
      }
    })

    # Warning explaining the removed variables
    if (any(all_miss_cols)) {
      cols_with_all_miss <- names(all_miss_cols[all_miss_cols == TRUE])
      showModal(
        modalDialog(
          title = "Warning",
          paste(
            "Variables ",
            paste(cols_with_all_miss, collapse=", "),
            " have been removed from training as in the selected period they
            are all NA."
          ),
          size = c("s"),
          easyClose = TRUE
        )
      )
    }

    # End models calculations
    print("Calculating mean influences")
    end_list <- change_char_to_factor(
      influ,
      iter,
      input$data_type,
      values,
      positions,
      target_nu,
      input$target,
      inputs,
      min_train,
      max_train,
      min_test,
      max_test
    )

    influ$mean <- end_list[[1]]
    text <- end_list[[2]]
    title <- "Computation ended"
    if ((!is.null(input$data_type) && input$data_type == 2)) {
      rows <- c(seq_len(nrow(values$dat)))[-positions]
    } else {
      rows <- scat$ini:scat$end
    }

    # Check if there are NA in traget variable
    if (any(is.na(values$dat[rows, target_nu]))) {
      perc <- 100 * sum(is.na(values$dat[rows, target_nu])) / (length(rows))
      title <- paste(
        "Warning",
        "There are",
        round(perc),
        "% of NA on target variable"
      )
    }

    values$dat <- datum
    values$train_data <- train_data
    values$test_data <- test_data

    showModal(modalDialog(title = title, text, size = c("s"), easyClose = TRUE))
    model_res_fit
  })

  # Save the new model
  output$iDownload1 <- renderUI({

    datum <- values$dat

    # Adapt options to the user choices
    if (!aux_soldier) {
      return(NULL)
    }

    # Check if there is any data
    if (is.null(model_res_fit())) {
      return(NULL)
    }

    # Check if there is any data
    if (is.null(datum)) {
      return(NULL)
    }

    downloadButton("download1", "Download RDS")
  })

  observeEvent(input$download1, {
  })

  output$download1 <- downloadHandler(
    filename = "name.rds",
    content = function(file) {
      saveRDS(
        model_res_fit(),
        file
      )
    }
  )

  #----------------------------------------------------------------------------#
  #--------------------TabItem 2: Show results of new model--------------------#
  #----------------------------------------------------------------------------#

  # Show MAE and R2 for training
  output$iMaeTrain <- renderValueBox({
    if (is.null(values$dat)) {
      return(valueBox(NULL, "Training MAE", icon("chart-line"), "blue"))
    }

    # Adapt options to the user choices
    if (input$info3) {
      return(valueBox(NULL, "Training MAE", icon("chart-line"), "blue"))
    }

    if (length(models$num) > 0) {
      mae_train <- 0

      # Calculate mean of MAE for all the models for train period
      for (i in seq_len(models$num)) {
        mae_train <- mae_train + results$error[1, i]
      }
      mae_train <- round(mae_train / models$num, digits = 2)
    }

    valueBox(mae_train, "Training MAE", icon("chart-line"), "blue")
  })

  output$iR2Train <- renderValueBox({
    if (is.null(values$dat)) {
      return(valueBox(NULL, "Training R2", icon("list"), "blue"))
    }

    if (input$info3) { # Adapt options to the user choices
      return(valueBox(NULL, "Training R2", icon("list"), "blue"))
    }

    if (length(models$num) > 0) {
      r2_train <- 0

      # Calculate mean of R2 for all the models for train period
      for (i in seq_len(models$num)) {
        r2_train <- r2_train + results$error[3, i]
      }
      r2_train <- round(r2_train / models$num, digits = 2)
    }

    valueBox(r2_train, "Training R2", icon("list"), "blue")
  })

  # Show MAE and R2 for testing
  output$iMaeTest <- renderValueBox({
    if (is.null(values$dat)) {
      return(valueBox(NULL, "Testing MAE", icon("chart-line"), "blue"))
    }

    # Adapt options to the user choices
    if (input$info3) {
      return(valueBox(NULL, "Testing MAE", icon("chart-line"), "blue"))
    }

    if (length(models$num) > 0) {
      mae_test <- 0

      # Calculate mean of MAE for all the models for test period
      for (i in seq_len(models$num)) {
        mae_test <- mae_test + results$error[2, i]
      }
      mae_test <- round(mae_test / models$num, digits = 2)
    }

    valueBox(mae_test, "Testing MAE", icon("chart-line"), "blue")
  })
  output$iR2Test <- renderValueBox({
    if (is.null(values$dat)) {
      return(valueBox(NULL, "Testing R2", icon("list"), "blue"))
    }

    # Adapt options to the user choices
    if (input$info3) {
      return(valueBox(NULL, "Testing R2", icon("list"), "blue"))
    }

    if (length(models$num) > 0) {
      r2_test <- 0

      # Calculate mean of R2 for all the models for test period
      for (i in seq_len(models$num)) {
        r2_test <- r2_test + results$error[4, i]
      }
      r2_test <- round(r2_test / models$num, digits = 2)
    }

    valueBox(r2_test, "Testing R2", icon("list"), "blue")
  })

  # Show info
  output$iInfo3 <- renderUI({
    if (is.null(values$dat)) {
      return(NULL)
    }

    # Adapt options to the user choices
    if (!input$info3) {
      return(NULL)
    }
    HTML("<b>MAE</b>: Mean Absolute Error <br>
               <b>R2</b>: Coefficient of Determination")
  })

  # Graph for model fitting (date and non-date data)
  output$iRefre <- renderUI({
    if (
      !is.null(input$data_type) &&
      input$data_type >= 1
    ) {
      actionButton(
        "refresh_pred_data",
        "Show/Refresh results",
        icon = icon("signal")
      )
    }
  })

  # Graph for observation/prediction/residual of model fitting (date data)
  ref_date_data <- eventReactive(input$refresh_pred_data, {
    refresh <- TRUE
  })

  # Graph for observation/prediction/residual of model fitting (non-date data)
  ref_no_date_data <- eventReactive(input$refresh_pred_data, {
    refresh <- TRUE
  })

  output$pred_graph <- renderPlotly({
    if (input$data_type != 2) { # Graphs for time-dependent data
      refresh <- ref_date_data()
      isolate({
        aux_soldier <- "model_fit_graph"
        source("aux_soldier.R", local = TRUE)$value
        res_graph
      })
    } else { # Graphs for time-independent data
      refresh <- ref_no_date_data()

      model_res <- model_res_fit()
      n_model <- models$num
      predict <- model_p$pre

      results$residual <- model_res$data_out[,4]

      # Calculate train fitting plot
      ini_train <- 1
      end_train <- nrow(model_res$data_out) - length(model_res$positions)
      text_train <- "Training observation"
      sca_plot_train <- fit_fun(
        model_res$data_out,
        n_model,
        predict,
        ini_train,
        end_train,
        text_train
      )

      # Calculate test fitting plot
      ini_test <- nrow(model_res$data_out) - length(model_res$positions) + 1
      end_test <- nrow(model_res$data_out)
      text_test <- "Testing observation"

      sca_plot_test <- fit_fun(
        model_res$data_out,
        n_model,
        predict,
        ini_test,
        end_test,
        text_test
      )

      plotly_sca_plot_train <- plotly::ggplotly(sca_plot_train) %>%
      plotly::layout(xaxis = list(title = list(text = text_train)),
                     yaxis = list(title = list(text = "Prediction")))

      plotly_sca_plot_test <- plotly::ggplotly(sca_plot_test)%>%
      plotly::layout(xaxis = list(title = list(text = text_test)),
                     yaxis = list(title = list(text = "Prediction")))

      plotly_sca_plot <- subplot(
        plotly_sca_plot_train,
        plotly_sca_plot_test,
        titleX = TRUE,
        shareY = TRUE
      )

      return(plotly_sca_plot)
    }
  })

  # Graph for model fitting (no-date-data training)
  output$iRefre8 <- renderUI({
    if ((!is.null(input$data_type) && input$data_type == 2)) {
      actionButton("refresh8", "Show/Refresh results", icon = icon("signal"))
    }
  })

  refPlot7 <- eventReactive(input$refresh8, {
    refresh <- TRUE
  })

  output$fitPlot1 <- renderPlot({
    if ((is.null(input$data_type) || input$data_type != 2)) {
      return(NULL)
    }

    refresh <- refPlot7()

    model_res <- model_res_fit()
    n_model <- models$num
    predict <- model_p$pre

    ini <- 1
    end <- nrow(model_res$data_out) - length(model_res$positions)
    text <- "Training observation"

    # Calculate fitting plot
    sca_plot <- fit_fun(model_res$data_out, n_model, predict, ini, end, text)

    return(sca_plot)
  })

  refPlot8 <- eventReactive(input$refresh8, {
    refresh <- TRUE
  })

  # Out-of-bag plot for regression
  output$oobPlot <- renderPlot({
    if (is.null(values$dat)) {
      return(NULL)
    }

    oob_dat <- model_res_fit()$model

    plot_oob <- gbm.perf(
      oob_dat,
      plot.it = TRUE,
      oobag.curve = FALSE,
      overlay = TRUE,
      method = "OOB"
    )
    return(plot_oob)
  })

  #----------------------------------------------------------------------------#
  #--------------------TabItem 3: Relative influence plots---------------------#
  #----------------------------------------------------------------------------#

  # Bars plot for relative influence (best 20 variables): mean new models
  output$barPlot2 <- renderPlotly({
    # Check if there is any data
    if (is.null(influ$mean)) {
      return(NULL)
    }

    print("Calculating relative influence bars graph")

    mean_influence_ordered <- influ$mean[order(influ$mean[, 2]), ]
    var_inf <- data.frame(
      var = mean_influence_ordered[, 1],
      rel.inf = mean_influence_ordered[, 2]
    )

    influ$less <- var_inf[var_inf[, 2] < (var_inf[nrow(var_inf), 2] / 30), 1]

    vars_plot <- min(20, nrow(var_inf), na.rm = TRUE)

    # The last variables are the ones with more influence
    max_var <- nrow(var_inf)
    min_var <- max_var + 1 - vars_plot

    # Calculate bars plots
    rel_influence_plot <- bars_fun(var_inf, min_var, max_var)
    rel_influence_plot <- rel_influence_plot +
      geom_bar(stat = "identity", fill = "steelblue1", colour = "black")

    return(rel_influence_plot)
    dev.off()
  })

  # Bars plot for relative influence (best 20 vars): mean new models in groups
  output$barPlot3 <- renderPlotly({
    # Check if there is any data
    if (is.null(influ$mean)) {
      return(NULL)
    }

    print("Calculating relative influence bars graph")

    rel_influence_plot <- bars_new_fun(influ$mean)
    return(rel_influence_plot)
    dev.off()
  })

  # Box plot for relative influence (best 10 variables) mean new models
  output$plotBox <- renderPlot({
    # Check if there is any data
    if (is.null(influ$mean)) {
      return(NULL)
    }
    print("Calculating relative influence box graph")
    box <- box_fun(influ, models$num)
    return(box)
    dev.off()
  })

  # Save relative influence data
  output$iDownload6 <- renderUI({
    datum <- values$dat
    if (is.null(input$x_dp2)) {
      return(NULL)
    } # Check if there is any data
    if (!aux_soldier) {
      return(NULL)
    } # Adapt options to the date based data
    if (is.null(datum)) {
      return(NULL)
    } # Check if there is any data
    downloadButton("download6", "Download CSV")
  })
  output$download6 <- downloadHandler(
    filename = "name.csv",
    content = function(file) {
      write.table(
        data.frame(
          var = influ$mean[order(influ$mean[, 2]), ][, 1],
          inf = influ$mean[order(influ$mean[, 2]), ][, 2]
        ),
        file
      )
    }
  )

  #----------------------------------------------------------------------------#
  #--------------------TabItem 3: Partial dependence plots---------------------#
  #----------------------------------------------------------------------------#

  # Menu for selecting variables for partial dependence plots
  output$varsPd2 <- renderUI({
    mod_vars <- model_res_fit()$model$var.names

    selectizeInput("x_dp2", NULL, choices = c("None", mod_vars))
  })

  output$varsPd3 <- renderUI({
    mod_vars <- model_res_fit()$model$var.names

    selectizeInput("x_dp3", NULL, choices = c("None", mod_vars))
  })

  output$iTextNum <- renderUI({
    if (!aux_soldier) {
      return(NULL)
    }
    HTML("<b>Number of points:</b>")
  })

  output$iPointsPd2 <- renderUI({
    if (!aux_soldier) {
      return(NULL)
    }
    numericInput(
      inputId = "points_pd2",
      label = NULL,
      value = 10,
      min = 5,
      max = 40,
      step = NA
    )
  })

  observeEvent(input$points_pd2, {
    values$points_pd2 <- input$points_pd2
  })

  # Show partial dependence plot 1D
  output$pdPlot1 <- renderPlotly({
    if (is.null(input$x_dp2) || input$x_dp2 == "None") {
      return(NULL)
    }

    if (class(values$dat[, input$x_dp2]) == "factor" && input$x_dp3 != "None") {
      showModal(
        modalDialog(
          title = "Warning",
          "Can't compare factor variables",
          size = c("s")
        )
      )
    }

    if (input$x_dp3 != "None") {
      if (
        class(values$dat[, input$x_dp3]) == "factor" && input$x_dp2 != "None"
      ) {
        showModal(
          modalDialog(
            title = "Warning",
            "Can't compare factor variables",
            size = c("s")
          )
        )
      }
    }

    pd_plot <- pdp1d_fun(
      model_res_fit(),
      input$data_type,
      values$points_pd2,
      input$x_dp3,
      input$x_dp2,
      input$target
    )

    pd_plot
  })

  # Show partial dependence plot 2D
  output$pdPlot2 <- renderPlotly({
    if (is.null(input$x_dp2) || input$x_dp2 == "None") {
      return(NULL)
    }

    if (is.null(input$x_dp3) || input$x_dp3 == "None") {
      return(NULL)
    }
    print("Calculating 2D PDP graph")
    heat_p <- pdp2d_fun(
      input$data_type,
      model_res_fit(),
      values$points_pd2,
      input$x_dp2,
      input$x_dp3,
      input$target
    )

    heat_p
  })

  # Show partial dependence plot 3D
  output$warning2 <- renderUI({
    if (!aux_soldier) {
      warning <- em("Not avaliable")
    } else if ((length(input$reduce) > 0) && input$reduce == "Month") {
      warning <- em("Not avaliable")
    } else {
      return(NULL)
    }
    warning
  })
  output$iPdPlot3 <- renderUI({
    if (aux_soldier) {
      plotly::plotlyOutput("pdPlot3", height = 600, width = 600)
    } else {
      return(NULL)
    }
  })
  output$pdPlot3 <- renderPlotly({
    if (is.null(input$x_dp2) || input$x_dp2 == "None") {
      return(NULL)
    }
    if (is.null(input$x_dp3) || input$x_dp3 == "None") {
      return(NULL)
    }
    print("Calculating 3D PDP graph")
    plot_3d <- pdp3d_fun(
      model_res_fit(),
      input$data_type,
      values$points_pd2,
      input$x_dp2,
      input$x_dp3,
      input$target
    )

    plot_3d
  })
})
