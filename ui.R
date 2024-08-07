
#------------------------------------------------------------------------------#
#----------------------------------Libraries-----------------------------------#
#------------------------------------------------------------------------------#

# Load or install libraries for building applications
if (!require("base")) {
  install.packages("base")
}
if (!require("lubridate")) {
  install.packages("lubridate")
}
if (!require("dotCall64")) {
  install.packages("dotCall64")
}
if (!require("evaluate")) {
  install.packages("evaluate")
}
if (!require("markdown")) {
  install.packages("markdown")
}
if (!require("knitr")) {
  install.packages("knitr")
}
if (!require("rmarkdown")) {
  install.packages("rmarkdown")
}
if (!require("rsconnect")) {
  install.packages("rsconnect")
}
if (!require("shiny")) {
  install.packages("shiny")
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
}
if (!require("magrittr")) {
  install.packages("magrittr")
}

# Load or install libraries for creating graphics
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
if (!require("colorRamps")) {
  install.packages("colorRamps")
}
if (!require("dygraphs")) {
  install.packages("dygraphs")
}
if (!require("forecast")) {
  install.packages("forecast")
}
if (!require("lattice")) {
  install.packages("lattice")
}
if (!require("maps")) {
  install.packages("maps")
}
if (!require("plotly")) {
  install.packages("plotly")
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
}
if (!require("scales")) {
  install.packages("scales")
}

# Load or install libraries for working with regression methods
if (!require("fields")) {
  install.packages("fields")
}
if (!require("gbm")) {
  install.packages("gbm")
}
if (!require("mgcv")) {
  install.packages("mgcv")
}
if (!require("nlme")) {
  install.packages("nlme")
}
if (!require("pdp")) {
  install.packages("pdp")
}
if (!require("survival")) {
  install.packages("survival")
}
if (!require("urca")) {
  install.packages("urca")
}

# Load or install libraries for working with data
if (!require("DT")) {
  install.packages("DT")
}
if (!require("highr")) {
  install.packages("highr")
}
if (!require("Matrix")) {
  install.packages("Matrix")
}
if (!require("spam")) {
  install.packages("spam")
}
if (!require("tinytex")) {
  install.packages("tinytex")
}
if (!require("xts")) {
  install.packages("xts")
}
if (!require("readxl")) {
  install.packages("readxl")
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
}

# Testing
if (!require("shinytest2")) {
  install.packages("shinytest2")
}
if (!require("testthat")) {
  install.packages("testthat")
}

set.seed(1)

#------------------------------------------------------------------------------#
#-----------------------------Title and background-----------------------------#
#------------------------------------------------------------------------------#

# User interface background and title
dashboardPage(
  skin = "black",
  dashboardHeader(
    title = div(
      tags$a("SOLDIER - SOLution for Dam behavior Interpretation and safety Evaluation with BRT"),
      tags$img(src = "CIMNE.png", height = "30px")
    ),
    titleWidth = 1300
  ),

  #----------------------------------------------------------------------------#
  #----------------------------------Sidebar-----------------------------------#
  #----------------------------------------------------------------------------#

  # Menus in sidebar
  dashboardSidebar(
    width = 220,
    sidebarMenu(id = "tabs", style = "white-space: normal;",

      # TabPanel 1
      menuItem("Data exploration",
       tabName = "exploration",
       icon = icon(name = "zoom-in",
       lib = "glyphicon")),

      # TabPanel 2
      menuItem("Model fitting",
       tabName = "fit",
       icon = icon("cog")),

      # TabPanel 3
      menuItem("Interpretation",
      tabName = "interpretation",
      icon = icon("info-circle")),

      # Disclaimer
      menuItem(
        "Disclaimer",
        tabName = "disclaimer",
        icon = icon("exclamation"),
        div(style = "text-align: start; padding: 5px",
            p("In no event shall any of the developers be liable for any
               damages resulting from the inability to use or the misuse of
               the SOLDIER software. If the use of materials or information
               from this application entails the need to provide a service or
               repair or correct the equipment or data, the user will be
               responsible for those costs. For assistance or support please
               refer to: cimnemadrid@cimne.upc.edu."))
      )
    )
  ),

  #----------------------------------------------------------------------------#
  #------------------------------------Body------------------------------------#
  #----------------------------------------------------------------------------#

  # Options for the title and colapse boxes
  dashboardBody(
    tags$head(tags$style(HTML('.skin-black .main-header .logo {
               font-family:"Georgia", Times, "Times New Roman", serif;
               font-weight:italic;
               font-size:28px;
          }'))),

    #--------------------------------------------------------------------------#
    #-----------------------TabPanel 1: Data exploration-----------------------#
    #--------------------------------------------------------------------------#

    tabItems(
      tabItem(
        tabName = "exploration",
        tags$style(
          type = "text/css",
          ".shiny-output-error{visibility:hidden;}",
          ".shiny-output-error:before{visibility:hidden;}"
        ),
        column(
          width = 9,
          fluidRow(

            # Let choose type of data
            box(
              title = "Select data type",
              width = 6,
              height = 112,
              status = "primary",
              solidHeader = TRUE,
              uiOutput("i_data_type")
            ),

            # Box for choosing new file
            conditionalPanel(
              condition = "input.data_type == 1 || input.data_type == 2",
              box(
                title = "Select a data file",
                width = 6,
                height = 112,
                status = "primary",
                solidHeader = TRUE,
                fileInput(inputId = "file_new_data",
                          label = NULL,
                          accept = c(".csv", ".rds", ".xls", ".xlsx"))
              )
            ),
          ),
          fluidRow(

            # Box for initial image
            conditionalPanel(
              condition = "input.plot_type != 1 &&
                           input.plot_type != 2 &&
                           input.plot_type != 3 &&
                           input.plot_type != 4",
              div(
                imageOutput("frontImage"),
                div(
                  # Adjust the margin as needed for spacing
                  style = "margin-top: 130px;",
                  HTML("<p>Thank you for using our application. If you find it
                        useful, we kindly ask that you cite the associated paper
                        in your work. Your citation helps support<br>the
                        development and maintenance of the application.</p>"),
                  HTML("<p>Please cite the following paper: Salazar, F.,
                        Irazábal, J. & Conde, A. (2024). SOLDIER: SOLution for
                        Dam behavior Interpretation and safety Evaluation<br>
                        with boosted Regression trees. SoftwareX, 25, 101598.
                        </p>"),
                  HTML("<p>For any questions or further assistance, please feel
                        free to contact us at cimnemadrid@cimne.upc.edu</p>"),
                  HTML("<p>Thank you for your support!</p>")
                )
              )
            ),

            # Box for drawing time series plot
            conditionalPanel(
              condition = "(input.data_type == 1 || input.data_type == 2) &&
                           input.plot_type == 1",
              tabBox(
                width = 12,
                tabPanel(
                  "TIME SERIES",
                  plotly::plotlyOutput("time_graph", height = "640px")
                ),
                tabPanel(
                  "Help image",
                  fileInput(inputId = "fileImg",
                            label = NULL,
                            accept = "image/*"),
                  imageOutput("image1")
                )
              )
            ),

            # Box for drawing scatterplot
            conditionalPanel(
              condition = "(input.data_type == 1 || input.data_type == 2) &&
                           input.plot_type == 2",
              tabBox(
                width = 12,
                tabPanel(
                  "SCATTERPLOT",
                  plotly::plotlyOutput(
                    outputId = "scatter_plot",
                    height = "640px"
                  )
                ),
                tabPanel(
                  "Help image",
                  fileInput(inputId = "fileImg2",
                            label = NULL,
                            accept = "image/*"),
                  imageOutput("image2")
                )
              )
            ),

            # Box for drawing scatterplot 4D
            conditionalPanel(
              condition = "(input.data_type == 1 || input.data_type == 2) &&
                           input.plot_type == 3",
              tabBox(
                width = 12,
                tabPanel(
                  "SCATTERPLOT 4D",
                  plotly::plotlyOutput(
                    outputId = "scatter_plot4d",
                    height = "640px"
                  )
                ),
                tabPanel(
                  "Help image",
                  fileInput(inputId = "fileImg3",
                            label = NULL,
                            accept = "image/*"),
                  imageOutput("image3")
                )
              )
            )
          )
        ),
        column(
          width = 3,

          # Let choose plot type
          conditionalPanel(
            condition = "(input.data_type == 1 || input.data_type == 2)",
            box(
              title = "Select plot",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("iPlot")
            )
          ),

          # Box for choosing variables to show on time series plot
          conditionalPanel(
            condition = "(input.data_type == 1 || input.data_type == 2) &&
                         input.plot_type == 1",
            box(
              title = "Variables to plot",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("i_variables_left"),
              uiOutput("i_variables_right"),
              actionButton(inputId = "refresh5",
                           label = "Draw/Refresh",
                           icon = icon("signal")),
              uiOutput("i_back_colour_time_plot")
            )
          ),

          # Box for choosing variables to show on scatterplot
          conditionalPanel(
            condition = "(input.data_type == 1 || input.data_type == 2) &&
                         input.plot_type == 2",
            box(
              title = "Variables to plot",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("x_var_scat"),
              uiOutput("y_var_scat"),
              uiOutput("i_color_scat"),
              uiOutput("i_draw_scat"),
              uiOutput("i_only_train_hull"),
              uiOutput("i_back_colour_scatter_plot")
            )
          ),

          # Box for choosing variables to show on scatterplot 4D
          conditionalPanel(
            condition = "(input.data_type == 1 || input.data_type == 2) &&
                         input.plot_type == 3",
            box(
              title = "Variables to plot",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("x_var_scat4d"),
              uiOutput("y_var_scat4d"),
              uiOutput("z_var_scat4d"),
              uiOutput("i_color_scat4d"),
              uiOutput("i_draw_scat4d")
            )
          )
        )
      ),

      #------------------------------------------------------------------------#
      #------------------------TabItem 2: Model fitting------------------------#
      #------------------------------------------------------------------------#

      tabItem(
        tabName = "fit",
        column(
          width = 3,

          # Box with variables for new model
          conditionalPanel(
            condition = "input.data_type == 1 || input.data_type == 2",
            box(
              title = "Variables Selection",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("targetVar"),
              uiOutput("inputVars")
            )
          ),

          # Box with parameters for new model
          conditionalPanel(
            condition = "input.data_type == 1 || input.data_type == 2",
            box(
              title = "Training Parameters",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("i_random_data"),
              uiOutput("iTrainTest"),
              uiOutput("iTrainTestYears"),
              uiOutput("iTestPerc1"),
              uiOutput("iTestPerc2"),
              uiOutput("i_shrinkage"),
              uiOutput("i_num_trees"),
              uiOutput("i_int_depth"),
              uiOutput("i_bag_fraction"),
              uiOutput("iDefault"),
              uiOutput("iInfo1"),
              checkboxInput(inputId = "info1", label = "Help", value = FALSE)
            )
          )
        ),
        column(
          width = 9,
          fluidRow(
            # Box for calculating new models or prediction
            conditionalPanel(
              condition = "input.data_type == 1 || input.data_type == 2",
              box(
                title = "Calculation",
                width = 3,
                height = 215,
                status = "primary",
                solidHeader = TRUE,
                div(
                  style = "display:inline-block;
                          vertical-align:top;
                          width: 55px;",
                  actionButton("build", label = "Calculate", icon = icon("cog"))
                ),
                htmlOutput(outputId = NULL, container = tags$h5),
                div(
                  style = "display:inline-block;
                          vertical-align:top;
                          width: 55px;",
                  uiOutput("iDownload1")
                ),
                valueBoxOutput(outputId = "i_prediction_box", width = 12),
                div(
                  style = "display:inline-block;
                          vertical-align:top;
                          width: 73px;"
                )
              )
            ),

            # Box for errors
            conditionalPanel(
              condition = "input.data_type == 1 || input.data_type == 2",
              box(
                title = "Accuracy",
                width = 9,
                height = 215,
                status = "primary",
                solidHeader = TRUE,
                valueBoxOutput(outputId = "iMaeTrain", width = 3),
                valueBoxOutput(outputId = "iR2Train", width = 3),
                valueBoxOutput(outputId = "iMaeTest", width = 3),
                valueBoxOutput(outputId = "iR2Test", width = 3),
                uiOutput("iInfo3"),
                checkboxInput(inputId = "info3", label = "Help", value = FALSE)
              )
            )
          ),
          fluidRow(
            # Bottom row for plotting model fit for date-data
            conditionalPanel(
              condition = "input.data_type == 1 || input.data_type == 2",
              box(
                width = 12,
                tabBox(
                  title = NULL,
                  width = 12,
                  tabPanel(
                    "Model fitting",
                    fluidRow(
                      column(
                        width = 9,
                        actionButton(
                          "refresh_pred_data",
                          "Show/Refresh results",
                          icon = icon("signal")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.data_type == 1",
                        column(
                          width = 3,
                          align = "right",
                          checkboxInput(
                            "confidence_interval",
                            label = "Draw confidence interval",
                            value = FALSE
                          )
                        )
                      )
                    ),
                    plotly::plotlyOutput(outputId = "pred_graph", height = "540px")
                  ),
                  tabPanel(
                    "Out-of-bag estimation of the optimal number of boosting iterations",
                    plotOutput(outputId = "oobPlot", height = "568px")
                  )
                )
              )
            )
          )
        )
      ),

      #------------------------------------------------------------------------#
      #-----------------------TabItem 3: Interpretation------------------------#
      #------------------------------------------------------------------------#

      tabItem(
        tabName = "interpretation",
        column(
          width = 5,

          # Relative influence plots for new model
          conditionalPanel(
            condition = "input.data_type == 1 || input.data_type == 2",
            box(
              title = "Most influential variables",
              width = 13,
              height = 780,
              status = "primary",
              solidHeader = TRUE,
              tabBox(
                width = 13,
                tabPanel(
                  "Individual",
                  plotly::plotlyOutput(outputId = "barPlot2", height = "650px")
                ),
                tabPanel(
                  "Group",
                  plotly::plotlyOutput(outputId = "barPlot3", height = "650px")
                ),
                tabPanel(
                  "Help and save",
                  HTML("Value of the Relative Influence for the 20 inputs with
                        higher influence (in alphabetical order). The Relative
                        Influence is related to the strength of the association
                        between the predictor variables and the corresponding
                        target."),
                  htmlOutput(outputId = NULL, container = tags$h5),
                  div(
                    style = "display:inline-block;
                             vertical-align:top;
                             width: 200px;",
                    uiOutput("iDownload6")
                  )
                )
              )
            )
          )
        ),
        column(
          width = 7,
          fluidRow(

            # Partial dependence column
            conditionalPanel(
              condition = "input.data_type == 1 || input.data_type == 2",
              box(
                title = "Partial Dependence",
                width = 13,
                status = "primary",
                solidHeader = TRUE,
                height = 780,
                div(
                  style = "display:inline-block;
                           vertical-align:top;
                           width: 165px;",
                  uiOutput("varsPd2")
                ),
                div(
                  style = "display:inline-block;
                           vertical-align:top;
                           width: 165px;",
                  uiOutput("varsPd3")
                ),
                div(
                  style = "display:inline-block;
                           vertical-align:top;
                           width: 40px;"
                ),
                div(
                  style = "display:inline-block;
                           vertical-align:top;
                           width: 135px;",
                  uiOutput("iTextNum")
                ),
                div(
                  style = "display:inline-block;
                           vertical-align:top;
                           width: 65px;",
                  uiOutput("iPointsPd2")
                ),

                # Partial dependence plots
                tabBox(
                  width = 12,
                  tabPanel(
                    "Lines",
                    plotly::plotlyOutput("pdPlot1", height = 600)
                  ),
                  tabPanel(
                    "2D surface",
                    plotly::plotlyOutput("pdPlot2", height = 600, width = 600)
                  ),
                  tabPanel(
                    "3D surface",
                    uiOutput("warning2"),
                    uiOutput("iPdPlot3")
                  ),
                  tabPanel(
                    "Help",
                    HTML("The Partial Dependence Plots show the effect of each
                          input on the response under analysis, taking the
                          average effect of the remaining inputs into account.
                          See the manual for further info.")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
