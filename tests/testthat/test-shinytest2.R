library(shiny)
library(shinytest2)

test_that("{shinytest2} recording: load_data_file_xlsx", {
  app <- AppDriver$new(name = "load_data_file_xlsx", height = 916, width = 1619)
  app$set_inputs(data_origin = "1")
  app$set_inputs(data_type = "1")
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$expect_values()
})



test_that("{shinytest2} recording: scatter_plot", {
  app <- AppDriver$new(name = "scatter_plot", height = 916, width = 1619)
  app$set_inputs(data_origin = "1")
  app$set_inputs(data_type = "1")
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$set_inputs(plotType = "2")
  app$set_inputs(color = "Year")
  Sys.sleep(0.5)
  app$click("refresh6")
  app$click("refresh6")
  app$set_inputs(back_colour_scatter_plot = TRUE)
  app$click("refresh6")
  app$click("refresh6")
  app$expect_values()
})



test_that("{shinytest2} recording: scatter_plot_4d", {
  app <- AppDriver$new(name = "scatter_plot_4d", height = 916, width = 1619)
  app$set_inputs(data_origin = "1")
  app$set_inputs(data_type = "1")
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$set_inputs(plotType = "3")
  app$set_inputs(z_scat4d = "Temp030")
  app$set_inputs(color4d = "Year")
  Sys.sleep(0.5)
  app$click("refresh3")
  app$expect_values()
})



test_that("{shinytest2} recording: time_series_plot", {
  app <- AppDriver$new(name = "time_series_plot", height = 916, width = 1619)
  app$set_inputs(data_origin = "1")
  app$set_inputs(data_type = "1")
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$set_inputs(plotType = "1")
  app$set_inputs(vars_left_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(vars_left = "Disp01")
  app$set_inputs(vars_left = c("Disp01", "Disp02"))
  app$set_inputs(vars_left_open = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(vars_right_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(vars_right = "Temp007")
  app$set_inputs(vars_right_open = FALSE, allow_no_input_binding_ = TRUE)
  Sys.sleep(0.5)
  app$click("refresh5")
  app$set_inputs(colours2 = TRUE)
  app$click("refresh5")
  app$expect_values()
})



test_that("{shinytest2} recording: model_prediction_date", {
  app <- AppDriver$new(name = "model_prediction_date", height = 916, width = 1619)
  app$set_inputs(data_origin = "1")
  app$set_inputs(data_type = "1")
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$set_inputs(inputs_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(inputs = "Tem group")
  app$set_inputs(inputs = c("Tem group", "Lev"))
  app$set_inputs(inputs = c("Tem group", "Lev", "Year"))
  # Switch to the desired tab
  app$updateTabItems(session, "tabset-dashboard-body", selected = "fit")
  app$click("build")
  app$click("refresh_pred_data")
  app$expect_values()
})



test_that("{shinytest2} recording: dummy_test", {
  app <- AppDriver$new(name = "dummy_test", height = 916, width = 1619)
  app$set_inputs(data_origin = "1")
  app$set_inputs(data_type = "1")
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$set_inputs(plotType = "2")
  app$set_inputs(color = "Year")
  app$click("refresh6")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":806,\"x\":\"2015-06-14\",\"y\":-3.6117333520856114}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_values()
})

