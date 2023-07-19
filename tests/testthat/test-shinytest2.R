library(shinytest2)

test_that("{shinytest2} recording: load_file", {
  app <- AppDriver$new(name = "load_file", height = 943, width = 1577)
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_inputs(data_type = "1")
  rlang::warn(paste0("`file_new_data` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(file_new_data = "DummyData.xlsx")
  app$expect_values()
})
