context("test process_json_files using legacy data")

source("../../process_json_files.R")

library(readr)


test_that("test process_json_files using legacy data", {
  json_files<-list.files("../test_data/legacy", ".*\\.json", full.names=TRUE)
  expect_gt(length(json_files), 1)
  
  # function under test
  # TODO does not yet work with legacy data
  # output_files <- process_json_files(json_files)

  # expect_equal(length(output_files), 3)
  
  #expect_true(file.exists(output_files$sage_symbols))

  #expect_true(file.exists(output_files$sage_grids))
  # TODO compare expected to actual 'sage_grids'
  	
  #expect_true(file.exists(output_files$sage_prices))
  # TODO compare expected to actual 'sage_prices'
  	
  # TODO compare expected to actual 'hasd_merged'
})
