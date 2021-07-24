context("test process_json_files using novel data")

source("../../process_json_files.R")

library(readr)
library(testthat)

test_that("test process_json_files using novel data", {
  json_files <- list.files("../test_data/novel", ".*\\.json", full.names = TRUE)
  expect_gt(length(json_files), 1)
  
  # function under test
  output_files <- process_json_files(json_files)

  print(output_files)
  
  expect_equal(length(output_files), 4)
  
  expect_true(file.exists(output_files$sage_symbols))
  expected_sage_symbols <- read.csv("../expected_result/sage_symbols.csv")	
  expect_equal(read.csv(output_files$sage_symbols), expected_sage_symbols)

  expect_true(file.exists(output_files$sage_grids))
  expected_sage_grids <- read.csv("../expected_result/sage_grids.csv")	
  expect_equal(read.csv(output_files$sage_grids), expected_sage_grids)

  expect_true(file.exists(output_files$sage_prices))
  expected_sage_prices <- read.csv("../expected_result/sage_prices.csv")	
  expect_equal(read.csv(output_files$sage_prices), expected_sage_prices)

  expect_true(file.exists(output_files$hasd_merged))
  expected_hasd_merged <- read.csv("../expected_result/hasd_merged.csv")	
  expect_equal(read.csv(output_files$hasd_merged), expected_hasd_merged)
})
