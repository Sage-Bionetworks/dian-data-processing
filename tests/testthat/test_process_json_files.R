context("test process_json_files")

source("../../process_json_files.R")

library(readr)

test_that("process_json_files has correct output", {
  json_files<-list.files("../test_data", ".*\\.json", full.names=TRUE)
  expect_gt(length(json_files), 1)
  
  # function under test
  output_files <- process_json_files(json_files)

  expect_equal(length(output_files), 3)
  
  
  expect_true(file.exists(output_files$sage_symbols))
  expected_sage_symbols<-data.frame(
  	subid=c(440001, 440001, 600600, 600622), 
  	session=c(0,33,0,0),
  	symbols_date=c(1615478953.52758, 1632159012, 1618586613.616, 1623347426.727),
  	symbols=c(0.13780701160431, 0.307, 0.206000000000001, 3.366),
  	symbols_acc=c(0.583333333333333, 0.416666666666667, 0.25,0.916666666666667)
  	)
  	expect_equal(read.csv(output_files$sage_symbols), expected_sage_symbols)
  	
  	expect_true(file.exists(output_files$sage_grids))
  	# TODO compare expected to actual 'sage_grids'
  	
  	expect_true(file.exists(output_files$sage_prices))
  	# TODO compare expected to actual 'sage_prices'
  	
  	# TODO compare expected to actual 'hasd_merged'

})
