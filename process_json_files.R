
#
#
# Script to process JSON files for DIAN

# Input: A list of file paths. Each refers to one JSON file.
#
# Output: A list of file paths each containing results in .csv format.
#
#

library(tidyverse)
library(readit)
library(tidyjson)
library(R2HTML)
library(jsonlite)

process_json_files<-function(files) {
	n_files<-length(files)
	
	output_files <- list()

	pricesout <- matrix(, nrow = 1, ncol = 7)
	colnames(pricesout) <- c("subid","session","prices_date","pricesCorrect","pricesSelected", "Trial", "device")
	
	for (i in 1:n_files){
	  file <- files[i]
	  if (!file.exists(file)) {
	  	stop(sprintf("File %s does not exist.", file))
	  }
	  foo <- fromJSON(file)
	  
	  subid <- foo$participant_id
	  session <- foo$session_id
	  prices_date <- foo$session_date
	  device <- foo$device_info
	  foo <- as.data.frame(foo$tests$price_test$sections)
	  if(length(foo) == 0) next # if blank skip to next iteration
	  pricesCorrect <- foo$correct_index
	  pricesSelected <- foo$selected_index
	  if(length(pricesSelected) == 0) next
	  
	  
	  pricesdf <- cbind(subid, session, prices_date, pricesCorrect, pricesSelected, device)
	  pricesdf <-as.data.frame(pricesdf) # convert to data frame
	  pricesdf$Trial <- seq.int(nrow(pricesdf))
	  
	  colnames(pricesdf) = c("subid","session","prices_date","pricesCorrect","pricesSelected",  "device", "Trial")
	  
	  pricesout <- rbind(pricesout,pricesdf)
	}
	
	pricesout$acc <- ifelse(pricesout$pricesCorrect == pricesout$pricesSelected, 1,0)
	# drop first row
	pricesout <- pricesout[-1,]
	# convert session to numeric
	pricesout$session <- as.numeric(pricesout$session)
	# sort by session
	pricesout <- arrange(pricesout, session)
	
	# remove duplicate observations
	
	pricesout <- pricesout %>% distinct(subid, session,Trial, .keep_all = TRUE) %>% ungroup()
	
	pricesout <- pricesout %>% group_by(subid,session, prices_date, device) %>% summarise(prices = mean(acc)) %>% ungroup()
	
	pricesout$prices_err <- 100*(1-pricesout$prices)
	sage_prices<-file.path(tempdir(), "sage_prices.csv")
	write.csv(pricesout,sage_prices,row.names = F )
	output_files$sage_prices<-sage_prices
	
	
	#========================================================================
	# function generate permutations of integers from 1:n and return as matrix
	# M[1:n!, 1:n]
	#
	# taken from library e1071 to make code self-contained
	#
	permutations <- function (n) 
	{
	  if (n == 1) 
	    return(matrix(1))
	  else if (n < 2) 
	    stop("n must be a positive integer")
	  z <- matrix(1)
	  for (i in 2:n) {
	    x <- cbind(z, i)
	    a <- c(1:i, 1:(i - 1))
	    z <- matrix(0, ncol = ncol(x), nrow = i * nrow(x))
	    z[1:nrow(x), ] <- x
	    for (j in 2:i - 1) {
	      z[j * nrow(x) + 1:nrow(x), ] <- x[, a[1:i + j]]
	    }
	  }
	  dimnames(z) <- NULL
	  z
	}
	
	
	grid_test_computeDistance <- function(numSections,
	                                      numItems,
	                                      images.x,
	                                      images.y, 
	                                      choices.x,
	                                      choices.y){
	  
	  minDist = matrix(NA, nrow=numSections)
	  numPerm = factorial(numItems)
	  res = matrix(NA, nrow=numPerm)
	  
	  #generate permutations
	  # for 3 items returns matrix 6 x 3
	  #
	  #      [,1] [,2] [,3]
	  #[1,]    1    2    3
	  #[2,]    2    1    3
	  #[3,]    2    3    1
	  #[4,]    1    3    2
	  #[5,]    3    1    2
	  #[6,]    3    2    1
	  
	  P = permutations(numItems)
	  
	  for (j in 1:numSections){
	    for (k in 1:numPerm){
	      
	      #permutation of indices
	      ind = P[k,1:numItems]
	      
	      ##### for some reason the code contained in this box started causing problems. The original code is commented out here at the top and my modications below that      
	      # res[k] = mean(sqrt( 
	      #     (images.x[j,1:numItems] - choices.x[j,ind])^2 +
	      #     (images.y[j,1:numItems] - choices.y[j,ind])^2
	      #     ))     
	      
	      # Euclidean distance for each permutation
	      # averaged across items
	      
	      check1 <- (images.x[j,1:numItems] - choices.x[j,ind])
	      check11 <- check1^2
	      
	      check2 <-  (images.y[j,1:numItems] - choices.y[j,ind])
	      check22 <- check2^2
	      check3 <- check11 + check22
	      check4 <- sqrt(check3)
	      check4 <- as.numeric(check4)
	      res[k] = mean(check4)
	      
	    }
	    
	    # min distance for each section
	    minDist[j] = min( res [1:numPerm] )
	  } 
	  
	  #average over sections
	  out = mean ( minDist )
	  
	}
	
	
	# number of trials
	numSections=2
	
	# number of items per trial
	numItems=3
	
	# size of the screen grid
	GridSizeX=5
	GridSizeY=5
	
	# merge grids all together
	
	gridsout <- matrix(, nrow = 1, ncol = 17)
	colnames(gridsout) <- c("tapx.1","tapx.2","tapx.3","tapy.1","tapy.2","tapy.3","imagex.1","imagex.2","imagex.3","imagey.1","imagey.2","imagey.3","Trial","session","subid", "grids_date", "grids")
	
	for (i in 1:n_files){
	  file = files[i]
	  
	  foo <- fromJSON(file)
	  
	  subid <- foo$participant_id
	  session <- foo$session_id
	   grids_date = foo$session_date
	
	  foo <- as.data.frame(foo$tests$grid_test$sections)
	  check <- foo$display_symbols
	  check <- as.data.frame(check)
	  len_check <- length(check)
	  if(len_check == 0) next # if blank skip to next iteration
	  
	  len_check2 = ifelse(foo$display_symbols < 0, 0,1)
	  if(len_check2 == 0) next # if blank skip to next iteration
	  gridstap <- foo$choices
	  gridsTap1 <- as.data.frame(gridstap[1])
	  gridsTap2 <- as.data.frame(gridstap[2])
	  
	  test <- gridsTap1[1,"y"]
	  
	  gridsTapx.1 <- gridsTap1[1,"x"]
	  gridsTapx.2 <-  gridsTap1[2,"x"] 
	  gridsTapx.3 <-  gridsTap1[3,"x"]
	  gridsTapx.4 <- gridsTap2[1,"x"]
	  gridsTapx.5 <-  gridsTap2[2,"x"]
	  gridsTapx.6 <-  gridsTap2[3,"x"]
	  
	  gridsTapy.1 <- gridsTap1[1,"y"]
	  gridsTapy.2 <- gridsTap1[2,"y"]
	  gridsTapy.3 <- gridsTap1[3,"y"]
	  gridsTapy.4 <- gridsTap2[1,"y"]
	  gridsTapy.5 <- gridsTap2[2,"y"]
	  gridsTapy.6 <- gridsTap2[3,"y"]
	  
	  gridsImages <- foo$images
	  
	  gridsimages1 <- as.data.frame(gridsImages[1])
	  gridsimages2 <- as.data.frame(gridsImages[2])
	  gridsimagesx.1 <- gridsimages1[1,"x"]
	  gridsimagesx.2 <- gridsimages1[2,"x"]  
	  gridsimagesx.3 <-  gridsimages1[3,"x"]   
	  gridsimagesx.4 <-gridsimages2[1,"x"]
	  gridsimagesx.5 <-  gridsimages2[2,"x"] 
	  gridsimagesx.6 <-  gridsimages2[3,"x"] 
	  
	  gridsimagesy.1 <-gridsimages1[1,"y"]
	  gridsimagesy.2 <- gridsimages1[2,"y"]  
	  gridsimagesy.3 <-  gridsimages1[3,"y"]   
	  gridsimagesy.4 <-gridsimages2[1,"y"]
	  gridsimagesy.5 <-  gridsimages2[2,"y"] 
	  gridsimagesy.6 <-  gridsimages2[3,"y"] 
	  
	  # make a matrix of those items. one row per trial 
	  table1 <- matrix(c(gridsTapx.1,gridsTapx.2,gridsTapx.3,
	                     gridsTapy.1,gridsTapy.2,gridsTapy.3,
	                     gridsimagesx.1,gridsimagesx.2,gridsimagesx.3,
	                     gridsimagesy.1,gridsimagesy.2,gridsimagesy.3,
	                     gridsTapx.4,gridsTapx.5,gridsTapx.6,
	                     gridsTapy.4,gridsTapy.5,gridsTapy.6,
	                     gridsimagesx.4,gridsimagesx.5,gridsimagesx.6,
	                     gridsimagesy.4,gridsimagesy.5,gridsimagesy.6),
	                   nrow = 2, ncol = 12, byrow = TRUE)
	  table1 <- as.data.frame(table1)
	  colnames(table1) <- c("tapx.1", "tapx.2","tapx.3","tapy.1", "tapy.2","tapy.3","imagex.1", "imagex.2","imagex.3","imagey.1", "imagey.2","imagey.3")
	  
	  table1$Trial <- rownames(table1) 
	  table1$session <- session
	  table1$subid <- subid
	  table1$grids_date <- grids_date
	  # convert each to numeric
	  table1$imagex.1 <- as.numeric(table1$imagex.1)
	  table1$imagex.2 <- as.numeric(table1$imagex.2)
	  table1$imagex.3 <- as.numeric(table1$imagex.3)
	  table1$imagey.1 <- as.numeric(table1$imagey.1)
	  table1$imagey.2 <- as.numeric(table1$imagey.2)
	  table1$imagey.3 <- as.numeric(table1$imagey.3)
	  
	  table1$tapx.1 <- as.numeric(table1$tapx.1)
	  table1$tapx.2 <- as.numeric(table1$tapx.2)
	  table1$tapx.3 <- as.numeric(table1$tapx.3)
	  table1$tapy.1 <- as.numeric(table1$tapy.1)
	  table1$tapy.2 <- as.numeric(table1$tapy.2)
	  table1$tapy.3 <- as.numeric(table1$tapy.3)
	  
	  images.x = table1 %>% select(imagex.1, imagex.2, imagex.3)
	  images.y = table1 %>% select(imagey.1, imagey.2, imagey.3)
	  choices.x = table1 %>% select(tapx.1, tapx.2, tapx.3)
	  choices.y = table1 %>% select(tapy.1, tapy.2, tapy.3)
	  
	  
	  tak_grids = grid_test_computeDistance (numSections, numItems,images.x, images.y, choices.x, choices.y)
	  table1$grids <- tak_grids
	  
	  
	  gridsout <- rbind(gridsout,table1)
	}
	
	# this has a duplication for each trial so can just subset to Trial 1
	gridsout <- gridsout %>% distinct(subid, session, .keep_all = TRUE) %>% ungroup()
	# drop first row
	gridsout <- gridsout[-1,]
	sage_grids <- file.path(tempdir(), "sage_grids.csv")
	write.csv(gridsout, sage_grids, row.names= F)
	output_files$sage_grids<-sage_grids
	
	# Processing symbols data -------------------------------------------------
	
	# merge symbols all together
	
	symbolsout <- matrix(, nrow = 1, ncol = 8)
	colnames(symbolsout) <- c("subid","session","symbols_date", "symbolsCorrect","symbolsSelected", "selectionTime", "appearanceTime", "Trial")
	
	for (i in 1:n_files){
	  file = files[i]
	  
	  foo <- fromJSON(file)
	  
	  subid <- foo$participant_id
	  session <- foo$session_id
	  
	  symbols_date = foo$session_date
	  
	  foo <- as.data.frame(foo$tests$symbol_test$sections)
	  check <- sum(foo$selected)
	  if(check < 11) next # if blank skip to next iteration
	  
	  symbolsCorrect <- foo$correct
	  symbolsSelected <- foo$selected
	  selectionTime <- foo$selection_time
	  appearanceTime <- foo$appearance_time
	  
	  symbolsdf <- cbind(subid, session, symbols_date, symbolsCorrect, symbolsSelected, selectionTime,appearanceTime)
	  symbolsdf <-as.data.frame(symbolsdf) # convert to data frame
	  
	  colnames(symbolsdf) = c("subid","session","symbols_date", "symbolsCorrect","symbolsSelected", "selectionTime", "appearanceTime")
	  
	  #add trial number
	  symbolsdf$Trial <- seq.int(nrow(symbolsdf))
	  symbolsout <- rbind(symbolsout,symbolsdf)
	}
	# drop first row
	symbolsout <- symbolsout[-1,]
	# define accuracy
	symbolsout$acc <- ifelse(symbolsout$symbolsCorrect == symbolsout$symbolsSelected, 1, 0)
	#define RT
	symbolsout$RT <- as.numeric(symbolsout$selectionTime) - as.numeric(symbolsout$appearanceTime)
	
	# convert session to numberic
	symbolsout$session <- as.numeric(symbolsout$session)
	# sort by session
	symbolsout <- arrange(symbolsout, session)
	
	# remove duplicate observations
	symbolsout <- symbolsout %>% distinct(subid, session,Trial, .keep_all = TRUE) %>% ungroup()
	
	# write the data to a .csv
	
	symbolsout <- symbolsout %>% group_by(subid,session,symbols_date) %>% filter(acc == 1) %>% summarise(symbols = median(RT), symbols_acc = (n()/12)) %>% ungroup()

	sage_symbols<-file.path(tempdir(), "sage_symbols.csv")
	write.csv(symbolsout, sage_symbols,row.names = F )
	output_files$sage_symbols <- sage_symbols
	
	
	
	# merge into one data frame -----------------------------------------------
	s <- readit(sage_symbols)
	p <- readit(sage_prices)
	g <- readit(sage_grids)
	
	
	
	data <- left_join(p, g, by = c("subid","session"))
	data <- left_join(data, s, by = c("subid","session"))
	
	# code for visit if timestamps are more than 7 months apart
	data = data %>% arrange(subid, prices_date)
	
	data$date = as.POSIXct(data$prices_date, origin = "1970-01-01")
	
	data=data %>% mutate(date_diff = date - lag(date)) 
	
	data = data %>% group_by(subid) %>% mutate(visit = case_when(
	  session <= 27 ~ 1,
	  session <= 55 ~ 2,
	  session <= 83 ~ 3,
	  session <= 111 ~ 4,
	  session <= 139 ~ 5,
	  session <= 167 ~ 6,
	  session <= 195 ~ 7,
	  session <= 223 ~ 8,
	))
	
	# keep only the variables I need in the final dataframe
	data <- data %>% select(subid, session,prices_date, symbols_date, grids_date, prices_err,grids, symbols, symbols_acc, visit)
	
	
	# drop session 0 as it is just training
	if (FALSE) {
		# The next line has a bug:  'hasd' is read before it's defined
		hasd <- subset(hasd, session >= 1)
		
		
		get_num_sesh <-  hasd %>% 
		  group_by(subid) %>% 
		  summarise( num_sesh = n(), bl_date = min(date))  %>% ungroup() %>% select(subid, num_sesh, bl_date)
		
		hasd <- left_join(hasd, get_num_sesh, by = "subid")
		
		hasd = hasd %>% group_by(subid, visit) %>% summarize(Prices = mean(prices_err, na.rm = T), Symbols = mean(symbols, na.rm = T), Grids = mean(grids, na.rm = T), arc_date = min(date, na.rm = T), number_sessions_completed = n()) %>% mutate(adherence = number_sessions_completed / 28)
		
		hasd_merged<-file.path(tempdir(), "hasd_merged.csv")
		write.csv(hasd, file = hasd_merged,row.names = F)
		output_files$hasd_merged<-hasd_merged
	}
	
	output_files
}









