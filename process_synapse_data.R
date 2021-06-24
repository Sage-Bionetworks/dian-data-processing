#
#
#  This script 'wraps' the JSON file processing logic with the interface to Synapse
#
#  input_table_id: The table to draw input from
#  output_folder_id: The parent folder to put output files in.  The tree structure will be:
#		<output_folder_id>/<date>/
#
#  To run from the command line:
#  put a Synapse access token (personal access token) in an environment
#  variable, synapse_access_token, then run:
#
#     Rscript process_synapse_data.R <input_table_id> <output_folder_id>
#

library(synapser)
library(readr)

source("process_json_files.R")

args <- commandArgs(trailingOnly=TRUE)
input_table_id <- args[1]
output_folder_id <- args[2]
auth_token <- Sys.getenv("synapse_access_token")

# download data from Synapse
synLogin(authToken=auth_token)

latest_date_queried_file_name <- "latestDateQueried.txt"
latest_date_entity_id <- synFindEntityId(latest_date_queried_file_name, parent=output_folder_id)
if (is.null(latest_date_entity_id)) {
	# no high water mark, so just get all the data
	message("No record of latest processed date found.")
	results <- synTableQuery(sprintf("SELECT createdOn, \"data.json\" FROM %s", input_table_id))
} else {
	message(sprintf("Latest createdOn date found in %s", latest_date_entity_id))
	latest_date_file_entity<-synGet(latest_date_entity_id)
	# read file contents
	latest_date_queried<-readr::read_file(latest_date_file_entity$path)
	results <- synTableQuery(sprintf("SELECT createdOn, \"data.json\" FROM %s where createdOn>%s", input_table_id, latest_date_queried))
}
data <- synDownloadTableColumns(results, columns = list("data.json"))

files = as.data.frame(data)

if (length(files$data)==0) {
	message("\nNo new data to process.")
} else {
	message(sprintf("\nProcessing %s records.", length(files$data)))
	# apply processing
	result_file_names <- process_json_files(files$data)
	message("Done processing new records.")
	
	# upload results to Synapse
	tm <- as.POSIXlt(Sys.time(), "UTC")
	folder_name<-strftime(tm , "%Y-%m-%dT%H.%M.%S%z")
	folder<-Folder(folder_name, output_folder_id)
	folder<-synStore(folder)
	for (file_name in result_file_names) {
		result_file <- File(file_name, folder)
		synStore(result_file)
	}
	message(sprintf("Uploaded %s output files to %s.", length(result_file_names), folder_name))
	
	# Store new high water mark
	new_latest_date<-max(read.csv(results$filepath)$createdOn)
	latest_date_file<-file.path(tempdir(), latest_date_queried_file_name)
	# write new_latest_date to latest_date_file
	readr::write_file(as.character(new_latest_date), latest_date_file)
	latest_date_file<-File(latest_date_file, output_folder_id)
	synStore(latest_date_file)
	message(sprintf("Stored new latest-created-on date, %s.", new_latest_date))
}

