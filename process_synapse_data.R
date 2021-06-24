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

source(process_json_files)

args <- commandArgs()
input_table_id <- args[2]
output_folder_id <- args[3]
auth_token <- Sys.getenv("synapse_access_token")

# download data from Synapse
synLogin(authToken=auth_token)

latest_date_queried_file_name <- "latestDateQueried.txt"
latest_date_entity_id <- synFindEntityId(latest_date_queried_file_name, parent=output_folder_id)
if (is.null(latest_date_entity_id)) {
	# no high water mark, so just get all the data
	results <- synTableQuery(sprintf("SELECT createdOn, \"data.json\" FROM %s", input_table_id))
} else {
	latest_date_file_entity<-synGet(latest_date_entity_id)
	# read file contents
	latest_date_queried<-readr::read_file(latest_date_file_entity$path)
	results <- synTableQuery(sprintf("SELECT createdOn, \"data.json\" FROM %s where createdOn>%s", input_table_id, latest_date_queried))
}
data <- synDownloadTableColumns(results, columns = list("data.json"))

files = as.data.frame(data)
files$data = paste0('"', files$data, '"')

# apply processing
result_file_names <- process_json_files(files$data)

# upload results to Synapse
tm <- as.POSIXlt(Sys.time(), "UTC")
folder_name<-strftime(tm , "%Y-%m-%dT%H.%M.%S%z")
folder<-Folder(folder_name, output_folder_id)
folder<-synStore(folder)
for (file_name in result_file_names) {
	synStore(file_name, folder)
}

# Store new high water mark
new_latest_date<-max(as.data.frame(results)$createdOn)
latest_date_file<-file.path(tempdir(), latest_date_queried_file_name)
# write new_latest_date to latest_date_file
readr::write_file(new_latest_date, latest_date_file)
latest_date_file<-File(latest_date_file, output_folder_id)
synStore(latest_date_file)
