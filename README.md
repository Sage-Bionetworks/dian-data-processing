# dian-data-processing

### Usage

To run from the command line, put a Synapse access token (personal access token) in an environment variable, `synapse_access_token`, then run:

```
Rscript process_synapse_data.R <input_table_id> <output_folder_id>
```

where

- `input_table_id`: The table to draw input from.  (The column 'data.json' is expected.)

- `output_folder_id`: The parent folder to put output files in.  The tree structure for results is:

	`<output_folder_id>/<date>/<file>.csv`

### Testing
Before using the code be sure it works by running the tests in the `tests/testthat` directory:

```
testthat::test_dir(testthat::test_path())
```