Steps to run the GCAM diagnostics:

1) Run the model verification queries on the output of at least one scenario. It is recommended to running the batch queries to generate CSV files directly instead of XLS.  If writing directly to CSV you can skip to step 5.
2) Split runs into separate sheets, and put all queries within the same sheet (don't generate figures). A typical 14-region scenario takes 15 minutes to write the data out, and a typical 30-region run takes about 30 minutes.
3) Open the XLS file that was created, and save each scenario as its own CSV. Insert a row above each query, put the name of the query above the column called "scenario" (column B), and delete the column called "title". This will put the data into a format similar to what the R code is expecting.
4) Open the .CSV files in a text editor, and delete the string of commas trailing the query name at the top of each table. Every query will have the same number of trailing commas so a find and replace is pretty simple.
5) Open the diagnostics.R script. If R was not already open, this will by default set the working directory to the correct path. Otherwise, set the working directory to this folder (setwd() function, or select from a drop-down menu).
6) Set the FILES to the correct names of the CSV files in the folder that was created above. Any number of scenarios can be set here; the default is to use two, for a pair-wise comparison.
7) Set the BASE_SCENARIO_NAME to whatever the "reference" is going to be. This is the name of the scenario in the model output, not the file name, though for convenience it may make sense to name the CSV files exactly according to their scenario names.
8) Set the OUTPUT_DIR where the figures will be placed. By default this is figures.
9) Run the script:
source("diagnostics.R")
