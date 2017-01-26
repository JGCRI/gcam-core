# Take `sample-chunk-pattern.R` and use it as a pattern

# TODO:
# Handle A_common_data, etc. correctly
# Put folder onto file inputs, e.g. socioeconomics/GCAM3_population
#   look through examples
#   basically I think if not COMMON or something like that prepend module to sourcedata_string_q


library(magrittr)

DS_CODE <- ""
DS_DATA <- ""
PATTERNFILE <- "sample-generator/sample-pattern.R"

pattern <- readLines(PATTERNFILE)

fn <- "/Users/d3x290/Desktop/gcam-data-system-master/aglu-processing-code/level1/LB109.ag_an_ALL_R_C_Y.R"
filecode <- readLines(fn)

# Isolate the module and level information from the filename
x <- strsplit(fn, "/")[[1]]
level <- x[length(x) - 1]
module <- gsub("-processing-code", "", x[length(x) - 2], fixed = TRUE)

# Replace file info
pattern <- gsub(pattern = "ORIGINALFILE_PATTERN",
                replacement = basename(fn),
                pattern,
                fixed = TRUE)
pattern <- gsub(pattern = "MODULE_PATTERN",
                replacement = module,
                pattern,
                fixed = TRUE)
pattern <- gsub(pattern = "LEVEL_PATTERN",
                replacement = level,
                pattern,
                fixed = TRUE)

# Replace CHUNK_NAME with file name (minus .R)
# Use make.names to ensure syntactically valid
newfn <- make.names(gsub("\\.R$", "", basename(fn)))
pattern <- gsub(pattern = "CHUNK_NAME", replacement = newfn, pattern, fixed = TRUE)

extract_argument <- function(pattern, dsfile, stringpos = 2) {
  newinputstring <- ""
  inputlines <- grep(pattern, dsfile, fixed = TRUE)
  if(length(inputlines)) {
    newinputs <- NULL
    for(il in inputlines) {
      x <- strsplit(dsfile[il], ",")[[1]][stringpos]
      cat(il, x, "\n")
      x <- gsub(pattern, "", x, fixed = TRUE)
      x <- gsub("\"", "", x)
      x <- gsub(")", "", x)
      newinputs <- c(newinputs, trimws(x))
    }
  }
  newinputs
}

# Find sourcedata lines
sourcedata_string <- extract_argument("sourcedata(", filecode)
sourcedata_string_q <- paste0("\"", sourcedata_string, "\"")
sourcedata_string_qf <- paste("FILE =", sourcedata_string_q)

# Find domains associated with sourcedata lines
domain_string <- extract_argument("sourcedata(", filecode, stringpos = 1)

# Find readdata lines
readdata_string <- extract_argument("readdata(", filecode)
readdata_string_q <- paste0("\"", readdata_string, "\"")

# Replace INPUTS_PATTERN, marking "FILE =" as necessary
pattern <- gsub(pattern = "INPUTS_PATTERN",
           replacement = paste0("c(", paste(c(sourcedata_string_qf, readdata_string_q), collapse = ",\n"), ")"),
           pattern,
           fixed = TRUE)

# Replace LOAD_PATTERN
load_string <- paste0("  ", c(sourcedata_string, readdata_string),
                      " <- get_data(all_data, ",
                      c(sourcedata_string_q, readdata_string_q), ")")

pattern <- gsub(pattern = "LOAD_PATTERN",
                replacement = paste(load_string, collapse = "\n"),
                pattern,
                fixed = TRUE)

# Construct COMMENTED_CODE_PATTERN

# Find output lines
writedata_string <- extract_argument("writedata(", filecode, stringpos = 1)

# Replace OUTPUTS_PATTERN
writedata_string_q <- paste0("\"", writedata_string, "\"")
pattern <- gsub(pattern = "OUTPUTS_PATTERN",
                replacement = paste0("c(", paste(writedata_string_q, collapse = ",\n"), ")"),
                pattern,
                fixed = TRUE)

# Replace MAKEOUT_PATTERN
makeoutputs_string <- paste(writedata_string, collapse = " ->\n  ")
pattern <- gsub(pattern = "MAKEOUT_PATTERN",
                replacement = makeoutputs_string,
                pattern,
                fixed = TRUE)

# Replace RETURNOUT_PATTERN
pattern <- gsub(pattern = "RETURNOUT_PATTERN",
                replacement = paste(writedata_string, collapse = ", "),
                pattern,
                fixed = TRUE)

# Write out chunk (appending to proper file)
cat(pattern, file = "sample-generator/test.R", sep = "\n")
