# A utility script to find all the data system R scripts in a directory
# tree, parse them one by one, and fill in a template form to generate
# one chunk per script. We write these to files in the outputs/ dir

library(tibble)

PATTERNFILE <- "chunk-generator/sample-pattern.R"

DOMAIN_MAP <- c("AGLU" = "aglu/",
                "ENERGY" = "energy/",
                "EMISSIONS" = "emissions/",
                "SOCIO" = "socioeconomics/",
                "GCAMUSA" = "gcam-usa/")

# Workhorse function to read, parse, construct new strings/code, and substitute
make_substitutions <- function(fn, patternfile = PATTERNFILE) {
  pattern <- readLines(patternfile)

  print(basename(fn))
  filecode <- readLines(fn, warn = FALSE)

  # Isolate the module and level information from the filename
  fn <- gsub("//", "/", fn, fixed = TRUE)
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
  chunkname <- make.names(paste("module", module, gsub("\\.R$", "", basename(fn)), sep = "_"))
  pattern <- gsub(pattern = "CHUNK_NAME", replacement = chunkname, pattern, fixed = TRUE)

  # General function to pull info out of code function calls
  extract_argument <- function(pattern, filecode, stringpos = 2) {
    newinputstring <- ""
    filecode <- filecode[grep("^(\\s)*#", filecode, invert = TRUE)]  # remove comments
    inputlines <- grep(pattern, filecode, fixed = TRUE)
    newinputs <- NULL
    if(length(inputlines)) {
      for(il in inputlines) {
        xsplit <- strsplit(filecode[il], ",")[[1]]
        x <- xsplit[[stringpos]]
        x <- gsub(pattern, "", x, fixed = TRUE)
        x <- gsub("\"", "", x)
        x <- gsub(")", "", x)
        x <- trimws(x)

        if(grepl("COMMON_MAPPINGS", filecode[il])) {
          domain <- "common/"
        } else if (grepl("LEVEL1_DATA", filecode[il])) {
          domain <- ""
        } else if (grepl("(MAPPINGS|ASSUMPTIONS|LEVEL0)", filecode[il])) {
          # Chunks might load mapping/assumption data from their own domain (module),
          # or from somewhere else. Find and parse the string to figure it out
          domaininfo <- regexpr("[A-Z]*_(MAPPINGS|ASSUMPTIONS|LEVEL0)", filecode[il])
          domain <- substr(filecode[il], domaininfo, domaininfo + attr(domaininfo, "match.length") - 1)
          domain <- strsplit(domain, "_")[[1]][1]
          domain <- DOMAIN_MAP[domain]
        } else {
          domain <- ""
        }
        newinputs <- c(newinputs, paste0(domain, x))
      }
    }
    newinputs
  }


  # Find readdata lines
  readdata_string <- extract_argument("readdata(", filecode)
  no_inputs <- is.null(readdata_string)
  if(no_inputs) {
    warning("No inputs for ", basename(fn))
    replacement <- "NULL"
  } else {
    readdata_string_q <- paste0("\"", readdata_string, "\"")
    fileinputs <- grep("/", readdata_string, fixed = TRUE)
    fileprefix <- rep("", length(readdata_string))
    fileprefix[fileinputs] <- "FILE ="
    replacement <- paste0("c(", paste(paste(fileprefix, readdata_string_q), collapse = ",\n"), ")")
  }

  # Replace INPUTS_PATTERN, marking "FILE =" as necessary
  pattern <- gsub(pattern = "INPUTS_PATTERN",
                  replacement = replacement,
                  pattern,
                  fixed = TRUE)

  # Replace LOAD_PATTERN
  if(no_inputs) {
    load_string <- ""
  } else {
    load_string <- paste0("  ", basename(readdata_string), " <- get_data(all_data, ", readdata_string_q, ")")
  }

  pattern <- gsub(pattern = "LOAD_PATTERN",
                  replacement = paste(load_string, collapse = "\n"),
                  pattern,
                  fixed = TRUE)

  # Find output lines
  writedata_string <- extract_argument("writedata(", filecode, stringpos = 1)
  midata_string <- extract_argument("write_mi_data(", filecode, stringpos = 1)
  dataprefix <- c(rep("", length(writedata_string)), rep("XML = ", length(midata_string)))
  writedata_string <- basename(c(writedata_string, midata_string))
  no_outputs <- is.null(writedata_string)

  if(no_outputs) {
    warning("No outputs for ", basename(fn))
    replacement <- "NULL"
  } else {
    writedata_string_q <- paste0("\"", writedata_string, "\"")
    replacement <- paste0("c(", paste(paste0(dataprefix, writedata_string_q), collapse = ",\n"), ")")
  }

  # Replace OUTPUTS_PATTERN
  pattern <- gsub(pattern = "OUTPUTS_PATTERN",
                  replacement = replacement,
                  pattern,
                  fixed = TRUE)

  # Replace DOCOUT_PATTERN
  if(no_outputs) {
    writedata_string_doc <- "(none)"
  } else {
    writedata_string_doc <- paste0("\\code{", writedata_string, "}")
  }
  pattern <- gsub(pattern = "DOCOUT_PATTERN",
                  replacement = paste(writedata_string_doc, collapse = ", "),
                  pattern,
                  fixed = TRUE)

  # Replace MAKEOUT_PATTERN
  if(no_outputs) {
    makeoutputs_string <- ""
  } else {
    makeoutputs_string <- rep(NA, length(writedata_string))
    for(i in seq_along(writedata_string)) {
      txt1 <- paste0('add_title("descriptive title of data") %>%\n',
                     ' add_units("units") %>%\n',
                     ' add_comments("comments describing how data generated") %>%\n',
                     ' add_comments("can be multiple lines") %>%\n',
                     ' add_legacy_name("', writedata_string[i], '") %>%\n',
                     ' add_precursors("precursor1", "precursor2", "etc") %>%\n',
                     ' # typical flags, but there are others--see `constants.R` \n')
      if(dataprefix[i] == "") {
        txt2 <- "add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR)"
      } else {
        txt2 <- "add_flags(FLAG_NO_TEST) %>%\n  add_xml_data()"
      }
      makeoutputs_string[i] <- paste("tibble() %>%\n  ", txt1, txt2, "->\n  ", writedata_string[i])
    }
    makeoutputs_string <- paste(makeoutputs_string, collapse = "\n")
  }


  pattern <- gsub(pattern = "MAKEOUT_PATTERN",
                  replacement = makeoutputs_string,
                  pattern,
                  fixed = TRUE)

  # Replace RETURNOUT_PATTERN
  pattern <- gsub(pattern = "RETURNOUT_PATTERN",
                  replacement = paste(writedata_string, collapse = ", "),
                  pattern,
                  fixed = TRUE)

  pattern
}



# ----------------------- MAIN -----------------------

files <- list.files("~/Documents/Work/Code/gcam-data-system-OLD/",
                    pattern = "*.R$", full.names = TRUE, recursive = TRUE)
# Limit to scripts in the processing code folers
files <- files[grepl("processing-code", files, fixed = TRUE)]

linedata <- list()

for(fn in files) {
  # Isolate the module and level information from the filename
  fn <- gsub("//", "/", fn, fixed = TRUE)
  x <- strsplit(fn, "/")[[1]]
  level <- x[length(x) - 1]
  module <- gsub("-processing-code", "", x[length(x) - 2], fixed = TRUE)
  newfn <- file.path("chunk-generator", "outputs", paste0("module-", module, "-", level, ".R"))

  out <- NULL
  try(out <- make_substitutions(fn))
  if(is.null(out)) {
    warning("Ran into error with ", basename(fn))
  } else {
    newfn <- paste0("chunk-generator/outputs/chunk_", basename(fn))
    cat(out, "\n", file = newfn, sep = "\n", append = FALSE)
  }
  linedata[[newfn]] <- tibble(filename = basename(newfn),
                              lines = length(readLines(fn)))

}

linedata <- dplyr::bind_rows(linedata)
readr::write_csv(linedata, "chunk-generator/linedata.csv")
