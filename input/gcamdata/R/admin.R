# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.


# Following functions are extremely specific administrative ones
# Don't worry about covering them in tests

# nocov start

#' normalize_files
#'
#' Normalize line endings for all package input data.
#'
#' @param root Folder root to scan, character
#' @return Nothing - run for side effects only.
#' @note Set \code{root} to "./extdata" in the git directory, not the package root, to make changes that 'stick'.
#' @details Some GCAM input datafiles have bad line endings, and/or
#' don't have a final newline. This utility script converts all files to have Unix line endings (\code{\\n}) and a final newline.
#' @author BBL
normalize_files <- function(root = system.file("extdata", package = "gcamdata")) {
  assert_that(is.character(root))
  message("Root: ", root)

  # Get a list of all CSV input files
  files <- list.files(root, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

  for(f in seq_along(files)) {
    shortfn <- gsub(root, "", files[f])
    size <- round(file.size(files[f]) / 1024 / 1024, 3)  # MB
    message(f, "/", length(files), ": ", shortfn, ", ", size, " Mb ", appendLF = FALSE)

    # Read file and then write it back out
    message("\tReading...", appendLF = FALSE)
    txt <- readLines(files[f], warn = FALSE)
    uc_size <- format(utils::object.size(txt), units = "Mb")
    message("OK. ", uc_size, " uncompressed")

    message("\tWriting...", appendLF = FALSE)
    writeLines(txt, files[f])
    message("OK")
  }
}


#' add_column_types_header_line
#'
#' One-off function to insert a `Column types` header line into input files.
#'
#' @param overwrite Overwrite any previous column type lines? Logical
#' @return Nothing.
add_column_types_header_line <- function(overwrite = FALSE) {

  files <- list.files(path = "inst/extdata/", pattern = "csv$", full.names = TRUE, recursive = TRUE)

  colchars <- c("character" = "c", "integer" = "i", "numeric" = "n", "double" = "d", "logical" = "I")

  for(f in files) {
    cat(f, "...")
    header <- find_header(f)
    original_header_length <- length(header)

    # Remove any previous Column types entry
    if(overwrite) {
      column_rows <- grepl("^# Column types", header)
      if(length(column_rows)) {
        header <- header[!column_rows]
      }
    }

    # A few files are custom and don't have headers; skip
    if(length(header)) {

      # Remopve any existing separator line
      if(header[length(header)] == "# ----------") {
        header <- header[-length(header)]
      }

      # Read the data and infer column classes
      dat <- utils::read.csv(f, comment.char = "#", stringsAsFactors = F)
      coltypes_list <- lapply(dat, class)

      allNAs <- unlist(lapply(dat, function(x) all(is.na(x))))
      coltypes_list[allNAs] <- "character"  # we want all-NA columns to be character, not logical

      coltypes <- unlist(coltypes_list)
      coltypes <- paste(colchars[coltypes], collapse = "")
      cat(coltypes, "\n")

      # Now we want to write the header, the new columns line, the separator, and then the data

      outfile <- f
      fullfile <- readLines(f)
      cat(header, file = outfile, sep = "\n")
      cat(paste("# Column types:", coltypes, "\n"), file = outfile, append = TRUE)
      cat("# ----------", file = outfile, sep = "\n", append = TRUE)
      cat(fullfile[(original_header_length + 1):length(fullfile)],
          file = outfile, sep = "\n", append = TRUE)

    } else {
      cat("SKIP\n")
      next
    }
  }
}

#' rename_gcamdata_chunks
#'
#' Renames gcamdata chunks to new format, including getting rid of LA/LBs, cleaning up module
#' names, and replacing "batch" with "xml". Also renames function within scripts
#'
#' @return Nothing
#' @author ENL 2023
rename_gcamdata_chunks <- function() {

  # Part 1: Rename files
  # Get list of available modules
  devtools::load_all()
  module_options <- c("zaglu", "zemissions", "zsocio", "zenergy", "zgcamusa",
                      "zwater", "zclimate", "zmodeltime")

  # Get list of files to rename
  # All scripts in R/ that contain "zchunk"
  files_to_rename <- data.frame(orig_file_name = list.files("R/", full.names = TRUE)) %>%
    filter(grepl("zchunk", orig_file_name))

  # Read 3rd line of each file which should contain module name
  # This avoids doing any manual sector matching
  first_three_lines <- lapply(files_to_rename$orig_file_name, readLines, 3)
  files_to_rename$third_line <- lapply(first_three_lines, `[[`, 3)

  # Error if third line isn't module name. Need to manually fix these
  files_to_rename %>% filter(!grepl("#' module_", third_line)) -> third_line_wrong
  if(nrow(third_line_wrong) != 0){
    stop("Third line isn't module name in ", third_line_wrong$orig_file_name, ". Needs to be fixed manually")
  }

  # Get module names from column read in above to use during rename
  files_to_rename %>%
    mutate(mod_type = sub(".*#' module_", "", third_line), # Extract text after "module_"
           mod_type = sub("\\_.*", "", mod_type), # Extract text before first "_". This should be module name
           mod_type = paste0("z", mod_type)) %>% # Put "z" in front of module to keep files ordered
    select(-third_line) -> files_w_mods

  # If there are "socioeconomics" sectors, change them to "socio"
  # Change all files with USA in name to module "gcamusa"
  files_w_mods %>%
    mutate(mod_type = gsub("socioeconomics", "socio", mod_type),
           mod_type = if_else(grepl("USA", orig_file_name), "zgcamusa", mod_type)) -> files_w_mods_fixed

  # Error if module not in list of available modules. Need to manually fix these
  files_w_mods_fixed %>% filter(!mod_type %in% module_options) -> not_in_available_mods
  if(nrow(not_in_available_mods) != 0){
    stop("Module not available for ", not_in_available_mods$orig_file_name, ". Needs to be fixed manually")
  }

  # Get new names for files
  # Replace LA and LB with L
  # Get rid of appended _USA since we already specify with zgcamusa
  # Get rid of "batch" and move xml to after module name
  files_w_mods_fixed %>%
    mutate(new_file = paste0("R/", mod_type, "_", sub("^.*?_", "", orig_file_name)),
           new_file = gsub("LA|LB", "L", new_file),
           new_file = gsub("_USA", "", new_file),
           new_file = gsub("_xml", "", new_file),
           new_file = gsub("batch", "xml", new_file)) %>%
    select(-mod_type)-> name_map

  # Check for duplicate file names in entire R folder
  data.frame(files = list.files("R/", full.names = TRUE)) %>%
    rbind(data.frame(files = name_map$new_file)) %>%
    group_by(files) %>%
    filter(dplyr::n()>1) -> duplicates
  # Error if there are, since we can't write two files with same name
  if(nrow(duplicates > 0)){
    stop("Duplicate file names: ", unique(duplicates$files) ," Fix this before continuing")
  }

  # Do the rename and let users know:
  file.rename(name_map$orig_file_name, name_map$new_file)
  if(nrow(name_map)>0){  message(nrow(name_map), " file(s) renamed") }

  # Part 2: Fix content of files, including file name
  # Note: Only fixing content of files that we updated names of above
  files_w_mods %>%
    left_join(name_map, by = c("orig_file_name")) -> updated_files

  # Replace "socioeconomics" with "socio" in function names
  updated_files %>%
    filter(grepl("zsocioeconomics", mod_type)) -> socio_chunks

  for(name in socio_chunks$new_file){
    #Read file
    tx  <- readLines(name)
    #Replace "module_socioeconomics" with "module_socio"
    tx_rpl <- gsub(pattern = "module_socioeconomics", replace = "module_socio", tx)
    # Save file with new text
    writeLines(tx_rpl, con=name)
  }
  # If we updated any chunks let user know
  if(nrow(socio_chunks > 0)){
    message("Replaced ", nrow(socio_chunks), " socioeconomics function name(s) with \"socio\"")
  }

  # Get rid of "_USA" and only include module_gcamusa in function names
  # Have to be careful to only change overall function name and not any outputs
  updated_files %>%
    filter(mod_type == "zgcamusa") -> gcamusa_chunks

  for(name in gcamusa_chunks$new_file){
    #Read file and get the line numbers that need updating
    tx  <- readLines(name)
    tx_rpl <- tx
    lines_to_update <- which(grepl("module_.*USA", tx))
    #Remove _USA in function name and replace any module name with "gcamusa"
    for(i in lines_to_update){
      tx_rpl[i] <- gsub("_USA", "", tx_rpl[i])
      tx_rpl[i] <- gsub(sub("\\_.*", "", sub(".*module_", "", tx_rpl[i])), "gcamusa", tx_rpl[i])
    }
    # Save file with new text
    writeLines(tx_rpl, con=name)
  }

  # If we updated any chunks let user know
  if(nrow(gcamusa_chunks > 0)){
    message("Updated function name in ", nrow(gcamusa_chunks), " gcamusa chunk(s)")
  }

  # Replace "batch" with "XML" in function name
  updated_files %>%
    filter(grepl("_xml_", new_file)) -> xml_chunks

  for(name in xml_chunks$new_file){
    tx  <- readLines(name)
    tx_rpl <- tx
    #Remove _USA in module function name
    lines_to_update <-which(grepl("module_.*batch", tx_rpl))
    for(i in lines_to_update){
      tx_rpl[i] <- gsub("_batch", "", tx_rpl[i])
    }
    writeLines(tx_rpl, con=name)
  }

  # If we updated any chunks let user know
  if(nrow(xml_chunks > 0)){
    message("Updated function name in ", nrow(xml_chunks), " XML chunk(s)")
  }

  # Get rid of all LA's and LB's in chunks since these have no meaning
  updated_files %>%
    filter(grepl("LA|LB", orig_file_name)) -> LALB_files

  for(name in LALB_files$new_file){
    tx  <- readLines(name)
    # Get name of original file to check if LA or LB
    orig_name <- LALB_files[which(LALB_files$new_file == name), 1]
    if(grepl("LA", orig_name)){
      # For the LA's the world DECLARE also has an "LA"
      # Workaround: grab the integer following the "LA" and search and replace LA + integer
      # Also prevent changing the "original file" note by searching for strings starting with _
      number <- substr(sub(".*LA", "", orig_name),1,1)
      # Get replace text for LAs
      tx_rpl <- gsub(pattern = paste0("_LA", number), replace = paste0("_L", number), x = tx)
    }
    else{
      # Get replace text for LB's
      tx_rpl <- gsub(pattern = "_LB", replace = "_L", x = tx)
    }
    # Save file with new text
    writeLines(tx_rpl, con=name)
  }

  # If we updated any chunks let user know
  if(nrow(LALB_files > 0)){
    message("Removed LA or LB in ", nrow(LALB_files), " function name(s)")
  }
} # end rename_gcamdata_chunks

# nocov end
