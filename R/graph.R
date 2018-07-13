
#' graph_chunks
#'
#' @param module_filter Optional name of module to filter by
#' @param plot_gcam Plot a node for GCAM (all XMLs feed to)?
#' @param include_disabled Plots nodes of disabled chunks?
#' @param quiet Suppress messages?
#' @return Adjacency matrix showing chunk-to-chunk data flows
#' @importFrom grDevices rainbow
#' @importFrom graphics plot title
#' @export
graph_chunks <- function(module_filter = NULL,
                         plot_gcam = FALSE,
                         include_disabled = FALSE,
                         quiet = TRUE) {

  output <- to_xml <- module <- name.y <- name <- disabled <- input <- num <-
    NULL                              # silence notes on package check.

  assert_that(is.null(module_filter) | is.character(module_filter))
  assert_that(is.logical(plot_gcam))
  assert_that(is.logical(include_disabled))
  assert_that(is.logical(quiet))

  chunklist = find_chunks(include_disabled = include_disabled)
  chunklist$modulenum <- as.numeric(as.factor(chunklist$module))
  vertexcolors <- palette()

  chunkinputs <- chunk_inputs(chunklist$name)
  chunkoutputs <- chunk_outputs(chunklist$name)

  if(plot_gcam) {
    chunkoutputs %>%
      rename(input = output) %>%
      mutate(name = "GCAM", from_file = FALSE) %>%
      filter(to_xml) %>%
      bind_rows(chunkinputs) ->
      chunkinputs
    tibble(name = "GCAM", module = "GCAM", chunk = "GCAM") %>%
      bind_rows(chunklist) ->
      chunklist
  }

  if(!is.null(module_filter)) {
    # We want just chunks in 'module' AND anything that feeds them
    cl_main <- filter(chunklist, module == module_filter)

    if(nrow(cl_main) == 0) {
      warning("No chunks in module ", module_filter)
      return(NULL)
    }

    # Join chunks to their inputs, and then to outputs; looking for
    # chunks that feed chunks in the current (filtered) module
    cl_main %>%
      left_join(chunkinputs, by = "name") %>%
      left_join(chunkoutputs, by = c("input" = "output")) %>%
      filter(!is.na(name.y)) %>%
      select(name.y) %>%
      distinct ->
      module_feeders

    # Add those into the main chunklist
    chunklist %>%
      filter(name %in% module_feeders$name.y) %>%
      bind_rows(cl_main) %>%
      distinct ->
      chunklist

    # AGLU special case
    #     x <- substr(chunklist$chunk, 1, 2) %in% c("L1")
    #     y <- substr(chunklist$chunk, 1, 3) %in% c("LA1", "LB1")
    # chunklist <- chunklist[x | y,]

    chunkinputs <- chunk_inputs(chunklist$name)
    chunkoutputs <- chunk_outputs(chunklist$name)
  }

  # Filter (unless caller has asked to include disabled chunks)
  chunklist <- filter(chunklist, !disabled | include_disabled)

  chunklist$num <- seq_len(nrow(chunklist))
  chunkinputs %>%
    left_join(chunklist, by = "name") %>%
    select(name, input, num) ->
    chunkinputs
  if(!quiet) cat("Found", nrow(chunkinputs), "chunk data requirements\n")
  chunkoutputs %>%
    left_join(chunklist, by = "name") %>%
    select(name, output, to_xml, num) ->
    chunkoutputs
  if(!quiet) cat("Found", nrow(chunkoutputs), "chunk data products\n")

  # Compute number of outputs
  chunkoutputs %>%
    group_by(name) %>%
    summarise(noutputs = n()) %>%
    right_join(chunklist, by = "name") ->
    chunklist

  # Compute edges (dependencies)
  chunkinputs %>%
    inner_join(chunkoutputs, by = c("input" = "output")) ->
    edgelist

  # Make an adjacency matrix
  mat <- matrix(0, nrow = nrow(chunklist), ncol = nrow(chunklist))
  colnames(mat) <- chunklist$chunk
  for(i in seq_len(nrow(edgelist))) {
    mat[edgelist$num.y[i], edgelist$num.x[i]] <- 1
  }

  # Plot it
  set.seed(1224)
  g <- igraph::graph.adjacency(mat)
  coords <- igraph::layout_nicely(g)

  # Use 'disabled' status as color if we're plotting them; otherwise, module
  if(include_disabled) {
    vc <- rainbow(2)[chunklist$disabled + 1]
  } else {
    vc <- vertexcolors[chunklist$modulenum]
  }

  plot(g,
       vertex.color = vc,
       #      vertex.size = chunklist$noutputs p* 3,
       #      vertex.label.dist = 1,
       vertex.label.cex = .5,
       vertex.label.color = "grey",
       vertex.size = 5,
       edge.arrow.size = 0.3,
       layout = coords)
  title(module_filter, sub = paste("DSR-integration", date()))

  invisible(mat)
}
