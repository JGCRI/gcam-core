

# Playing around!


#' graph_chunks
#'
#' @param chunklist A tibble of chunks
#' @param plot_gcam Plot a node for GCAM (all XMLs feed to)?
#' @param include_disabled Plots nodes of disabled chunks?
#' @return A plot
#' @export
graph_chunks <- function(chunklist, plot_gcam = FALSE, include_disabled = FALSE) {

  if(missing(chunklist)) {
    chunklist <- find_chunks(include_disabled = include_disabled)
  }
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

  chunklist$num <- 1:nrow(chunklist)
  chunklist$modulenum <- as.numeric(as.factor(chunklist$module))
  chunkinputs %>%
    left_join(chunklist, by = "name") %>%
    select(name, input, num) ->
    chunkinputs
  cat("Found", nrow(chunkinputs), "chunk data requirements\n")
  chunkoutputs %>%
    left_join(chunklist, by = "name") %>%
    select(name, output, to_xml, num) ->
    chunkoutputs
  cat("Found", nrow(chunkoutputs), "chunk data products\n")

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
  mat <- matrix(0, nrow=nrow(chunklist), ncol=nrow(chunklist))
  colnames(mat) <- chunklist$chunk
  for(i in seq_len(nrow(edgelist))) {
    mat[edgelist$num.y[i], edgelist$num.x[i]] <- 1
  }

  # Plot it
  set.seed(1224)
  plot(igraph::graph.adjacency(mat),
       vertex.color = rainbow(length(unique(chunklist$modulenum)))[chunklist$modulenum],
       #      vertex.size = chunklist$noutputs p* 3,
       #      vertex.label.dist = 1,
       vertex.label.cex = .5,
       vertex.size = 5,
       edge.arrow.size = 0.5,
       margin = -0.2)

  invisible(mat)
}
