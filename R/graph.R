

# Playing around!


#' graph_chunks
#'
#' @param chunklist A tibble of chunks
#' @return A plot
#' @export
graph_chunks <- function(chunklist = find_chunks()) {
  chunklist$num <- 1:nrow(chunklist)
  chunklist$modulenum <- as.numeric(as.factor(chunklist$module))
  chunk_inputs() %>%
    left_join(chunklist, by = "name") %>%
    select(name, input, num) ->
    chunkinputs
  cat("Found", nrow(chunkinputs), "chunk data requirements\n")
  chunk_outputs() %>%
    left_join(chunklist, by = "name") %>%
    select(name, output, num) ->
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
  set.seed(123)
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
