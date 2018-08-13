#' Get an adjacency matrix from an edge data frame
#'
#' Get an adjacency matrix from an edge data frame.
#' If no weights are provided all weights are set to 1
#'
#' @param edges a data frame with columns a, b, and (optionally) weight
#' @param nodes an array of the unique node IDs used in a, b (inferred if not provided)
#' @return an adjacency matrix
#' @export
adjacencyMatFromDF <- function( edges, nodes = NULL, cluster = NULL ) {

  if ( is.null( nodes ) ) nodes <- getUniqueNodesFromEdgesDF( edges )

  weighted <- !is.null( edges$weight )

  n <- length( nodes )

  # Check if we want a sparse matrix
  if ( ( nrow(edges)*2 < n^2/3 ) && ( "Matrix" %in% installed.packages()[,"Package"] ) ) {
    adj_mat <- Matrix::sparseMatrix( i = edges$a, j = edges$b, x = edges$weight,
                                     dims = c(n,n), use.last.ij = T, symmetric = T )
  } else {
    if( is.null( cluster ) )
      adj_mat <- mapply( function(i){
        mask_a <- edges$a == nodes[i]
        mask_b <- edges$b == nodes[i]
        jays <- match( c( edges$b[mask_a], edges$a[mask_b] ), nodes )
        w <- numeric( n )
        w[ jays ] <- c( edges$weight[mask_a], edges$weight[mask_b] )
        return ( w )
      }, 1:n, SIMPLIFY = TRUE )
    else
      adj_mat <- parallel::parSapply( cl = cluster, X = 1:n, FUN = function (i){
        mask_a <- edges$a == nodes[i]
        mask_b <- edges$b == nodes[i]
        jays <- match( c( edges$b[mask_a], edges$a[mask_b] ), nodes )
        w <- numeric( n )
        w[ jays ] <- c( edges$weight[mask_a], edges$weight[mask_b] )
        return ( w )
      }, simplify = T )
  }
  return ( adj_mat )
}
