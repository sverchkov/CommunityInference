#' Community detection, asynchronous label propagation
#'
#' An implementation of community detection by label propagation in an undirected weighted graph based on
#' Raghavan, Albert, Kumara. Phys Rev E 76, 036106 (2007)
#'
#' This is fully asynchronous -- labels are updated one at a time in a random order.
#'
#' @param unique_edges a data frame with columns a, b, weight representing the connections between nodes.
#' We assume undirected graph, and therefore b < a.
#'
#' @return a list of two vectors,
#' node--node id (taken from a, b of input) and
#' label--unique cluster/community ID.
#' @import Matrix
#' @import futile.logger
#' @author Yuriy Sverchkov
#' @export
inferCommunitiesASLP <- function( edges ){

  flog.trace( "Counting nodes..." )

  # First we get a sorted list of all node IDs
  nodes <- sort( unique( c( edges$a, edges$b ) ) )
  n <- length( nodes )
  # Starting labels
  labels <- 1:n

  flog.trace( "Making weight matrix..." )

  # We're going to store the weights in a matrix
  # Figure out if matrix should be sparse
  neighbors <- Matrix( data = 0.0, nrow = n, ncol = n, sparse = ( nrow(edges)*2 > n^2/3 ) )
  for ( row in 1:nrow( edges ) ){
    i <-  which( nodes == edges$a[row] )
    j <-  which( nodes == edges$b[row] )
    neighbors[i,j] <- ( neighbors[j,i] <- edges$weight[row] )
  }

  old_labels <- -1
  while ( any( old_labels != labels ) ) {

    active_labels <- unique( labels )
    l <- length( active_labels )

    # Ensure that labels are sequential so that we can use them as indeces
    labels <- match( labels, active_labels )

    flog.trace( "Label propagation: Number of communities: %s.", l )

    old_labels <- labels

    voting_order <- sample.int( n, replace = T )

    for ( i in voting_order ){
      votes <- numeric( l )
      for ( j in 1:n ) votes[ labels[ j ] ] <- votes[ labels[ j ] ] + neighbors[ i, j ]
      winners <- which( votes == max( votes ) )

      # Label update
      labels[i] =
        if ( length( winners ) == 1 ) winners
        else if ( any( winners == labels[ i ] ) ) # Don't change your own vote if it's a winner
          labels[i]
        else
          sample( winners, size = 1 )
    }
  }

  return ( list( node = nodes, label = labels ) )
}
