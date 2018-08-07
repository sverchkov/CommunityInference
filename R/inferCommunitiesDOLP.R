#' Community detection, "dropout" label propagation
#'
#' An implementation of community detection by label propagation in an undirected weighted graph based on
#' Raghavan, Albert, Kumara. Phys Rev E 76, 036106 (2007)
#'
#' This version has "dropout" behavior -- asynchronysity is implemented by only updating a random subset of nodes
#'
#' @param unique_edges a data frame with columns a, b, weight representing the connections between nodes.
#' We assume undirected graph, and therefore b < a.
#' @param async_prop proportion of nodes to update before synchronous update
#' @param check_unique whether to check edges data frame for uniqueness
#'
#' @return a data frame with two columns,
#' node--node id (taken from a, b of input) and
#' label--unique cluster/community ID.
#' @import dplyr
#' @import futile.logger
#' @author Yuriy Sverchkov
#' @export
inferCommunitiesDOLP <- function( unique_edges, async_prop = .5, check_unique = F ){

  # For this algorithm it's more convenient to just have all edges listed twice
  flog.trace( "Converting distinct edges to bidirectional edges..." )
  edges <- union_all( select( unique_edges, src = a, tgt = b, weight ),
                      select( unique_edges, src = b, tgt = a, weight ) )

  if ( check_unique ) {
    flog.trace( "Making sure edges are unique..." )
    edges <- edges %>% distinct( src, tgt, .keep_all = T )
  }

  # Create node table and initialize label table
  flog.trace( "Making node table..." )
  nodes <- distinct( edges, node = src ) %>% mutate( label = node )
  nodes_array <- nodes$node

  repeat {
    flog.trace( "Label propagation: Number of communities: %s.", nrow( distinct( nodes, label ) ) )

    # Get votes from all
    flog.trace( "Computing votes..." )
    votes <- voteForLabelPropagation( edges, nodes )

    # Check whether we're done
    flog.trace( "Checking stop condition...")
    checks <- votes %>% ungroup() %>%
      left_join( nodes, by = "node" ) %>%
      group_by( node ) %>%
      summarize( concensus = any( label == new_label ) ) %>%
      ungroup() %>%
      summarize( done = all( concensus ) )

    if ( checks$done ) break;

    # Update a fraction of nodes
    flog.trace("Updating labels...")
    new_nodes <- votes %>%
      sample_n( 1 ) %>%
      ungroup() %>%
      sample_frac( async_prop )

    nodes <- left_join( nodes, new_nodes, by = "node" ) %>%
      mutate( label = if_else( is.na( new_label ), label, new_label ) ) %>%
      select( node, label )
  }

  return ( nodes )
}
