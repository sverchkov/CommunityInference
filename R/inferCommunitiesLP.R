#' Community detection, label propagation
#'
#' An implementation of community detection by label propagation in an undirected weighted graph based on
#' Raghavan, Albert, Kumara. Phys Rev E 76, 036106 (2007)
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
inferCommunitiesLP <- function( unique_edges, async_prop = .5, check_unique = F ){

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

    # Select first batch of nodes to update
    first_batch <- nodes %>% select( node ) %>% sample_frac(async_prop )

    # Propagate votes from first batch
    first_batch_votes <- edges %>%
      right_join( first_batch, by = c( "tgt" = "node" ) ) %>%
      voteForLabelPropagation( nodes ) %>%
      sample_n( 1 ) %>% ungroup()

    # Update nodes
    nodes <- left_join( nodes, first_batch_votes, by = "node" ) %>%
      mutate( label = if_else( is.na( new_label ), label, new_label ) ) %>%
      select( node, label )

    # Get votes from all
    votes <- voteForLabelPropagation( edges, nodes )

    # Check whether we're done
    checks <- votes %>% ungroup() %>%
      left_join( nodes, by = "node" ) %>%
      group_by( node ) %>%
      summarize( concensus = any( label == new_label ) ) %>%
      ungroup() %>%
      summarize( done = all( concensus ) )

    if ( checks$done ) break;

    # Propagate votes from all
    nodes <- votes %>%
      sample_n( 1 ) %>%
      ungroup() %>%
      select( node, label = new_label )
  }

  return ( nodes )
}

#' Helper, implements voting part of label propagation
#'
#' Helper implements voting part of label propagation
#'
#' @param edges edges data frame
#' @param nodes nodes data frame
#' @import dplyr
voteForLabelPropagation <- function( edges, nodes ) {
  left_join( edges, nodes, by = c( "src" = "node" ) ) %>%
    group_by( tgt, label, add = F ) %>%
    summarize( vote = sum( weight ) ) %>%
    group_by( tgt, add = F ) %>%
    filter( vote == max( vote ) ) %>%
    select( node = tgt, new_label = label )
}
