#' Community detection, greedy algorithm
#'
#' A naive implementation of community detection in a fully-connected weighted graph based on
#' Newman ME, Girvan M. Finding and evaluating community structure in networks.
#' Physical review E. 2004 Feb 26;69(2):026113.
#'
#' @param similarities a data frame with columns a, b, similarity representing the similarities between nodes.
#' We assume undirected graph and b < a.
#' @return a data frame with two columns, n - node id (taken from a, b of input) and cluster - unique cluster ID.
#' @import dplyr
#' @import futile.logger
#' @author Yuriy Sverchkov
#' @export
inferCommunitiesGreedily <- function( similarities, save.file = NULL, full_trajectory = F, simplify = T, loop_limit = Inf ){

  # Modularity = sum_c (e_cc - a_c^2)
  # where c - communities
  #       e_xy - proportion of edges from community x to community y
  #       a_c - proportion of edges going into community c total

  # When two communities (x and y) are merged, the change in sum is:
  # (e_xx + e_xy + e_yy - (a_x + a_y)^2) - (e_xx - a_x^2) - (e_yy - a_y^2) =
  # e_xy - 2 a_x a_y
  # The prospective cost of merging z to the newly merged group is
  # e_zx + e_zy - 2 a_z ( a_x + a_y )
  # which is the sum of the costs of merging to a_x and a_y

  # Proposed algorithm:
  # compute merge costs for initial nodes
  # merge best
  # update merge costs table
  # repeat
  # either stop at first peak or report all peaks

  # IMPLEMENTATION

  my <- initCommunitySearch( similarities )
  next_id <- max( my$membership$id ) + 1

  # For full trajectory
  peak_number <- 1
  prev_m_cost <- 0

  # The greedy iteration part
  while ( nrow( my$communities ) > 0 ) {
    merge_row <- filter( my$communities, cost == max( cost ) )

    if ( 1 != ( n_merge <- nrow( merge_row ) ) ){
      flog.error( "The merge candidate is %s rows :/", merge_row )
      if ( n_merge < 1 ) stop()
      if ( n_merge > 1 ) merge_row = sample_n( merge_row, 1 )
    }

    if ( 0 > ( m_cost <- merge_row$cost ) && 0 <= prev_m_cost ){
      if ( full_trajectory ) {
        my$membership[[paste("Peak", peak_number)]] <- my$membership$community
        peak_number <- 1 + peak_number
      } else {
        flog.trace("Max merge cost is %s, we're done.", m_cost )
        break;
      }
    }
    prev_m_cost <- m_cost

    # We're going to call the two communities we're merging yin and yang as a shorthand
    yin <- merge_row$a
    yang <- merge_row$b

    flog.trace( "Merging %s with %s...", yin, yang )

    ## Compute costs of merging with new community
    right_communities <- my$communities %>%
      filter( (a == yin) || (a == yang),
              b != yin, b != yang ) %>%
      rename( src = b, tgt = a )
    left_communities <- my$communities %>%
      filter( (b == yin) || (b == yang),
              a != yin, a != yang ) %>%
      rename( src = a, tgt = b )

    new_community_costs <- union_all( right_communities, left_communities ) %>%
      group_by( src ) %>%
      summarize( new_cost = sum( cost ) ) %>%
      transmute( a = src, b = next_id, cost = new_cost )

    # Add to community table
    my$communities <- union_all(
      filter( my$communities, a != yin, a != yang, b != yin, b != yang ),
      new_community_costs )

    flog.trace("...merged!")

    # Update membership table
    my$membership <- mutate( my$membership,
                             community = if_else(
                               (community == yin) | (community == yang),
                               as.integer( next_id ), community ) )

    # Update next id
    next_id <- 1 + next_id

    if( !is.null( save.file ) )
      saveRDS( my$communities, save.file )

    iteration <-
      if ( !exists( "iteration" ) ) 1
    else 1 + iteration
    if ( iteration >= loop_limit ) break;
  }

  if ( simplify )
    return ( my$membership )
  else
    return ( my )
}
