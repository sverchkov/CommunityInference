#' Initialize community search objects
#'
#' Initialize the communities and membership tables
#' @param similarities see [inferCommunitiesGreedily]
#' @import dplyr
#' @author Yuriy Sverchkov
initCommunitySearch <- function( similarities ) {
  # Compute merge costs for initial nodes, prepare community indeces
  membership <- union( distinct( similarities, member = a ), distinct( similarities, member = b ) ) %>%
    arrange( member )

  if ( convert_ids <- is.integer( membership$member ) )
    membership <- mutate( membership, id = member )
  else
    membership <- mutate( membership, id = row_number() )

  membership <- mutate( membership, community = id )

  # Compute total similarity
  total <- summarize( similarities, total = sum( similarity ) )$total

  flog.trace( "Total similarity of graph is %s.", total )

  # Figure out weighted degree of nodes
  flog.trace( "Computing weighted degree of nodes..." )
  degrees <- union_all(
    similarities %>% group_by( a ) %>% summarize( deg = sum( similarity ) ) %>% ungroup() %>% rename( id = a ),
    similarities %>% group_by( b ) %>% summarize( deg = sum( similarity ) ) %>% ungroup() %>% rename( id = b ) ) %>%
    group_by( id ) %>% summarize( degree = sum( deg ) ) %>% ungroup()
  flog.trace( "Weighted degrees computed.")

  # Convert similarities to community sum change
  flog.trace( "Converting similarities to community score change..." )
  communities <- similarities %>%
    left_join( degrees, by = c("a" = "id") ) %>%
    rename( deg_a = degree ) %>%
    left_join( degrees, by = c("b" = "id") ) %>%
    rename( deg_b = degree ) %>%
    transmute( a, b, cost = similarity - 2 * deg_a * deg_b / total )

  if ( convert_ids )
    communities <- communities %>%
    left_join( select( membership, member, a_id = id ), by = c("a" = "member") ) %>%
    left_join( select( membership, member, b_id = id ), by = c("b" = "member") ) %>%
    select( a = a_id, b = b_id, cost )

  flog.trace( "Community score change computed." )

  return ( list( membership = membership, communities = communities ) )
}
