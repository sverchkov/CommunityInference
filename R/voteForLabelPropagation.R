#' Helper, implements voting part of label propagation
#'
#' Helper implements voting part of label propagation
#'
#' @param edges edges data frame
#' @param nodes nodes data frame
#' @import dplyr
#' @author Yuriy Sverchkov
voteForLabelPropagation <- function( edges, nodes ) {
  left_join( edges, nodes, by = c( "src" = "node" ) ) %>%
    group_by( tgt, label, add = F ) %>%
    summarize( vote = sum( weight ) ) %>%
    group_by( tgt, add = F ) %>%
    filter( vote == max( vote ) ) %>%
    select( node = tgt, new_label = label )
}
