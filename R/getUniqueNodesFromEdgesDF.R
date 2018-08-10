#' Get unique node array from an edges dataframe
#'
#' Get unique node array from an edges dataframe.
#'
#' @param edges data frame with columns a, b
#' @return a sorted array of the unique elements appearing in columns a, b
getUniqueNodesFromEdgesDF <- function( edges ){
  sort( unique( c( edges$a, edges$b ) ) )
}
