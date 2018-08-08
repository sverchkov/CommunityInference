context("Testing Asynchronous Label Propagation")

zkc <- read.csv( system.file("testdata", "zachary-karate-club.csv", package = "CommunityInference") )

test_that("ZKC example gives cinsistent result", {
  set.seed(1987)
  zkc_lp <- inferCommunitiesASLP( zkc )

  expect_equal( length( unique( zkc_lp$label ) ), 3 )
  expect_equal( length( unique( zkc_lp$label[ zkc_lp$node %in% c(1, 34, 17) ])), 3 )
})

