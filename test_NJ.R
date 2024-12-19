library(testthat)
library(ape)

source("NJ.R")

# Test 1: Comparison with ape::nj()
test_that("NJ_iter() produces identical topology as ape::nj", {
  D <- matrix(c(0, 5, 9, 9, 8,
                5, 0, 10, 10, 9,
                9, 10, 0, 8, 7,
                9, 10, 8, 0, 3,
                8, 9, 7, 3, 0), ncol = 5)
  
  custom_tree <- NJ_iter(D)
  
  ape_tree <- ape::nj(as.dist(D))
  
  expect_equal(sort(custom_tree$tip.label), sort(ape_tree$tip.label))
  expect_equal(sort(custom_tree$edge), sort(ape_tree$edge))
  expect_equal(length(custom_tree$edge.length), length(ape_tree$edge.length))
})

# Test 2: Edge case - Minimum size matrix (3 taxa)
test_that("NJ_iter() handles minimum size matrix (3 taxa)", {
  D <- matrix(c(0, 3, 4,
                3, 0, 5,
                4, 5, 0), ncol = 3)
  
  tree <- NJ_iter(D)
  
  expect_equal(length(tree$tip.label), 3)
  expect_equal(tree$Nnode, 1)
  expect_equal(nrow(tree$edge), 3)
  expect_length(tree$edge.length, 3)
})

# Test 3: Large matrix stress test
test_that("NJ_iter() handles larger matrices correctly", {
  set.seed(1)
  D <- as.matrix(dist(matrix(rnorm(100), nrow = 10)))
  
  tree <- NJ_iter(D)
  
  expect_true(inherits(tree, "phylo"))
  expect_equal(length(tree$tip.label), 10)
  expect_equal(tree$Nnode, 8)
  expect_equal(nrow(tree$edge), 17)
  expect_length(tree$edge.length, 17)
})
