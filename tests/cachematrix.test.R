test.cachematrix <- function()
{
  # set test conditions and expectations
  test_matrix_size <- 200
  decimal_places_to_compare <- 3
  test_matrix <- matrix(rexp(test_matrix_size ^ 2), test_matrix_size, test_matrix_size)
  solve_benchmarks <- benchmark(expected_inverse_test_matrix <- solve(test_matrix))
  cat(sprintf('\nsolve() computation time: %f\n', solve_benchmarks$elapsed))
  
  # compute inverse using caching
  cached_test_matrix <- makeCacheMatrix(test_matrix)
  cache_solve_benchmarks <- benchmark(inverse_test_matrix <- cacheSolve(cached_test_matrix))
  cat(sprintf('\ncacheSolve() computation time: %f\n', cache_solve_benchmarks$elapsed))
  
  # check if method returns the expected result first
  expect_that(round(inverse_test_matrix, decimal_places_to_compare), equals(round(expected_inverse_test_matrix, decimal_places_to_compare
  )))
  
  # check if benchmarks improved for caching
  expect_less_than(cache_solve_benchmarks$elapsed, solve_benchmarks$elapsed)
}