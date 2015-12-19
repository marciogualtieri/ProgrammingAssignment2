test.cachematrix <- function()
{
  given_the_test_setup()
  when_i_compute_the_inverse_matrix_using_caching()
  then_i_get_the_correct_inverse_matrix()
  then_caching_is_faster() 
}

given_the_test_setup <- function() {
  TEST_MATRIX_SIZE <<- 200
  DECIMAL_PLACES_TO_COMPARE <<- 3
  TEST_MATRIX <<- matrix(rexp(TEST_MATRIX_SIZE ^ 2), TEST_MATRIX_SIZE, TEST_MATRIX_SIZE)
  SOLVE_BENCHMARKS <<- benchmark(EXPECTED_INVERSE_TEST_MATRIX <<- solve(TEST_MATRIX))
  cat(sprintf('\nsolve() computation time: %f\n', SOLVE_BENCHMARKS$elapsed))  
}

when_i_compute_the_inverse_matrix_using_caching <- function() {
  CACHED_TEST_MATRIX <<- makeCacheMatrix(TEST_MATRIX)
  CACHE_SOLVE_BENCHMARKS <<- benchmark(INVERSE_TEST_MATRIX <<- cacheSolve(CACHED_TEST_MATRIX))
  cat(sprintf('\ncacheSolve() computation time: %f\n', CACHE_SOLVE_BENCHMARKS$elapsed))
}

then_i_get_the_correct_inverse_matrix <- function() {
  expect_that(round(INVERSE_TEST_MATRIX, DECIMAL_PLACES_TO_COMPARE),
              equals(round(EXPECTED_INVERSE_TEST_MATRIX, DECIMAL_PLACES_TO_COMPARE)))
}

then_caching_is_faster <- function() {
  expect_less_than(CACHE_SOLVE_BENCHMARKS$elapsed,
                   SOLVE_BENCHMARKS$elapsed)
}