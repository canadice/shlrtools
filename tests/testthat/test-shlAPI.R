test_that("Data is loaded", {
  expect_equal(
    is.data.frame(
      readAPI(
        "https://index.simulationhockey.com/api/v1/players/ratings")
      ),
    TRUE
    )
  expect_equal(
    length(
      playerLoader(0)
    ),
    2
  )
})
