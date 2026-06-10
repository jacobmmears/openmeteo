with_mock_api({
  test_that("air quality query returns correctly", {
    result <- air_quality("beijing",
      "2023-01-01", "2023-01-02",
      hourly = "carbon_monoxide"
    )

    expect_true(dim(result)[1] >= 30)
    expect_true(dim(result)[2] == 2)
  })
  test_that("current air quality returns single-row tibble", {
    result <- air_quality(c(52.52, 13.41), current = c("pm10", "us_aqi"))
    expect_true(tibble::is_tibble(result))
    expect_equal(nrow(result), 1)
    expect_true(all(c("datetime", "current_pm10", "current_us_aqi") %in% names(result)))
  })
  test_that("need to provide hourly or current variables", {
    expect_error(
      air_quality(
        c(-45.46427, -9.18951),
        "2030-01-01",
        "2030-01-03"
      ),
      "at least one of `hourly` or `current`"
    )
  })
  test_that("hourly/current must be character", {
    expect_error(
      air_quality("Beijing", hourly = 123),
      "`hourly` must be a character"
    )
    expect_error(
      air_quality("Beijing", current = TRUE),
      "`current` must be a character"
    )
  })
})
