with_mock_api({
  test_that("climate forecast query returns correctly", {
    result <- ensemble_models(
      location = c(39, -86),
      hourly = c("temperature_2m", "precipitation"),
      model = "gfs_seamless",
      response_units = list(
        temperature_unit = "fahrenheit",
        precipitation_unit = "inch"
      )
    )

    expect_true(dim(result)[1] == 168)
    expect_true(dim(result)[2] == 63)
  })
  test_that("need to provide hourly variables", {
    expect_error(
      ensemble_models(
        "indianapolis"
      ),
      "hourly measure not supplied"
    )
  })
})

