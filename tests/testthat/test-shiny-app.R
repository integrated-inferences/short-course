library(testthat)

test_that("app loads and key labels exist", {
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = "shiny",
    name = "app-load",
    seed = 123,
    load_timeout = 60000
  )

  html <- app$get_html()
  expect_true(grepl("Query model", html, fixed = TRUE))
  expect_true(grepl("CausalQueries", html, fixed = TRUE))
  expect_true(grepl("Integrated Inferences framework", html, fixed = TRUE))

  app$stop()
})

test_that("default query uses greater-than", {
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = "shiny",
    name = "default-query",
    seed = 456,
    load_timeout = 60000
  )

  app$click("create_model")
  app$set_inputs(main_tabs = "Query model")
  default_query <- app$get_value("query_text_1")

  expect_true(grepl(">", default_query, fixed = TRUE))
  expect_false(grepl("==", default_query, fixed = TRUE))

  app$stop()
})
