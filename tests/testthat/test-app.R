

#fake test data

app_VeloMtl = run_app()
data_test = dummy_data()

#
test_that("dummy true", {
  expect_equal(2 * 2, 4)
})

test_that("dummy false", {
  expect_false((2 * 3)==4)
})


test_that("default_colors(default=T) works", {
  expect_equal(length(default_colors(default = T)), 55)
})


test_that("default_colors(default=F) works", {
  expect_equal(length(default_colors(default = F)), 55)
})


test_that("barplotly_statistics works", {
  expect_true(all(class(barplotly_statistics(data = data_test))==c("plotly","htmlwidget")))
})


test_that("loess_plotly works", {
  expect_true(all(class(loess_plotly(data = data_test,stations="Boyer / Everett"))==c("plotly","htmlwidget")))
})


test_that("scatter_stats_plotly works", {
  expect_true(all(class(scatter_stats_plotly(data = data_test,stations="Boyer / Everett"))==c("plotly","htmlwidget")))
})


#app_VeloMtl = golem::run_dev();

test_that("app works", {
  expect_true(class(app_VeloMtl)=="shiny.appobj");
  expect_true(is.list(app_VeloMtl));
  expect_true(length(app_VeloMtl)==5)
})



