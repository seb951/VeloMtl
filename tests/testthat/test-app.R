

#fake test data
data_test = list(
  data.frame(day_counts=c(rep(as.Date("2020-01-01","%Y-%m-%d"),10),rep(as.Date("2021-01-01","%Y-%m-%d"),10)),
             station=rep(c(1001,1e+08),10),
             counts = seq(1,20),
             loess_smooth =seq(1,20),
             Nom=rep(c('a','moyenne'),10)),
  data.frame(Nom=letters[1:10],sum=sample(1:10)),
  data.frame(Nom=letters[1:10],sum=sample(1:10),ID = seq(1001,1010))
  )

app_VeloMtl = run_app()


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
  expect_true(all(class(barplotly_statistics(bike_data = data_test[[3]]))==c("plotly","htmlwidget")))
})


test_that("loess_plotly works", {
  expect_true(all(class(loess_plotly(data = data_test,stations="a"))==c("plotly","htmlwidget")))
})


test_that("scatter_stats_plotly works", {
  expect_true(all(class(scatter_stats_plotly(data = data_test,stations="a"))==c("plotly","htmlwidget")))
})


#app_VeloMtl = golem::run_dev();

test_that("app works", {
  expect_true(class(app_VeloMtl)=="shiny.appobj");
  expect_true(is.list(app_VeloMtl));
  expect_true(length(app_VeloMtl)==5)
})



