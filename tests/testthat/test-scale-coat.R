test_that("scale_coat_discrete() passes coat names through", {
  data <- data.frame(x = 1:2, y = 1, coat = c("tuxedo", "calico"))
  plot <- ggplot(data, aes(x, y, coat = coat)) +
    geom_cat() +
    scale_coat_discrete(name = "Coat")

  built <- ggplot_build(plot)

  expect_equal(built$plot$scales$get_scales("coat")$name, "Coat")
  expect_equal(built$data[[1]]$coat, c("tuxedo", "calico"))
})

test_that("the default coat scale is an identity scale", {
  data <- data.frame(x = 1:3, y = 1, coat = c("black", "tortoise", "white"))
  plot <- ggplot(data, aes(x, y, coat = coat)) + geom_cat()

  expect_s3_class(
    ggplot_build(plot)$plot$scales$get_scales("coat"),
    "ScaleDiscreteIdentity"
  )
})
