test_that("scale_pose_discrete() passes pose names through", {
  data <- data.frame(x = 1:2, y = 1, pose = c("sitting", "loaf"))
  plot <- ggplot(data, aes(x, y, pose = pose)) +
    geom_cat() +
    scale_pose_discrete(name = "Pose")

  built <- ggplot_build(plot)

  expect_equal(built$plot$scales$get_scales("pose")$name, "Pose")
  expect_equal(built$data[[1]]$pose, c("sitting", "loaf"))
})

test_that("scale_tail_discrete() passes tail names through", {
  data <- data.frame(x = 1:2, y = 1, tail = c("up", "curl"))
  plot <- ggplot(data, aes(x, y, tail = tail)) +
    geom_cat() +
    scale_tail_discrete(name = "Tail")

  built <- ggplot_build(plot)

  expect_equal(built$plot$scales$get_scales("tail")$name, "Tail")
  expect_equal(built$data[[1]]$tail, c("up", "curl"))
})

test_that("the default pose and tail scales are identity scales", {
  data <- data.frame(x = 1, y = 1, pose = "loaf", tail = "curl")
  plot <- ggplot(data, aes(x, y, pose = pose, tail = tail)) + geom_cat()
  scales <- ggplot_build(plot)$plot$scales

  expect_s3_class(scales$get_scales("pose"), "ScaleDiscreteIdentity")
  expect_s3_class(scales$get_scales("tail"), "ScaleDiscreteIdentity")
})
