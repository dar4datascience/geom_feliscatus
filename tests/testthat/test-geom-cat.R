test_that("defaults are applied", {
  plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_cat()
  data <- ggplot_build(plot)$data[[1]]

  expect_equal(data$coat, "black")
  expect_equal(data$size, 8)
  expect_equal(data$angle, 0)
})

test_that("the coat can be mapped from data", {
  data <- data.frame(x = 1:5, y = 1, coat = cat_coats())
  plot <- ggplot(data, aes(x, y, coat = coat)) + geom_cat()
  built <- ggplot_build(plot)$data[[1]]

  expect_equal(built$coat, cat_coats())
})

test_that("geom_cat() draws one cat per observation", {
  plot <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_cat()
  grob <- layer_grob(plot)[[1]]

  expect_s3_class(grob, "gTree")
  expect_length(grob$children, 3)
})

test_that("unknown coats fail with a helpful error", {
  plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_cat(coat = "dalmatian")

  expect_snapshot(error = TRUE, ggplotGrob(plot))
})

test_that("eyes and whiskers are layer parameters", {
  plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_cat(eyes = "closed", whiskers = FALSE)

  expect_no_error(ggplotGrob(plot))
})

test_that("fill overrides the base fur colour", {
  grob <- cat_grob(
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    width = unit(10, "mm"),
    height = unit(10, "mm"),
    coat = "black",
    fill = "pink"
  )

  # Draw order: tail rim, tail, two ears, two inner ears, body.
  body <- grob$children[[7]]
  expect_s3_class(body, "polygon")
  expect_equal(body$gp$fill, alpha("pink", 1))
})

test_that("size and angle reach the viewport", {
  grob <- cat_grob(
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    width = unit(12, "mm"),
    height = unit(12, "mm"),
    angle = 45
  )

  expect_equal(grob$vp$width, unit(12, "mm"))
  expect_equal(grob$vp$height, unit(12, "mm"))
  expect_equal(grob$vp$angle, 45)
})

test_that("the legend key draws a cat", {
  key <- draw_key_cat(
    list(coat = "tuxedo", colour = NA, fill = NA, alpha = NA, linewidth = 1),
    list(eyes = "open", whiskers = TRUE, seed = 1),
    size = 5
  )

  expect_s3_class(key, "gTree")
  expect_equal(key$vp$width, unit(0.9, "snpc"))
  expect_equal(attr(key, "width"), 0.5)
  expect_equal(attr(key, "height"), 0.5)
})
