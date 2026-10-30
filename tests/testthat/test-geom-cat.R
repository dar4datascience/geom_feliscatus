test_that("defaults are applied", {
  plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_cat()
  data <- ggplot_build(plot)$data[[1]]

  expect_equal(data$coat, "black")
  expect_equal(data$pose, "sitting")
  expect_equal(data$tail, "up")
  expect_equal(data$size, 8)
  expect_equal(data$angle, 0)
})

test_that("the coat can be mapped from data", {
  data <- data.frame(x = seq_along(cat_coats()), y = 1, coat = cat_coats())
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

  # Parts are named so that adding a part does not break this test.
  body <- grob$children[[which(vapply(
    grob$children,
    function(g) identical(g$name, "cat_body"),
    logical(1)
  ))]]
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

test_that("layers get distinct grob names", {
  data <- data.frame(x = 1:2, y = 1)
  plot <- ggplot(data, aes(x, y)) +
    geom_cat(data = data[1, , drop = FALSE]) +
    geom_cat(data = data[2, , drop = FALSE])

  grob <- ggplotGrob(plot)
  panel <- grob$grobs[[which(grob$layout$name == "panel")]]
  names <- vapply(panel$children, function(x) x$name, character(1))
  names <- names[names != "NULL"]

  expect_equal(anyDuplicated(names), 0L)
})

test_that("pose and tail can be mapped from data", {
  data <- data.frame(x = seq_along(cat_poses()), y = 1, pose = cat_poses())
  built <- ggplot_build(
    ggplot(data, aes(x, y, pose = pose)) + geom_cat()
  )$data[[1]]

  expect_equal(built$pose, cat_poses())
})

test_that("unknown poses and tails fail with a helpful error", {
  expect_error(match_pose("floating"), "Unknown cat pose")
  expect_error(match_tail("propeller"), "Unknown cat tail")
})

test_that("every pose and tail combination draws", {
  for (pose in cat_poses()) {
    for (tail in cat_tails()) {
      expect_no_error(cat_grob(
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        width = unit(12, "mm"),
        height = unit(12, "mm"),
        pose = pose,
        tail = tail
      ))
    }
  }
})

test_that("silhouettes have no face, markings or tail rings", {
  grob <- cat_grob(
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    width = unit(12, "mm"),
    height = unit(12, "mm"),
    coat = "calico",
    silhouette = TRUE
  )
  names <- vapply(grob$children, function(g) g$name, character(1))

  expect_false(any(grepl("eye|muzzle|whisker|ring", names)))
  expect_true(any(names == "cat_body"))
})

test_that("silhouettes fall back to ink so pale coats stay visible", {
  grob <- cat_grob(
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    width = unit(12, "mm"),
    height = unit(12, "mm"),
    coat = "white",
    silhouette = TRUE
  )
  body <- grob$children[[which(vapply(
    grob$children,
    function(g) identical(g$name, "cat_body"),
    logical(1)
  ))]]

  expect_equal(body$gp$fill, "#1F1B18FF")
})

test_that("every accessory and mood draws", {
  for (accessory in cat_accessories()) {
    expect_no_error(cat_grob(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(12, "mm"),
      height = unit(12, "mm"),
      accessory = accessory
    ))
  }
  for (mood in cat_moods()) {
    expect_no_error(cat_grob(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(12, "mm"),
      height = unit(12, "mm"),
      mood = mood
    ))
  }
})

test_that("accessories and moods are validated", {
  expect_error(geom_cat(accessory = "top hat"), "Unknown cat accessor")
  expect_error(geom_cat(mood = "smug"), "Unknown cat mood")
})

test_that("legend keys draw for every pose and tail", {
  for (pose in cat_poses()) {
    for (tail in cat_tails()) {
      key <- draw_key_cat(
        list(
          coat = "tuxedo",
          pose = pose,
          tail = tail,
          colour = NA,
          fill = NA,
          alpha = NA,
          linewidth = 1
        ),
        list(eyes = "open", whiskers = TRUE, seed = 1),
        size = 5
      )
      expect_s3_class(key, "gTree")
    }
  }
})
