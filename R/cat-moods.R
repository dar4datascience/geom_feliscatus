#' Cat moods understood by [geom_cat()]
#'
#' `cat_moods()` lists the small extras [geom_cat()] can add around a cat,
#' from sleeping `"zzz"` to a `"box"` to sit in.  The `mood` layer parameter
#' accepts these names.
#'
#' @return A character vector of mood names.
#' @export
#' @examples
#' cat_moods()
cat_moods <- function() {
  c(
    "none",
    "zzz",
    "heart",
    "sparkle",
    "anger",
    "sweat",
    "music",
    "fish",
    "yarn",
    "pawprints",
    "shadow",
    "box"
  )
}

match_mood <- function(mood, call = caller_env()) {
  key <- tolower(trimws(as.character(mood)))
  unknown <- setdiff(unique(key), cat_moods())
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown cat mood{?s}: {.val {unknown}}.",
        "i" = "Supported moods are {.val {cat_moods()}}."
      ),
      call = call
    )
  }
  key
}

heart_points <- function(cx, cy, size) {
  t <- seq(0, 2 * pi, length.out = 60)
  x <- 16 * sin(t)^3
  y <- 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
  list(x = cx + size * x / 17, y = cy + size * y / 17)
}

star_points <- function(cx, cy, size) {
  outer <- cbind(
    c(0, 0.25, 1, 0.25, 0, -0.25, -1, -0.25) * size,
    c(1, 0.25, 0, -0.25, -1, -0.25, 0, 0.25) * size
  )
  list(x = cx + outer[, 1], y = cy + outer[, 2])
}

paw_grob <- function(cx, cy, size, colour, ctx) {
  c(
    list(ellipse_grob(
      cx,
      cy,
      size,
      size * 0.8,
      gp = ctx$fill_gp(colour)
    )),
    lapply(c(-0.9, 0, 0.9), function(dx) {
      grid::circleGrob(
        x = cx + dx * size * 0.55,
        y = cy + size * 0.95,
        r = size * 0.30,
        gp = ctx$fill_gp(colour)
      )
    })
  )
}

mood_parts <- function(mood, a, ctx, where = c("back", "front")) {
  where <- match.arg(where)
  if (mood == "none") {
    return(list())
  }
  hx <- a$head[1]
  hy <- a$head[2]
  hr <- a$head[3]
  fs <- ctx$stroke * 6

  switch(
    mood,
    shadow = if (where == "back") {
      list(ellipse_grob(
        0.5,
        0.045,
        0.30,
        0.045,
        gp = grid::gpar(
          fill = grDevices::adjustcolor("#000000", alpha.f = 0.18),
          col = NA
        )
      ))
    } else {
      list()
    },
    box = if (where == "back") {
      list(
        grid::rectGrob(
          0.16,
          0.04,
          width = 0.68,
          height = 0.58,
          just = c("left", "bottom"),
          gp = grid::gpar(fill = "#c8a165", col = "#8a6a3f", lwd = ctx$stroke)
        ),
        grid::polygonGrob(
          x = c(0.16, 0.16, 0.02),
          y = c(0.62, 0.52, 0.44),
          gp = grid::gpar(fill = "#d8b47c", col = "#8a6a3f", lwd = ctx$stroke)
        ),
        grid::polygonGrob(
          x = c(0.84, 0.84, 0.98),
          y = c(0.62, 0.52, 0.44),
          gp = grid::gpar(fill = "#d8b47c", col = "#8a6a3f", lwd = ctx$stroke)
        )
      )
    } else {
      list(grid::rectGrob(
        0.16,
        0.04,
        width = 0.68,
        height = 0.26,
        just = c("left", "bottom"),
        gp = grid::gpar(fill = "#b8905a", col = "#8a6a3f", lwd = ctx$stroke)
      ))
    },
    zzz = if (where == "front") {
      list(
        grid::textGrob(
          "z",
          x = 0.78,
          y = 0.80,
          gp = grid::gpar(col = "#5b4bb5", fontsize = fs * 0.8, fontface = "bold")
        ),
        grid::textGrob(
          "z",
          x = 0.85,
          y = 0.88,
          gp = grid::gpar(col = "#5b4bb5", fontsize = fs, fontface = "bold")
        ),
        grid::textGrob(
          "z",
          x = 0.93,
          y = 0.97,
          gp = grid::gpar(col = "#5b4bb5", fontsize = fs * 1.25, fontface = "bold")
        )
      )
    } else {
      list()
    },
    heart = if (where == "front") {
      pts <- heart_points(0.82, 0.80, hr * 0.42)
      list(grid::polygonGrob(pts$x, pts$y, gp = ctx$fill_gp("#e74c3c")))
    } else {
      list()
    },
    sparkle = if (where == "front") {
      lapply(
        list(c(0.16, 0.82, 0.05), c(0.86, 0.68, 0.04), c(0.10, 0.58, 0.035)),
        function(s) {
          pts <- star_points(s[1], s[2], s[3])
          grid::polygonGrob(pts$x, pts$y, gp = ctx$fill_gp("#f1c40f"))
        }
      )
    } else {
      list()
    },
    anger = if (where == "front") {
      list(
        grid::segmentsGrob(
          x0 = c(0.79, 0.79),
          y0 = c(0.76, 0.86),
          x1 = c(0.89, 0.89),
          y1 = c(0.86, 0.76),
          gp = ctx$line_gp("#c0392b", ctx$stroke * 2.5)
        )
      )
    } else {
      list()
    },
    sweat = if (where == "front") {
      list(grid::polygonGrob(
        x = c(0.85, 0.90, 0.885, 0.85, 0.815, 0.80),
        y = c(0.79, 0.70, 0.65, 0.62, 0.65, 0.70),
        gp = ctx$fill_gp("#5dade2")
      ))
    } else {
      list()
    },
    music = if (where == "front") {
      list(
        grid::textGrob(
          "\u266a",
          x = 0.80,
          y = 0.82,
          gp = grid::gpar(col = "#8e44ad", fontsize = fs, fontface = "bold")
        ),
        grid::textGrob(
          "\u266b",
          x = 0.89,
          y = 0.92,
          gp = grid::gpar(col = "#8e44ad", fontsize = fs * 1.1, fontface = "bold")
        )
      )
    } else {
      list()
    },
    fish = if (where == "front") {
      list(
        ellipse_grob(0.17, 0.15, 0.09, 0.045, gp = ctx$fill_gp("#7fb3d5")),
        grid::polygonGrob(
          x = c(0.09, 0.03, 0.03),
          y = c(0.15, 0.21, 0.09),
          gp = ctx$fill_gp("#7fb3d5")
        ),
        grid::circleGrob(
          x = 0.22,
          y = 0.165,
          r = 0.010,
          gp = ctx$fill_gp("#1b2631")
        )
      )
    } else {
      list()
    },
    yarn = if (where == "front") {
      list(
        grid::circleGrob(
          x = 0.86,
          y = 0.12,
          r = 0.08,
          gp = ctx$fill_gp("#e67e22")
        ),
        grid::xsplineGrob(
          x = c(0.81, 0.90, 0.87, 0.83),
          y = c(0.15, 0.16, 0.09, 0.08),
          shape = 1,
          open = TRUE,
          gp = ctx$line_gp("#a04000", ctx$stroke * 0.8)
        )
      )
    } else {
      list()
    },
    pawprints = if (where == "front") {
      c(
        paw_grob(0.14, 0.08, 0.038, "#8a6a3f", ctx),
        paw_grob(0.29, 0.05, 0.032, "#8a6a3f", ctx),
        paw_grob(0.44, 0.08, 0.038, "#8a6a3f", ctx)
      )
    } else {
      list()
    }
  )
}
