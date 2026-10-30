# Visual references for the abstraction levels a cat geom could adopt.
#
#   Rscript dev/abstraction-refs.R
#
# Writes:
#   dev/references/abstraction-styles.png  - all six styles, large
#   dev/references/abstraction-sizes.png   - all six at 8, 12 and 18 mm
#
# The styles are sketches, not production code.  They exist so the package
# identity can be chosen by looking, not by arguing.

library(grid)
devtools::load_all(quiet = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

ink <- "#2b2b2b"
fur <- "#e8934a"
fur_dark <- "#c9722f"
fur_light <- "#f2b880"
cream <- "#f7efe2"
pink <- "#e08a9b"
paper <- "#faf8f5"

poly <- function(x, y, fill = NA, col = NA, lwd = 0) {
  grid::polygonGrob(
    x = x,
    y = y,
    gp = grid::gpar(fill = fill, col = col, lwd = lwd, linejoin = "round")
  )
}

flatten_grobs <- function(x) {
  if (inherits(x, "grob")) {
    return(list(x))
  }
  if (is.list(x)) {
    return(unlist(lapply(x, flatten_grobs), recursive = FALSE))
  }
  list()
}

gl <- function(...) do.call(grid::gList, flatten_grobs(list(...)))

# 1. Emoji-flat -------------------------------------------------------------
# Solid fills, no outline, oversized head, minimal detail.  The style that
# survives the smallest size.
style_emoji <- function() {
  g <- gl(
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = fur, lwd = 14, lineend = "round")
    ),
    ellipse_grob(0.5, 0.36, 0.26, 0.28, gp = grid::gpar(fill = fur, col = NA)),
    ellipse_grob(
      0.37,
      0.105,
      0.10,
      0.048,
      gp = grid::gpar(fill = fur_light, col = NA)
    ),
    ellipse_grob(
      0.63,
      0.105,
      0.10,
      0.048,
      gp = grid::gpar(fill = fur_light, col = NA)
    ),
    poly(c(0.345, 0.330, 0.465), c(0.830, 0.980, 0.875), fill = fur),
    poly(c(0.655, 0.670, 0.535), c(0.830, 0.980, 0.875), fill = fur),
    grid::circleGrob(0.5, 0.72, 0.19, gp = grid::gpar(fill = fur, col = NA)),
    poly(c(0.375, 0.362, 0.442), c(0.848, 0.940, 0.884), fill = pink),
    poly(c(0.625, 0.638, 0.558), c(0.848, 0.940, 0.884), fill = pink),
    ellipse_grob(
      0.455,
      0.655,
      0.058,
      0.045,
      gp = grid::gpar(fill = cream, col = NA)
    ),
    ellipse_grob(
      0.545,
      0.655,
      0.058,
      0.045,
      gp = grid::gpar(fill = cream, col = NA)
    ),
    ellipse_grob(
      0.435,
      0.748,
      0.038,
      0.044,
      gp = grid::gpar(fill = ink, col = NA)
    ),
    ellipse_grob(
      0.565,
      0.748,
      0.038,
      0.044,
      gp = grid::gpar(fill = ink, col = NA)
    ),
    grid::circleGrob(
      0.423,
      0.762,
      0.008,
      gp = grid::gpar(fill = "#ffffff", col = NA)
    ),
    grid::circleGrob(
      0.553,
      0.762,
      0.008,
      gp = grid::gpar(fill = "#ffffff", col = NA)
    ),
    poly(c(0.478, 0.522, 0.5), c(0.680, 0.680, 0.658), fill = pink),
    grid::xsplineGrob(
      c(0.5, 0.472, 0.452),
      c(0.658, 0.630, 0.645),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = ink, lwd = 1.4, lineend = "round")
    ),
    grid::xsplineGrob(
      c(0.5, 0.528, 0.548),
      c(0.658, 0.630, 0.645),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = ink, lwd = 1.4, lineend = "round")
    ),
    grid::segmentsGrob(
      x0 = c(0.40, 0.40, 0.60, 0.60),
      y0 = c(0.668, 0.640, 0.668, 0.640),
      x1 = c(0.24, 0.22, 0.76, 0.78),
      y1 = c(0.694, 0.648, 0.694, 0.648),
      gp = grid::gpar(col = "#c9a27e", lwd = 1.6, lineend = "round")
    )
  )
  grid::gTree(children = g)
}

# 2. Line-art ---------------------------------------------------------------
# One stroke weight, no fills except the eyes.  Reads like a pen drawing.
style_line <- function() {
  stroke <- grid::gpar(
    col = ink,
    fill = NA,
    lwd = 2.4,
    linejoin = "round",
    lineend = "round"
  )
  g <- gl(
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = stroke
    ),
    ellipse_grob(0.5, 0.36, 0.26, 0.28, gp = stroke),
    ellipse_grob(0.37, 0.105, 0.10, 0.048, gp = stroke),
    ellipse_grob(0.63, 0.105, 0.10, 0.048, gp = stroke),
    poly(c(0.345, 0.330, 0.465), c(0.830, 0.980, 0.875), col = ink, lwd = 2.4),
    poly(c(0.655, 0.670, 0.535), c(0.830, 0.980, 0.875), col = ink, lwd = 2.4),
    grid::circleGrob(0.5, 0.72, 0.19, gp = stroke),
    poly(c(0.383, 0.372, 0.438), c(0.856, 0.928, 0.886), col = ink, lwd = 1.7),
    poly(c(0.617, 0.628, 0.562), c(0.856, 0.928, 0.886), col = ink, lwd = 1.7),
    ellipse_grob(0.462, 0.652, 0.050, 0.040, gp = stroke),
    ellipse_grob(0.538, 0.652, 0.050, 0.040, gp = stroke),
    ellipse_grob(0.435, 0.748, 0.034, 0.040, gp = stroke),
    ellipse_grob(0.565, 0.748, 0.034, 0.040, gp = stroke),
    ellipse_grob(
      0.435,
      0.748,
      0.012,
      0.028,
      gp = grid::gpar(fill = ink, col = NA)
    ),
    ellipse_grob(
      0.565,
      0.748,
      0.012,
      0.028,
      gp = grid::gpar(fill = ink, col = NA)
    ),
    grid::circleGrob(
      0.427,
      0.758,
      0.005,
      gp = grid::gpar(fill = "#ffffff", col = NA)
    ),
    grid::circleGrob(
      0.557,
      0.758,
      0.005,
      gp = grid::gpar(fill = "#ffffff", col = NA)
    ),
    poly(c(0.478, 0.522, 0.5), c(0.680, 0.680, 0.658), col = ink, lwd = 1.8),
    grid::xsplineGrob(
      c(0.5, 0.472, 0.452),
      c(0.658, 0.630, 0.645),
      shape = 1,
      open = TRUE,
      gp = stroke
    ),
    grid::xsplineGrob(
      c(0.5, 0.528, 0.548),
      c(0.658, 0.630, 0.645),
      shape = 1,
      open = TRUE,
      gp = stroke
    ),
    grid::segmentsGrob(
      x0 = c(0.40, 0.40, 0.40, 0.60, 0.60, 0.60),
      y0 = c(0.668, 0.652, 0.636, 0.668, 0.652, 0.636),
      x1 = c(0.22, 0.20, 0.22, 0.78, 0.80, 0.78),
      y1 = c(0.700, 0.652, 0.604, 0.700, 0.652, 0.604),
      gp = grid::gpar(col = ink, lwd = 1.6, lineend = "round")
    ),
    # chest tufts
    grid::segmentsGrob(
      x0 = c(0.44, 0.50, 0.56),
      y0 = c(0.585, 0.575, 0.585),
      x1 = c(0.42, 0.50, 0.58),
      y1 = c(0.520, 0.500, 0.520),
      gp = grid::gpar(col = ink, lwd = 1.4, lineend = "round")
    )
  )
  grid::gTree(children = g)
}

# 3. Geometric / low-poly ---------------------------------------------------
# Every form is a flat-shaded facet.  No curves anywhere.
style_geo <- function() {
  facet_disc <- function(cx, cy, rx, ry, n, cols) {
    th <- seq(0, 2 * pi, length.out = n + 1)
    lapply(seq_len(n), function(i) {
      poly(
        c(cx, cx + rx * cos(th[i]), cx + rx * cos(th[i + 1])),
        c(cy, cy + ry * sin(th[i]), cy + ry * sin(th[i + 1])),
        fill = cols[(i - 1) %% length(cols) + 1],
        col = ink,
        lwd = 0.9
      )
    })
  }

  strip <- function(pts, w, cols) {
    n <- nrow(pts)
    normals <- lapply(seq_len(n), function(i) {
      d <- if (i == 1) {
        pts[2, ] - pts[1, ]
      } else if (i == n) {
        pts[n, ] - pts[n - 1, ]
      } else {
        pts[i + 1, ] - pts[i - 1, ]
      }
      d / sqrt(sum(d^2))
    })
    normals <- lapply(normals, function(d) c(-d[2], d[1]))
    left <- t(vapply(
      seq_len(n),
      function(i) pts[i, ] + normals[[i]] * w,
      numeric(2)
    ))
    right <- t(vapply(
      seq_len(n),
      function(i) pts[i, ] - normals[[i]] * w,
      numeric(2)
    ))
    outline <- poly(
      c(left[, 1], rev(right[, 1])),
      c(left[, 2], rev(right[, 2])),
      fill = cols[1],
      col = ink,
      lwd = 0.9
    )
    facets <- lapply(seq_len(n - 1), function(i) {
      poly(
        c(left[i, 1], left[i + 1, 1], right[i + 1, 1], right[i, 1]),
        c(left[i, 2], left[i + 1, 2], right[i + 1, 2], right[i, 2]),
        fill = if (i %% 2) cols[2] else cols[1],
        col = ink,
        lwd = 0.9
      )
    })
    c(list(outline), facets)
  }

  tones <- c(
    fur,
    mix_colour(fur, fur_light, 0.35),
    mix_colour(fur, fur_dark, 0.25)
  )

  tail_pts <- cbind(
    c(0.68, 0.84, 0.96, 0.95, 0.88),
    c(0.13, 0.08, 0.19, 0.40, 0.50)
  )

  g <- gl(
    strip(tail_pts, 0.055, c(fur, fur_dark)),
    facet_disc(0.5, 0.36, 0.26, 0.28, 6, tones),
    poly(
      c(0.29, 0.45, 0.43, 0.31),
      c(0.075, 0.075, 0.115, 0.115),
      fill = fur_light,
      col = ink,
      lwd = 0.9
    ),
    poly(
      c(0.55, 0.71, 0.69, 0.57),
      c(0.075, 0.075, 0.115, 0.115),
      fill = fur_light,
      col = ink,
      lwd = 0.9
    ),
    poly(
      c(0.345, 0.330, 0.465),
      c(0.830, 0.980, 0.875),
      fill = fur_dark,
      col = ink,
      lwd = 0.9
    ),
    poly(
      c(0.655, 0.670, 0.535),
      c(0.830, 0.980, 0.875),
      fill = fur_dark,
      col = ink,
      lwd = 0.9
    ),
    facet_disc(0.5, 0.72, 0.19, 0.19, 8, tones),
    poly(
      c(0.378, 0.366, 0.440),
      c(0.852, 0.930, 0.886),
      fill = pink,
      col = ink,
      lwd = 0.8
    ),
    poly(
      c(0.622, 0.634, 0.560),
      c(0.852, 0.930, 0.886),
      fill = pink,
      col = ink,
      lwd = 0.8
    ),
    poly(
      c(0.44, 0.50, 0.50, 0.44),
      c(0.690, 0.690, 0.645, 0.655),
      fill = cream,
      col = ink,
      lwd = 0.8
    ),
    poly(
      c(0.56, 0.50, 0.50, 0.56),
      c(0.690, 0.690, 0.645, 0.655),
      fill = cream,
      col = ink,
      lwd = 0.8
    ),
    poly(
      c(0.435, 0.465, 0.435, 0.405),
      c(0.775, 0.748, 0.721, 0.748),
      fill = ink
    ),
    poly(
      c(0.565, 0.595, 0.565, 0.535),
      c(0.775, 0.748, 0.721, 0.748),
      fill = ink
    ),
    poly(
      c(0.478, 0.522, 0.5),
      c(0.700, 0.700, 0.676),
      fill = pink,
      col = ink,
      lwd = 0.8
    ),
    grid::segmentsGrob(
      x0 = c(0.40, 0.40, 0.60, 0.60),
      y0 = c(0.668, 0.640, 0.668, 0.640),
      x1 = c(0.23, 0.21, 0.77, 0.79),
      y1 = c(0.700, 0.648, 0.700, 0.648),
      gp = grid::gpar(col = ink, lwd = 1.2, lineend = "round")
    )
  )
  grid::gTree(children = g)
}

# 4. Pure silhouette --------------------------------------------------------
# One shape, zero internal detail.  Recognition comes from the ears and tail.
style_silhouette <- function() {
  solid <- "#1f1b18"
  fill_gp <- grid::gpar(
    fill = solid,
    col = solid,
    lwd = 0,
    linejoin = "round",
    lineend = "round"
  )
  g <- gl(
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = solid, lwd = 17, lineend = "round")
    ),
    ellipse_grob(0.5, 0.36, 0.26, 0.28, gp = fill_gp),
    ellipse_grob(0.37, 0.105, 0.10, 0.048, gp = fill_gp),
    ellipse_grob(0.63, 0.105, 0.10, 0.048, gp = fill_gp),
    poly(
      c(0.345, 0.330, 0.465),
      c(0.830, 0.980, 0.875),
      fill = solid,
      col = solid,
      lwd = 0
    ),
    poly(
      c(0.655, 0.670, 0.535),
      c(0.830, 0.980, 0.875),
      fill = solid,
      col = solid,
      lwd = 0
    ),
    grid::circleGrob(0.5, 0.72, 0.19, gp = fill_gp)
  )
  grid::gTree(children = g)
}

# 5. Sticker ----------------------------------------------------------------
# A die-cut white border, then a flat cat with a crisp ink outline.
style_sticker <- function() {
  halo <- grid::gpar(
    col = "#ffffff",
    fill = "#ffffff",
    lwd = 14,
    linejoin = "round",
    lineend = "round"
  )
  edge <- grid::gpar(col = ink, fill = fur, lwd = 2.4, linejoin = "round")
  g <- gl(
    # halo
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = halo
    ),
    ellipse_grob(0.5, 0.36, 0.26, 0.28, gp = halo),
    ellipse_grob(0.37, 0.105, 0.10, 0.048, gp = halo),
    ellipse_grob(0.63, 0.105, 0.10, 0.048, gp = halo),
    poly(
      c(0.345, 0.330, 0.465),
      c(0.830, 0.980, 0.875),
      fill = "#ffffff",
      col = "#ffffff",
      lwd = 14
    ),
    poly(
      c(0.655, 0.670, 0.535),
      c(0.830, 0.980, 0.875),
      fill = "#ffffff",
      col = "#ffffff",
      lwd = 14
    ),
    grid::circleGrob(0.5, 0.72, 0.19, gp = halo),
    # cat
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = "#ffffff", lwd = 20, lineend = "round")
    ),
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = ink, lwd = 15, lineend = "round")
    ),
    grid::xsplineGrob(
      c(0.70, 0.86, 0.97, 0.94),
      c(0.14, 0.10, 0.26, 0.48),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = fur, lwd = 10, lineend = "round")
    ),
    ellipse_grob(0.5, 0.36, 0.26, 0.28, gp = edge),
    ellipse_grob(
      0.37,
      0.105,
      0.10,
      0.048,
      gp = grid::gpar(col = ink, fill = cream, lwd = 2.4)
    ),
    ellipse_grob(
      0.63,
      0.105,
      0.10,
      0.048,
      gp = grid::gpar(col = ink, fill = cream, lwd = 2.4)
    ),
    poly(
      c(0.345, 0.330, 0.465),
      c(0.830, 0.980, 0.875),
      fill = fur,
      col = ink,
      lwd = 2.4
    ),
    poly(
      c(0.655, 0.670, 0.535),
      c(0.830, 0.980, 0.875),
      fill = fur,
      col = ink,
      lwd = 2.4
    ),
    grid::circleGrob(0.5, 0.72, 0.19, gp = edge),
    poly(
      c(0.375, 0.362, 0.442),
      c(0.848, 0.940, 0.884),
      fill = pink,
      col = ink,
      lwd = 1.2
    ),
    poly(
      c(0.625, 0.638, 0.558),
      c(0.848, 0.940, 0.884),
      fill = pink,
      col = ink,
      lwd = 1.2
    ),
    # forehead stripes
    grid::xsplineGrob(
      c(0.455, 0.470, 0.475),
      c(0.845, 0.855, 0.830),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = fur_dark, lwd = 3, lineend = "round")
    ),
    grid::xsplineGrob(
      c(0.545, 0.530, 0.525),
      c(0.845, 0.855, 0.830),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = fur_dark, lwd = 3, lineend = "round")
    ),
    ellipse_grob(
      0.455,
      0.655,
      0.058,
      0.045,
      gp = grid::gpar(col = ink, fill = cream, lwd = 2.4)
    ),
    ellipse_grob(
      0.545,
      0.655,
      0.058,
      0.045,
      gp = grid::gpar(col = ink, fill = cream, lwd = 2.4)
    ),
    ellipse_grob(
      0.435,
      0.748,
      0.038,
      0.044,
      gp = grid::gpar(fill = "#ffffff", col = ink, lwd = 2.2)
    ),
    ellipse_grob(
      0.565,
      0.748,
      0.038,
      0.044,
      gp = grid::gpar(fill = "#ffffff", col = ink, lwd = 2.2)
    ),
    ellipse_grob(
      0.435,
      0.748,
      0.013,
      0.034,
      gp = grid::gpar(fill = ink, col = NA)
    ),
    ellipse_grob(
      0.565,
      0.748,
      0.013,
      0.034,
      gp = grid::gpar(fill = ink, col = NA)
    ),
    poly(
      c(0.478, 0.522, 0.5),
      c(0.680, 0.680, 0.658),
      fill = pink,
      col = ink,
      lwd = 1.6
    ),
    grid::xsplineGrob(
      c(0.5, 0.472, 0.452),
      c(0.658, 0.630, 0.645),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = ink, lwd = 2, lineend = "round")
    ),
    grid::xsplineGrob(
      c(0.5, 0.528, 0.548),
      c(0.658, 0.630, 0.645),
      shape = 1,
      open = TRUE,
      gp = grid::gpar(col = ink, lwd = 2, lineend = "round")
    ),
    grid::segmentsGrob(
      x0 = c(0.40, 0.40, 0.40, 0.60, 0.60, 0.60),
      y0 = c(0.668, 0.652, 0.636, 0.668, 0.652, 0.636),
      x1 = c(0.22, 0.20, 0.22, 0.78, 0.80, 0.78),
      y1 = c(0.700, 0.652, 0.604, 0.700, 0.652, 0.604),
      gp = grid::gpar(col = ink, lwd = 2.2, lineend = "round")
    )
  )
  grid::gTree(children = g)
}

# 6. Pixel art --------------------------------------------------------------
# A 20 x 19 sprite.  Legible, unmistakably digital, and cheap to draw.
style_pixel <- function() {
  sprite <- c(
    "....................",
    "....##......##......",
    "...#OO#....#OO#.....",
    "...#OPO#..#OPO#.....",
    "...#OOOO##OOOO#.....",
    "..#OOOOOOOOOOOO#....",
    "..#OOOOOOOOOOOO#....",
    "..#O##OOOOOO##O#....",
    "..#OKK#OOOO#KKO#....",
    "..#OKK#OOOO#KKO#....",
    "..#OOOOOPPOOOOO#....",
    "..#OOOOWWWWOOOO#....",
    "...#OOOOOOOOOO#.....",
    "...#OOOOOOOOOO#.....",
    "...#OOOOOOOOOO###...",
    "...#OOOOOOOOOO#OOO#.",
    "....#OO#OO#OO##OOO#.",
    "....###....###OOO#..",
    "..............####.."
  )
  keys <- c(
    "." = NA,
    "#" = "#3b2a20",
    "O" = fur,
    "W" = cream,
    "P" = pink,
    "K" = ink
  )
  n <- length(sprite)
  cols <- nchar(sprite[1])
  cells <- do.call(
    rbind,
    lapply(seq_len(n), function(r) {
      chars <- strsplit(sprite[r], "")[[1]]
      data.frame(
        x = seq_len(cols),
        y = n - r + 1,
        fill = unname(keys[chars]),
        stringsAsFactors = FALSE
      )
    })
  )
  cells <- cells[!is.na(cells$fill), ]
  grobs <- lapply(seq_len(nrow(cells)), function(i) {
    grid::rectGrob(
      x = (cells$x[i] - 0.5) / cols,
      y = (cells$y[i] - 0.5) / n,
      width = 1.02 / cols,
      height = 1.02 / n,
      gp = grid::gpar(fill = cells$fill[i], col = NA)
    )
  })
  grid::gTree(children = do.call(grid::gList, grobs))
}

styles <- list(
  "emoji-flat" = list(fn = style_emoji, note = "solid fills, no outline"),
  "line-art" = list(fn = style_line, note = "one stroke, no fills"),
  "geometric / low-poly" = list(fn = style_geo, note = "flat-shaded facets"),
  "pure silhouette" = list(
    fn = style_silhouette,
    note = "one shape, zero detail"
  ),
  "sticker" = list(fn = style_sticker, note = "die-cut white border"),
  "pixel art" = list(fn = style_pixel, note = "20 x 19 sprite")
)

draw_cell <- function(fn, size = NULL) {
  if (is.null(size)) {
    pushViewport(viewport(
      width = unit(0.74, "snpc"),
      height = unit(0.74, "snpc")
    ))
  } else {
    pushViewport(viewport(width = unit(size, "mm"), height = unit(size, "mm")))
  }
  grid.draw(fn())
  popViewport()
}

# Contact sheet -------------------------------------------------------------
styles_sheet <- function(file) {
  png(file, width = 11, height = 8.4, units = "in", res = 220, bg = "white")
  on.exit(dev.off())
  grid.newpage()
  pushViewport(viewport(
    layout = grid.layout(3, 3, heights = unit(c(0.14, 1, 1), "null"))
  ))

  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:3))
  grid.text(
    "Abstraction levels for a cat geom",
    y = 0.62,
    gp = gpar(fontsize = 22, fontface = "bold", col = "#1f1b18")
  )
  grid.text(
    "The same sitting cat, drawn six ways.  Pick one as the package identity.",
    y = 0.22,
    gp = gpar(fontsize = 11, col = "#6b6259")
  )
  popViewport()

  for (i in seq_along(styles)) {
    r <- if (i <= 3) 2 else 3
    c <- (i - 1) %% 3 + 1
    pushViewport(viewport(layout.pos.row = r, layout.pos.col = c))
    grid.rect(gp = gpar(fill = paper, col = "#ece7e0", lwd = 1))
    draw_cell(styles[[i]]$fn)
    grid.text(
      names(styles)[i],
      y = 0.115,
      gp = gpar(fontsize = 13, fontface = "bold", col = "#1f1b18")
    )
    grid.text(
      styles[[i]]$note,
      y = 0.055,
      gp = gpar(fontsize = 9.5, col = "#6b6259")
    )
    popViewport()
  }
  popViewport()
}

# Legibility sheet ----------------------------------------------------------
sizes_sheet <- function(file) {
  sizes <- c(8, 12, 18)
  short <- c(
    "emoji-flat",
    "line-art",
    "low-poly",
    "silhouette",
    "sticker",
    "pixel art"
  )
  png(file, width = 9, height = 9.4, units = "in", res = 220, bg = "white")
  on.exit(dev.off())
  grid.newpage()
  pushViewport(viewport(
    layout = grid.layout(
      9,
      4,
      widths = unit(c(0.85, 1, 1, 1), "null"),
      heights = unit(c(1, 0.55, rep(1, 6), 0.18), "null")
    )
  ))

  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2:4))
  grid.text(
    "Legibility at plot sizes",
    y = 0.62,
    gp = gpar(fontsize = 18, fontface = "bold", col = "#1f1b18")
  )
  grid.text(
    "Cats drawn at 8, 12 and 18 mm, the range a real plot uses.",
    y = 0.2,
    gp = gpar(fontsize = 10, col = "#6b6259")
  )
  popViewport()

  for (j in seq_along(sizes)) {
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = j + 1))
    grid.text(
      paste0(sizes[j], " mm"),
      gp = gpar(fontsize = 11, fontface = "bold", col = "#6b6259")
    )
    popViewport()
  }

  for (i in seq_along(styles)) {
    r <- i + 2
    pushViewport(viewport(layout.pos.row = r, layout.pos.col = 1))
    grid.text(
      short[i],
      x = 0.95,
      just = "right",
      gp = gpar(fontsize = 10, fontface = "bold", col = "#1f1b18")
    )
    popViewport()
    for (j in seq_along(sizes)) {
      pushViewport(viewport(layout.pos.row = r, layout.pos.col = j + 1))
      grid.rect(gp = gpar(fill = paper, col = "#ece7e0", lwd = 0.8))
      draw_cell(styles[[i]]$fn, size = sizes[j])
      popViewport()
    }
  }
  popViewport()
}

dir.create("dev/references", showWarnings = FALSE)
styles_sheet("dev/references/abstraction-styles.png")
sizes_sheet("dev/references/abstraction-sizes.png")
message(
  "wrote dev/references/abstraction-styles.png and dev/references/abstraction-sizes.png"
)
