# Colour helpers -----------------------------------------------------------

mix_colour <- function(a, b, amount = 0.5) {
  a <- grDevices::col2rgb(a)
  b <- grDevices::col2rgb(b)
  out <- a * (1 - amount) + b * amount
  grDevices::rgb(out[1, ], out[2, ], out[3, ], maxColorValue = 255)
}

luminance <- function(colour) {
  rgb <- grDevices::col2rgb(colour) / 255
  sum(c(0.2126, 0.7152, 0.0722) * rgb[, 1])
}

# A rim that reads against both light and dark fur.
contrast_colour <- function(colour) {
  if (luminance(colour) < 0.5) {
    mix_colour(colour, "#ffffff", 0.35)
  } else {
    mix_colour(colour, "#000000", 0.35)
  }
}

# Geometry helpers ---------------------------------------------------------

ellipse_points <- function(cx, cy, rx, ry, angle = 0, n = 40) {
  theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  x <- rx * cos(theta)
  y <- ry * sin(theta)
  cos_a <- cos(angle)
  sin_a <- sin(angle)
  list(
    x = cx + x * cos_a - y * sin_a,
    y = cy + x * sin_a + y * cos_a
  )
}

ellipse_grob <- function(cx, cy, rx, ry, angle = 0, gp = grid::gpar()) {
  pts <- ellipse_points(cx, cy, rx, ry, angle)
  grid::polygonGrob(x = pts$x, y = pts$y, gp = gp)
}

disc_to_ellipse <- function(pts, cx, cy, rx, ry) {
  list(x = cx + rx * pts[, 1], y = cy + ry * pts[, 2])
}

# The cat ------------------------------------------------------------------

# Draw one cat.  `x`/`y` are npc units, `width`/`height` are the physical size
# of the cat, and `stroke` is a grid line width (lwd) for the outline.
cat_grob <- function(
  x,
  y,
  width,
  height,
  coat = "black",
  colour = NA,
  fill = NA,
  alpha = NA,
  angle = 0,
  eyes = "open",
  whiskers = TRUE,
  seed = 1,
  stroke = 1
) {
  eyes <- match.arg(eyes, c("open", "closed"))
  spec <- coat_spec(coat, seed)

  base <- if (is.na(fill)) spec$base else fill
  outline <- if (is.na(colour)) contrast_colour(base) else colour
  opacity <- if (is.na(alpha)) 1 else alpha

  alpha_col <- function(col) ggplot2::alpha(col, opacity)
  fill_gp <- function(col) grid::gpar(col = NA, fill = alpha_col(col))
  line_gp <- function(col = outline, lwd = stroke) {
    grid::gpar(
      col = alpha_col(col),
      fill = NA,
      lwd = lwd,
      linejoin = "round",
      lineend = "round"
    )
  }

  # Proportions of a sitting cat, in a square viewport.
  body_x <- 0.5
  body_y <- 0.36
  body_rx <- 0.26
  body_ry <- 0.28
  head_x <- 0.5
  head_y <- 0.72
  head_r <- 0.19

  body_patches <- Filter(function(p) p$part == "body", spec$patches)
  head_patches <- Filter(function(p) p$part == "head", spec$patches)

  parts <- list()

  tail_lwd <- stroke * 3.4
  parts$tail_rim <- grid::xsplineGrob(
    x = c(0.70, 0.86, 0.97, 0.95),
    y = c(0.14, 0.10, 0.27, 0.50),
    shape = 1,
    open = TRUE,
    gp = line_gp(outline, tail_lwd + stroke * 1.2)
  )
  parts$tail <- grid::xsplineGrob(
    x = c(0.70, 0.86, 0.97, 0.95),
    y = c(0.14, 0.10, 0.27, 0.50),
    shape = 1,
    open = TRUE,
    gp = line_gp(spec$tail %||% base, tail_lwd)
  )

  parts$ear_left <- grid::polygonGrob(
    x = c(0.345, 0.330, 0.465),
    y = c(0.830, 0.980, 0.875),
    gp = grid::gpar(
      col = alpha_col(outline),
      fill = alpha_col(base),
      lwd = stroke,
      linejoin = "round"
    )
  )
  parts$ear_right <- grid::polygonGrob(
    x = c(0.655, 0.670, 0.535),
    y = c(0.830, 0.980, 0.875),
    gp = grid::gpar(
      col = alpha_col(outline),
      fill = alpha_col(base),
      lwd = stroke,
      linejoin = "round"
    )
  )
  parts$ear_left_inner <- grid::polygonGrob(
    x = c(0.375, 0.362, 0.442),
    y = c(0.848, 0.940, 0.884),
    gp = fill_gp(spec$ear)
  )
  parts$ear_right_inner <- grid::polygonGrob(
    x = c(0.625, 0.638, 0.558),
    y = c(0.848, 0.940, 0.884),
    gp = fill_gp(spec$ear)
  )

  parts$body <- ellipse_grob(
    body_x,
    body_y,
    body_rx,
    body_ry,
    gp = grid::gpar(
      col = alpha_col(outline),
      fill = alpha_col(base),
      lwd = stroke,
      linejoin = "round"
    )
  )
  for (patch in body_patches) {
    pts <- disc_to_ellipse(patch$poly, body_x, body_y, body_rx, body_ry)
    parts[[length(parts) + 1]] <- grid::polygonGrob(
      x = pts$x,
      y = pts$y,
      gp = fill_gp(patch$colour)
    )
  }

  paw_colour <- spec$paw %||% base
  parts$paw_left <- ellipse_grob(
    0.37,
    0.105,
    0.10,
    0.048,
    gp = grid::gpar(
      col = alpha_col(outline),
      fill = alpha_col(paw_colour),
      lwd = stroke
    )
  )
  parts$paw_right <- ellipse_grob(
    0.63,
    0.105,
    0.10,
    0.048,
    gp = grid::gpar(
      col = alpha_col(outline),
      fill = alpha_col(paw_colour),
      lwd = stroke
    )
  )

  parts$head <- grid::circleGrob(
    x = head_x,
    y = head_y,
    r = head_r,
    gp = grid::gpar(
      col = alpha_col(outline),
      fill = alpha_col(base),
      lwd = stroke
    )
  )
  for (patch in head_patches) {
    pts <- disc_to_ellipse(patch$poly, head_x, head_y, head_r, head_r)
    parts[[length(parts) + 1]] <- grid::polygonGrob(
      x = pts$x,
      y = pts$y,
      gp = fill_gp(patch$colour)
    )
  }

  parts$muzzle_left <- ellipse_grob(
    0.455,
    0.655,
    0.058,
    0.045,
    gp = fill_gp(spec$muzzle)
  )
  parts$muzzle_right <- ellipse_grob(
    0.545,
    0.655,
    0.058,
    0.045,
    gp = fill_gp(spec$muzzle)
  )

  parts$nose <- grid::polygonGrob(
    x = c(0.478, 0.522, 0.5),
    y = c(0.680, 0.680, 0.658),
    gp = fill_gp("#e08a9b")
  )

  mouth_gp <- line_gp(mix_colour(outline, base, 0.15), stroke * 0.7)
  parts$mouth_left <- grid::xsplineGrob(
    x = c(0.5, 0.472, 0.452),
    y = c(0.658, 0.630, 0.645),
    shape = 1,
    open = TRUE,
    gp = mouth_gp
  )
  parts$mouth_right <- grid::xsplineGrob(
    x = c(0.5, 0.528, 0.548),
    y = c(0.658, 0.630, 0.645),
    shape = 1,
    open = TRUE,
    gp = mouth_gp
  )

  if (eyes == "open") {
    for (side in c("left", "right")) {
      eye_x <- if (side == "left") 0.435 else 0.565
      parts[[paste0("eye_", side)]] <- ellipse_grob(
        eye_x,
        0.748,
        0.038,
        0.044,
        gp = fill_gp(spec$eye)
      )
      parts[[paste0("pupil_", side)]] <- ellipse_grob(
        eye_x,
        0.748,
        0.011,
        0.030,
        gp = fill_gp("#151515")
      )
      parts[[paste0("glint_", side)]] <- grid::circleGrob(
        x = eye_x - 0.012,
        y = 0.762,
        r = 0.007,
        gp = fill_gp("#ffffff")
      )
    }
  } else {
    for (side in c("left", "right")) {
      eye_x <- if (side == "left") 0.435 else 0.565
      parts[[paste0("eye_", side)]] <- grid::xsplineGrob(
        x = c(eye_x - 0.035, eye_x, eye_x + 0.035),
        y = c(0.752, 0.735, 0.752),
        shape = 1,
        open = TRUE,
        gp = line_gp(outline, stroke * 1.1)
      )
    }
  }

  if (whiskers) {
    whisker_gp <- line_gp(
      mix_colour(outline, "#ffffff", 0.35),
      max(stroke * 0.5, 0.25)
    )
    parts$whiskers <- grid::segmentsGrob(
      x0 = c(0.40, 0.40, 0.40, 0.60, 0.60, 0.60),
      y0 = c(0.668, 0.652, 0.636, 0.668, 0.652, 0.636),
      x1 = c(0.22, 0.20, 0.22, 0.78, 0.80, 0.78),
      y1 = c(0.700, 0.652, 0.604, 0.700, 0.652, 0.604),
      gp = whisker_gp
    )
  }

  grid::gTree(
    children = do.call(grid::gList, parts),
    vp = grid::viewport(
      x = x,
      y = y,
      width = width,
      height = height,
      angle = angle,
      clip = "off"
    )
  )
}

# Legend key ---------------------------------------------------------------

draw_key_cat <- function(data, params, size) {
  key_mm <- size[[1]]
  cat_mm <- data$size
  # Keep the key box a standard size, but let the cat inside it grow with the
  # `size` aesthetic so that size legends show a difference.
  fill_fraction <- if (is.null(cat_mm) || is.na(cat_mm[[1]])) {
    1
  } else {
    min(cat_mm[[1]] / (1.5 * key_mm), 1)
  }
  linewidth <- data$linewidth %||% 1
  if (is.na(linewidth)) {
    linewidth <- 1
  }

  grob <- cat_grob(
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5, "npc"),
    width = grid::unit(0.9 * fill_fraction, "snpc"),
    height = grid::unit(0.9 * fill_fraction, "snpc"),
    coat = data$coat %||% "black",
    colour = data$colour %||% NA,
    fill = data$fill %||% NA,
    alpha = data$alpha %||% NA,
    angle = 0,
    eyes = params$eyes %||% "open",
    whiskers = params$whiskers %||% TRUE,
    seed = params$seed %||% 1,
    stroke = 0.9 * linewidth
  )
  # Tell the legend how big the key is, in cm.  Without this, ggplot2 sizes
  # the key from the grob and the cats make the legend very tall.
  attr(grob, "width") <- key_mm / 10
  attr(grob, "height") <- key_mm / 10
  grob
}
