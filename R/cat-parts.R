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

# Drawing context ----------------------------------------------------------

# Everything the part builders need, so that poses, tails, faces and
# accessories share one definition of the fur, the rim and the stroke width.
cat_ctx <- function(
  spec,
  base,
  outline,
  opacity,
  stroke,
  silhouette = FALSE
) {
  alpha_col <- function(col) ggplot2::alpha(col, opacity)
  list(
    spec = spec,
    base = base,
    outline = outline,
    stroke = stroke,
    silhouette = silhouette,
    halo = if (silhouette) 0 else stroke * 1.1,
    alpha_col = alpha_col,
    fill_gp = function(col) grid::gpar(col = NA, fill = alpha_col(col)),
    line_gp = function(col = outline, lwd = stroke) {
      grid::gpar(
        col = alpha_col(col),
        fill = NA,
        lwd = lwd,
        linejoin = "round",
        lineend = "round"
      )
    },
    outer_gp = function(fill = base) {
      if (silhouette) {
        grid::gpar(col = NA, fill = alpha_col(base), lwd = 0)
      } else {
        grid::gpar(
          col = alpha_col(outline),
          fill = alpha_col(fill),
          lwd = stroke,
          linejoin = "round"
        )
      }
    }
  )
}

outer_ellipse <- function(cx, cy, rx, ry, fill, ctx) {
  ellipse_grob(cx, cy, rx, ry, gp = ctx$outer_gp(fill))
}

outer_poly <- function(x, y, fill, ctx) {
  grid::polygonGrob(x = x, y = y, gp = ctx$outer_gp(fill))
}

named <- function(grob, name) {
  grob$name <- name
  grob
}

# A sticker-style die-cut border.  Draw every outer part again, in white and
# slightly wider, before the coloured cat so the border sits behind it.
add_halo <- function(grobs, ctx) {
  if (ctx$silhouette || ctx$halo <= 0) {
    return(list())
  }
  lapply(grobs, function(g) {
    gp <- g$gp %||% grid::gpar()
    gp$col <- ctx$alpha_col("#ffffff")
    gp$fill <- ctx$alpha_col("#ffffff")
    gp$lwd <- (gp$lwd %||% 0) + 2 * ctx$halo
    g$gp <- gp
    # The halo and the coloured part are the same shape, so they must not
    # share a name: grid draws only the first of several siblings with the
    # same name.
    g$name <- paste0(g$name %||% "cat_part", ".halo")
    g
  })
}
