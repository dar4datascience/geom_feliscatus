#' Cat tails understood by [geom_cat()]
#'
#' `cat_tails()` lists the tail shapes [geom_cat()] knows how to draw.  The
#' names are also the values the `tail` aesthetic accepts, so the tail scale is
#' an identity scale.
#'
#' @return A character vector of tail names.
#' @export
#' @examples
#' cat_tails()
cat_tails <- function() {
  c("up", "curl", "plume", "kink", "bob", "none")
}

tail_aliases <- c(
  straight = "up",
  wrapped = "curl",
  fluffy = "plume",
  stub = "bob",
  manx = "none",
  tailless = "none"
)

match_tail <- function(tail, call = caller_env()) {
  key <- tolower(trimws(as.character(tail)))
  key <- ifelse(key %in% names(tail_aliases), tail_aliases[key], key)
  unknown <- setdiff(unique(key), cat_tails())
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown cat tail{?s}: {.val {unknown}}.",
        "i" = "Supported tails are {.val {cat_tails()}}."
      ),
      call = call
    )
  }
  key
}

tail_path <- function(tail, root, a, dir, sx = 1) {
  switch(
    tail,
    up = rbind(
      root,
      root + c(0.16 * dir * sx, -0.04),
      root + c(0.27 * dir * sx, 0.14),
      root + c(0.24 * dir * sx, 0.44)
    ),
    plume = rbind(
      root,
      root + c(0.14 * dir * sx, -0.02),
      root + c(0.24 * dir * sx, 0.16),
      root + c(0.21 * dir * sx, 0.46)
    ),
    kink = rbind(
      root,
      root + c(0.14 * dir * sx, 0.02),
      root + c(0.24 * dir * sx, 0.12),
      root + c(0.17 * dir * sx, 0.34)
    ),
    bob = rbind(
      root,
      root + c(0.06 * dir * sx, 0.04),
      root + c(0.10 * dir * sx, 0.12)
    ),
    curl = {
      b <- a$body
      side <- if (root[1] >= 0.5) 1 else -1
      rbind(
        root,
        c(root[1] + 0.08 * side, root[2] - 0.04),
        c(b[1] + b[3] * 0.30 * side, b[2] - b[4] * 1.18),
        c(b[1] - b[3] * 0.30 * side, b[2] - b[4] * 1.18),
        c(b[1] - b[3] * 0.85 * side, b[2] - b[4] * 0.80)
      )
    }
  )
}

tail_shape <- function(tail) {
  if (tail == "kink") 0 else 1
}

tail_width <- function(tail, stroke) {
  mult <- switch(tail, plume = 1.55, bob = 1.15, 1)
  stroke * 2.8 * mult
}

# A point and unit tangent at fraction `t` of a polyline.
sample_path <- function(pts, t) {
  d <- sqrt(rowSums(diff(pts)^2))
  cum <- c(0, cumsum(d))
  target <- t * cum[length(cum)]
  i <- max(which(cum <= target))
  i <- min(i, nrow(pts) - 1)
  frac <- (target - cum[i]) / d[i]
  list(
    p = pts[i, ] + frac * (pts[i + 1, ] - pts[i, ]),
    tangent = (pts[i + 1, ] - pts[i, ]) / d[i]
  )
}

# Tabby rings are short strokes across the tail, so they follow whatever path
# the tail aesthetic chose.
tail_rings <- function(pts, ctx) {
  if (!isTRUE(ctx$spec$rings) || ctx$silhouette) {
    return(list())
  }
  rings <- c(0.35, 0.62)
  lapply(seq_along(rings), function(i) {
    at <- sample_path(pts, rings[i])
    perp <- c(-at$tangent[2], at$tangent[1]) * 0.05
    named(
      grid::segmentsGrob(
        x0 = at$p[1] - perp[1],
        y0 = at$p[2] - perp[2],
        x1 = at$p[1] + perp[1],
        y1 = at$p[2] + perp[2],
        gp = ctx$line_gp(ctx$spec$stripe, ctx$stroke * 1.2)
      ),
      paste0("cat_tail_ring_", i)
    )
  })
}

tail_parts <- function(tail, pose, a, ctx) {
  if (tail == "none") {
    return(list(back = list(), front = list()))
  }
  dir <- if (pose == "standing") -1 else 1
  sx <- if (pose == "standing") 0.6 else 1
  pts <- tail_path(tail, a$tail, a, dir, sx)
  w <- tail_width(tail, ctx$stroke)
  colour <- ctx$spec$tail %||% ctx$base

  gp <- if (ctx$silhouette) {
    grid::gpar(col = ctx$alpha_col(ctx$base), lwd = w, lineend = "round")
  } else {
    ctx$line_gp(colour, w)
  }

  grobs <- list(named(
    grid::xsplineGrob(
      x = pts[, 1],
      y = pts[, 2],
      shape = tail_shape(tail),
      open = TRUE,
      gp = gp
    ),
    "cat_tail"
  ))

  if (tail == "plume" && !ctx$silhouette) {
    grobs <- c(
      grobs,
      list(named(
        grid::xsplineGrob(
          x = pts[, 1],
          y = pts[, 2],
          shape = 1,
          open = TRUE,
          gp = ctx$line_gp(mix_colour(colour, "#ffffff", 0.25), w * 0.55)
        ),
        "cat_tail_fluff"
      ))
    )
  }

  if (tail %in% c("up", "plume", "kink")) {
    grobs <- c(grobs, tail_rings(pts, ctx))
  }

  if (tail == "curl") {
    list(back = list(), front = grobs)
  } else {
    list(back = grobs, front = list())
  }
}
