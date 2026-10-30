#' Cat poses understood by [geom_cat()]
#'
#' `cat_poses()` lists the body shapes [geom_cat()] knows how to draw.  The
#' names are also the values the `pose` aesthetic accepts, so the pose scale is
#' an identity scale: the value you supply is the pose you get.
#'
#' @return A character vector of pose names.
#' @export
#' @examples
#' cat_poses()
cat_poses <- function() {
  c("sitting", "loaf", "curled", "standing")
}

pose_aliases <- c(
  sit = "sitting",
  loafing = "loaf",
  sleep = "curled",
  sleeping = "curled",
  curled_up = "curled",
  stand = "standing"
)

match_pose <- function(pose, call = caller_env()) {
  key <- tolower(trimws(as.character(pose)))
  key <- ifelse(key %in% names(pose_aliases), pose_aliases[key], key)
  unknown <- setdiff(unique(key), cat_poses())
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown cat pose{?s}: {.val {unknown}}.",
        "i" = "Supported poses are {.val {cat_poses()}}."
      ),
      call = call
    )
  }
  key
}

# Anchors are the only geometry the other parts need: where the head, body,
# tail root and neck sit, and how big they are.
pose_anchors <- function(pose) {
  switch(
    pose,
    sitting = list(
      head = c(0.50, 0.72, 0.19),
      body = c(0.50, 0.36, 0.26, 0.28),
      tail = c(0.70, 0.14),
      neck = c(0.50, 0.56)
    ),
    loaf = list(
      head = c(0.50, 0.62, 0.18),
      body = c(0.50, 0.28, 0.34, 0.20),
      tail = c(0.82, 0.20),
      neck = c(0.50, 0.47)
    ),
    curled = list(
      head = c(0.34, 0.46, 0.15),
      body = c(0.52, 0.30, 0.30, 0.24),
      tail = c(0.74, 0.22),
      neck = c(0.40, 0.40)
    ),
    standing = list(
      head = c(0.72, 0.60, 0.15),
      body = c(0.44, 0.36, 0.28, 0.15),
      tail = c(0.20, 0.40),
      neck = c(0.62, 0.46)
    )
  )
}

# Ear outlines, in head-radius units, taken from the original sitting cat.
ear_offsets <- list(
  outer_left = rbind(c(-0.816, 0.579), c(-0.895, 1.368), c(-0.184, 0.816)),
  outer_right = rbind(c(0.816, 0.579), c(0.895, 1.368), c(0.184, 0.816)),
  inner_left = rbind(c(-0.658, 0.674), c(-0.726, 1.158), c(-0.305, 0.863)),
  inner_right = rbind(c(0.658, 0.674), c(0.726, 1.158), c(0.305, 0.863))
)

ear_grobs <- function(head, ctx, inner = FALSE) {
  hx <- head[1]
  hy <- head[2]
  r <- head[3]
  keys <- if (inner) {
    c("inner_left", "inner_right")
  } else {
    c("outer_left", "outer_right")
  }
  gp <- if (inner) ctx$fill_gp(ctx$spec$ear) else ctx$outer_gp(ctx$base)
  lapply(keys, function(key) {
    m <- ear_offsets[[key]]
    named(
      grid::polygonGrob(x = hx + m[, 1] * r, y = hy + m[, 2] * r, gp = gp),
      paste0("cat_", key)
    )
  })
}

pose_outer <- function(pose, a, ctx) {
  switch(
    pose,
    sitting = sitting_outer(a, ctx),
    loaf = loaf_outer(a, ctx),
    curled = curled_outer(a, ctx),
    standing = standing_outer(a, ctx)
  )
}

sitting_outer <- function(a, ctx) {
  c(
    ear_grobs(a$head, ctx),
    list(named(
      outer_ellipse(a$body[1], a$body[2], a$body[3], a$body[4], ctx$base, ctx),
      "cat_body"
    )),
    list(
      named(
        outer_ellipse(
          0.37,
          0.105,
          0.10,
          0.048,
          ctx$spec$paw %||% ctx$base,
          ctx
        ),
        "cat_paw_left"
      ),
      named(
        outer_ellipse(
          0.63,
          0.105,
          0.10,
          0.048,
          ctx$spec$paw %||% ctx$base,
          ctx
        ),
        "cat_paw_right"
      )
    ),
    list(named(
      grid::circleGrob(
        a$head[1],
        a$head[2],
        a$head[3],
        gp = ctx$outer_gp(ctx$base)
      ),
      "cat_head"
    ))
  )
}

loaf_outer <- function(a, ctx) {
  c(
    ear_grobs(a$head, ctx),
    list(named(
      outer_ellipse(a$body[1], a$body[2], a$body[3], a$body[4], ctx$base, ctx),
      "cat_body"
    )),
    list(
      named(
        outer_ellipse(0.42, 0.10, 0.07, 0.035, ctx$spec$paw %||% ctx$base, ctx),
        "cat_paw_left"
      ),
      named(
        outer_ellipse(0.58, 0.10, 0.07, 0.035, ctx$spec$paw %||% ctx$base, ctx),
        "cat_paw_right"
      )
    ),
    list(named(
      grid::circleGrob(
        a$head[1],
        a$head[2],
        a$head[3],
        gp = ctx$outer_gp(ctx$base)
      ),
      "cat_head"
    ))
  )
}

curled_outer <- function(a, ctx) {
  c(
    ear_grobs(a$head, ctx),
    list(named(
      outer_ellipse(a$body[1], a$body[2], a$body[3], a$body[4], ctx$base, ctx),
      "cat_body"
    )),
    list(named(
      outer_ellipse(0.46, 0.17, 0.06, 0.035, ctx$spec$paw %||% ctx$base, ctx),
      "cat_paw"
    )),
    list(named(
      grid::circleGrob(
        a$head[1],
        a$head[2],
        a$head[3],
        gp = ctx$outer_gp(ctx$base)
      ),
      "cat_head"
    ))
  )
}

standing_outer <- function(a, ctx) {
  legs <- c(0.26, 0.36, 0.56, 0.66)
  c(
    lapply(seq_along(legs), function(i) {
      named(
        outer_ellipse(legs[i], 0.16, 0.045, 0.11, ctx$base, ctx),
        paste0("cat_leg_", i)
      )
    }),
    ear_grobs(a$head, ctx),
    list(named(
      outer_ellipse(a$body[1], a$body[2], a$body[3], a$body[4], ctx$base, ctx),
      "cat_body"
    )),
    lapply(seq_along(legs), function(i) {
      named(
        outer_ellipse(
          legs[i],
          0.055,
          0.05,
          0.03,
          ctx$spec$paw %||% ctx$base,
          ctx
        ),
        paste0("cat_paw_", i)
      )
    }),
    list(named(
      grid::circleGrob(
        a$head[1],
        a$head[2],
        a$head[3],
        gp = ctx$outer_gp(ctx$base)
      ),
      "cat_head"
    ))
  )
}

# Coat markings are described in unit-disc coordinates, so each pose maps them
# onto its own body and head.
pose_patches <- function(a, ctx) {
  spec <- ctx$spec
  body <- Filter(function(p) p$part == "body", spec$patches)
  head <- Filter(function(p) p$part == "head", spec$patches)
  body_grobs <- lapply(body, function(p) {
    pts <- disc_to_ellipse(p$poly, a$body[1], a$body[2], a$body[3], a$body[4])
    grid::polygonGrob(x = pts$x, y = pts$y, gp = ctx$fill_gp(p$colour))
  })
  head_grobs <- lapply(head, function(p) {
    pts <- disc_to_ellipse(p$poly, a$head[1], a$head[2], a$head[3], a$head[3])
    grid::polygonGrob(x = pts$x, y = pts$y, gp = ctx$fill_gp(p$colour))
  })
  c(body_grobs, head_grobs)
}
