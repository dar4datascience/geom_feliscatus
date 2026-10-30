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
  stroke = 1,
  pose = "sitting",
  tail = "up",
  accessory = "none",
  mood = "none",
  silhouette = FALSE
) {
  eyes <- match.arg(eyes, c("open", "closed"))
  pose <- match_pose(pose)
  tail <- match_tail(tail)
  accessory <- match_accessory(accessory)
  mood <- match_mood(mood)

  spec <- coat_spec(coat, seed)

  base <- if (is.na(fill)) spec$base else fill
  outline <- if (is.na(colour)) contrast_colour(base) else colour
  opacity <- if (is.na(alpha)) 1 else alpha

  # A silhouette is one solid shape, so a white cat must not vanish on a
  # white page.  Fall back to ink unless the user picks a colour.
  if (silhouette) {
    base <- if (!is.na(fill)) fill else if (!is.na(colour)) colour else "#1f1b18"
  }

  ctx <- cat_ctx(spec, base, outline, opacity, stroke, silhouette)
  anchors <- pose_anchors(pose)
  tail_grobs <- tail_parts(tail, pose, anchors, ctx)

  back <- if (silhouette) list() else mood_parts(mood, anchors, ctx, "back")

  outer <- c(pose_outer(pose, anchors, ctx), tail_grobs$back)
  front_tail <- tail_grobs$front
  halo <- add_halo(outer, ctx)
  # A tail drawn in front of the body needs its border drawn after the body
  # too, or it disappears into fur of the same colour.
  front_halo <- add_halo(front_tail, ctx)

  detail <- if (silhouette) {
    list()
  } else {
    c(
      ear_grobs(anchors$head, ctx, inner = TRUE),
      pose_patches(anchors, ctx),
      cat_face(anchors, ctx, eyes = eyes, whiskers = whiskers)
    )
  }

  front <- if (silhouette) {
    list()
  } else {
    c(
      accessory_parts(accessory, anchors, ctx),
      mood_parts(mood, anchors, ctx, "front")
    )
  }

  parts <- c(back, halo, outer, front_halo, front_tail, detail, front)

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
    stroke = 0.9 * linewidth,
    pose = data$pose %||% "sitting",
    tail = data$tail %||% "up",
    accessory = params$accessory %||% "none",
    mood = "none",
    silhouette = params$silhouette %||% FALSE
  )
  # Tell the legend how big the key is, in cm.  Without this, ggplot2 sizes
  # the key from the grob and the cats make the legend very tall.
  attr(grob, "width") <- key_mm / 10
  attr(grob, "height") <- key_mm / 10
  grob
}
