# The face is drawn in the original sitting-cat coordinates and scaled onto
# whatever head the pose provides.
cat_face <- function(a, ctx, eyes = "open", whiskers = TRUE) {
  hx <- a$head[1]
  hy <- a$head[2]
  hr <- a$head[3]
  s <- hr / 0.19
  fx <- function(x) hx + (x - 0.5) * s
  fy <- function(y) hy + (y - 0.72) * s

  spec <- ctx$spec
  parts <- list()

  parts$muzzle_left <- ellipse_grob(
    fx(0.455),
    fy(0.655),
    0.058 * s,
    0.045 * s,
    gp = ctx$fill_gp(spec$muzzle)
  )
  parts$muzzle_right <- ellipse_grob(
    fx(0.545),
    fy(0.655),
    0.058 * s,
    0.045 * s,
    gp = ctx$fill_gp(spec$muzzle)
  )

  parts$nose <- grid::polygonGrob(
    x = fx(c(0.478, 0.522, 0.5)),
    y = fy(c(0.680, 0.680, 0.658)),
    gp = ctx$fill_gp("#e08a9b")
  )

  mouth_gp <- ctx$line_gp(
    mix_colour(ctx$outline, ctx$base, 0.15),
    ctx$stroke * 0.7
  )
  parts$mouth_left <- grid::xsplineGrob(
    x = fx(c(0.5, 0.472, 0.452)),
    y = fy(c(0.658, 0.630, 0.645)),
    shape = 1,
    open = TRUE,
    gp = mouth_gp
  )
  parts$mouth_right <- grid::xsplineGrob(
    x = fx(c(0.5, 0.528, 0.548)),
    y = fy(c(0.658, 0.630, 0.645)),
    shape = 1,
    open = TRUE,
    gp = mouth_gp
  )

  if (eyes == "open") {
    for (side in c("left", "right")) {
      eye_x <- if (side == "left") 0.435 else 0.565
      parts[[paste0("eye_", side)]] <- ellipse_grob(
        fx(eye_x),
        fy(0.748),
        0.038 * s,
        0.044 * s,
        gp = ctx$fill_gp(spec$eye)
      )
      parts[[paste0("pupil_", side)]] <- ellipse_grob(
        fx(eye_x),
        fy(0.748),
        0.011 * s,
        0.030 * s,
        gp = ctx$fill_gp("#151515")
      )
      parts[[paste0("glint_", side)]] <- grid::circleGrob(
        x = fx(eye_x - 0.012),
        y = fy(0.762),
        r = 0.007 * s,
        gp = ctx$fill_gp("#ffffff")
      )
    }
  } else {
    for (side in c("left", "right")) {
      eye_x <- if (side == "left") 0.435 else 0.565
      parts[[paste0("eye_", side)]] <- grid::xsplineGrob(
        x = fx(c(eye_x - 0.035, eye_x, eye_x + 0.035)),
        y = fy(c(0.752, 0.735, 0.752)),
        shape = 1,
        open = TRUE,
        gp = ctx$line_gp(ctx$outline, ctx$stroke * 1.1)
      )
    }
  }

  if (whiskers) {
    parts$whiskers <- grid::segmentsGrob(
      x0 = fx(c(0.40, 0.40, 0.40, 0.60, 0.60, 0.60)),
      y0 = fy(c(0.668, 0.652, 0.636, 0.668, 0.652, 0.636)),
      x1 = fx(c(0.22, 0.20, 0.22, 0.78, 0.80, 0.78)),
      y1 = fy(c(0.700, 0.652, 0.604, 0.700, 0.652, 0.604)),
      gp = ctx$line_gp(
        mix_colour(ctx$outline, "#ffffff", 0.35),
        max(ctx$stroke * 0.5, 0.25)
      )
    )
  }

  parts
}
