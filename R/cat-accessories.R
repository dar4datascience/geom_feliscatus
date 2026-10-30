#' Cat accessories understood by [geom_cat()]
#'
#' `cat_accessories()` lists the accessories [geom_cat()] can draw.  The
#' `accessory` layer parameter accepts these names.
#'
#' @return A character vector of accessory names.
#' @export
#' @examples
#' cat_accessories()
cat_accessories <- function() {
  c(
    "none",
    "collar",
    "bowtie",
    "party_hat",
    "wizard_hat",
    "scarf",
    "glasses",
    "headphones",
    "crown",
    "santa_hat"
  )
}

match_accessory <- function(accessory, call = caller_env()) {
  key <- tolower(trimws(as.character(accessory)))
  unknown <- setdiff(unique(key), cat_accessories())
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown cat accessor{?ies}: {.val {unknown}}.",
        "i" = "Supported accessories are {.val {cat_accessories()}}."
      ),
      call = call
    )
  }
  key
}

acc_gp <- function(fill, ctx, lwd = ctx$stroke * 0.8) {
  grid::gpar(
    col = ctx$alpha_col(ctx$outline),
    fill = ctx$alpha_col(fill),
    lwd = lwd,
    linejoin = "round"
  )
}

acc_poly <- function(x, y, fill, ctx, lwd = ctx$stroke * 0.8) {
  grid::polygonGrob(x = x, y = y, gp = acc_gp(fill, ctx, lwd))
}

acc_circle <- function(x, y, r, fill, ctx, lwd = ctx$stroke * 0.8) {
  grid::circleGrob(x = x, y = y, r = r, gp = acc_gp(fill, ctx, lwd))
}

accessory_parts <- function(accessory, a, ctx) {
  if (accessory == "none") {
    return(list())
  }
  hx <- a$head[1]
  hy <- a$head[2]
  hr <- a$head[3]
  nx <- a$neck[1]
  ny <- a$neck[2]
  s <- hr / 0.19

  switch(
    accessory,
    collar = list(
      grid::xsplineGrob(
        x = c(nx - hr * 0.85, nx, nx + hr * 0.85),
        y = c(ny + 0.01, ny - 0.035, ny + 0.01),
        shape = 1,
        open = TRUE,
        gp = ctx$line_gp("#c0392b", ctx$stroke * 3.5)
      ),
      acc_circle(nx, ny - 0.035, hr * 0.12, "#f1c40f", ctx)
    ),
    bowtie = list(
      acc_poly(
        c(nx - 0.12 * s, nx - 0.12 * s, nx - 0.02 * s),
        c(ny + 0.06 * s, ny - 0.06 * s, ny),
        "#c0392b",
        ctx
      ),
      acc_poly(
        c(nx + 0.12 * s, nx + 0.12 * s, nx + 0.02 * s),
        c(ny + 0.06 * s, ny - 0.06 * s, ny),
        "#c0392b",
        ctx
      ),
      acc_circle(nx, ny, hr * 0.055, "#96281b", ctx)
    ),
    party_hat = list(
      acc_poly(
        c(hx - hr * 0.55, hx + hr * 0.55, hx + hr * 0.15),
        c(hy + hr * 0.75, hy + hr * 0.75, hy + hr * 2.0),
        "#f1c40f",
        ctx
      ),
      grid::segmentsGrob(
        x0 = hx - hr * 0.33,
        y0 = hy + hr * 1.30,
        x1 = hx + hr * 0.33,
        y1 = hy + hr * 1.30,
        gp = ctx$line_gp("#e67e22", ctx$stroke * 1.5)
      ),
      acc_circle(hx + hr * 0.15, hy + hr * 2.0, hr * 0.12, "#e74c3c", ctx)
    ),
    wizard_hat = list(
      ellipse_grob(
        hx,
        hy + hr * 0.85,
        hr * 0.95,
        hr * 0.16,
        gp = acc_gp("#4a3a8a", ctx)
      ),
      acc_poly(
        c(hx - hr * 0.48, hx + hr * 0.48, hx + hr * 0.18),
        c(hy + hr * 0.90, hy + hr * 0.90, hy + hr * 2.30),
        "#5b4bb5",
        ctx
      ),
      acc_poly(
        c(
          hx + hr * 0.08,
          hx + hr * 0.14,
          hx + hr * 0.22,
          hx + hr * 0.14,
          hx + hr * 0.08,
          hx,
          hx + hr * 0.02
        ),
        c(
          hy + hr * 1.66,
          hy + hr * 1.60,
          hy + hr * 1.54,
          hy + hr * 1.48,
          hy + hr * 1.54,
          hy + hr * 1.54,
          hy + hr * 1.60
        ),
        "#f1c40f",
        ctx
      )
    ),
    scarf = list(
      grid::xsplineGrob(
        x = c(nx - hr * 0.85, nx, nx + hr * 0.85),
        y = c(ny + 0.02, ny - 0.04, ny + 0.02),
        shape = 1,
        open = TRUE,
        gp = ctx$line_gp("#2e86c1", ctx$stroke * 4)
      ),
      acc_poly(
        c(nx + 0.02 * s, nx + 0.10 * s, nx + 0.12 * s, nx + 0.04 * s),
        c(ny - 0.02 * s, ny - 0.02 * s, ny - 0.24 * s, ny - 0.26 * s),
        "#2e86c1",
        ctx
      ),
      grid::segmentsGrob(
        x0 = nx + c(0.04, 0.07, 0.10) * s,
        y0 = ny - 0.25 * s,
        x1 = nx + c(0.03, 0.07, 0.11) * s,
        y1 = ny - 0.30 * s,
        gp = ctx$line_gp("#1f618d", ctx$stroke * 0.9)
      )
    ),
    glasses = list(
      grid::circleGrob(
        x = hx - hr * 0.342,
        y = hy + hr * 0.147,
        r = hr * 0.30,
        gp = ctx$line_gp(ctx$outline, ctx$stroke * 1.2)
      ),
      grid::circleGrob(
        x = hx + hr * 0.342,
        y = hy + hr * 0.147,
        r = hr * 0.30,
        gp = ctx$line_gp(ctx$outline, ctx$stroke * 1.2)
      ),
      grid::segmentsGrob(
        x0 = hx - hr * 0.05,
        y0 = hy + hr * 0.147,
        x1 = hx + hr * 0.05,
        y1 = hy + hr * 0.147,
        gp = ctx$line_gp(ctx$outline, ctx$stroke * 1.2)
      ),
      grid::segmentsGrob(
        x0 = hx + c(-0.64, 0.64) * hr,
        y0 = hy + hr * 0.147,
        x1 = hx + c(-0.95, 0.95) * hr,
        y1 = hy + hr * 0.25,
        gp = ctx$line_gp(ctx$outline, ctx$stroke * 1.2)
      )
    ),
    headphones = list(
      grid::xsplineGrob(
        x = c(hx - hr * 0.95, hx, hx + hr * 0.95),
        y = c(hy + hr * 0.15, hy + hr * 1.30, hy + hr * 0.15),
        shape = 1,
        open = TRUE,
        gp = ctx$line_gp("#2c3e50", ctx$stroke * 3.5)
      ),
      grid::roundrectGrob(
        x = hx - hr * 1.02,
        y = hy + hr * 0.05,
        width = hr * 0.36,
        height = hr * 0.62,
        r = grid::unit(hr * 0.10, "npc"),
        gp = acc_gp("#2c3e50", ctx)
      ),
      grid::roundrectGrob(
        x = hx + hr * 1.02,
        y = hy + hr * 0.05,
        width = hr * 0.36,
        height = hr * 0.62,
        r = grid::unit(hr * 0.10, "npc"),
        gp = acc_gp("#2c3e50", ctx)
      )
    ),
    crown = list(
      acc_poly(
        c(
          hx - hr * 0.70,
          hx - hr * 0.55,
          hx - hr * 0.38,
          hx - hr * 0.12,
          hx,
          hx + hr * 0.12,
          hx + hr * 0.38,
          hx + hr * 0.55,
          hx + hr * 0.70
        ),
        c(
          hy + hr * 0.95,
          hy + hr * 1.45,
          hy + hr * 1.05,
          hy + hr * 1.60,
          hy + hr * 1.10,
          hy + hr * 1.60,
          hy + hr * 1.05,
          hy + hr * 1.45,
          hy + hr * 0.95
        ),
        "#f1c40f",
        ctx
      ),
      acc_circle(
        hx - hr * 0.38,
        hy + hr * 1.08,
        hr * 0.060,
        "#c0392b",
        ctx,
        lwd = 0
      ),
      acc_circle(hx, hy + hr * 1.16, hr * 0.060, "#2e86c1", ctx, lwd = 0),
      acc_circle(
        hx + hr * 0.38,
        hy + hr * 1.08,
        hr * 0.060,
        "#27ae60",
        ctx,
        lwd = 0
      )
    ),
    santa_hat = list(
      acc_poly(
        c(hx - hr * 0.55, hx + hr * 0.55, hx + hr * 0.30),
        c(hy + hr * 0.78, hy + hr * 0.78, hy + hr * 2.15),
        "#c0392b",
        ctx
      ),
      ellipse_grob(
        hx,
        hy + hr * 0.78,
        hr * 0.65,
        hr * 0.16,
        gp = acc_gp("#ffffff", ctx)
      ),
      acc_circle(hx + hr * 0.30, hy + hr * 2.15, hr * 0.13, "#ffffff", ctx)
    )
  )
}
