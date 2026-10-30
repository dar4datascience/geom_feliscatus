# Template: a new geom for ggplot2
# ---------------------------------------------------------------------------
# Copy this file into R/ with:
#
#   usethis::use_template(
#     "geom-template.R",
#     save_as = "R/geom-thing.R",
#     package = "geomfeliscatus"
#   )
#
# Then replace "thing"/"Thing" with your geom's name, and work through the
# checklist at the bottom.  The template is deliberately small: it draws
# circles, so it runs as-is once copied.
#
# Read vignette("extending-ggplot2", package = "geomfeliscatus") for the
# reasoning behind each part.

# 1. The Geom ---------------------------------------------------------------
# A Geom turns one row of layer data into one or more grobs.
GeomThing <- ggplot2::ggproto(
  "GeomThing",
  ggplot2::Geom,
  # Aesthetics the user must supply.
  required_aes = c("x", "y"),
  # Aesthetics that get a value when the user does not supply one.  Any
  # name here is a new aesthetic, and can be mapped in aes().
  default_aes = ggplot2::aes(
    colour = "black",
    fill = NA,
    size = 2,
    alpha = NA
  ),
  # Parameters that are not aesthetics.  "na.rm" is the usual one.
  extra_params = "na.rm",
  # The legend key.
  draw_key = draw_key_thing,
  # The drawing method.  Keep the formal arguments explicit: ggplot2 reads
  # them to decide which layer parameters are valid.  Do not add `...` here.
  draw_panel = function(self, data, panel_params, coord, shape = "circle") {
    # Data is in data units; transform it to npc (0-1 within the panel).
    coords <- coord$transform(data, panel_params)

    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      grid::circleGrob(
        x = grid::unit(coords$x[i], "npc"),
        y = grid::unit(coords$y[i], "npc"),
        r = grid::unit(coords$size[i], "mm"),
        gp = grid::gpar(
          col = ggplot2::alpha(coords$colour[i], coords$alpha[i]),
          fill = ggplot2::alpha(coords$fill[i], coords$alpha[i])
        )
      )
    })

    grid::gTree(
      children = do.call(grid::gList, grobs),
      name = "geom_thing"
    )
  }
)

# 2. The legend key ---------------------------------------------------------
# Draw in a 0-1 viewport.  "snpc" keeps the key square.
draw_key_thing <- function(data, params, size) {
  grid::circleGrob(
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5, "npc"),
    r = grid::unit(0.25, "snpc"),
    gp = grid::gpar(
      col = ggplot2::alpha(data$colour %||% "black", data$alpha %||% NA),
      fill = ggplot2::alpha(data$fill %||% NA, data$alpha %||% NA)
    )
  )
}

# 3. The user-facing function ----------------------------------------------
# Copy the shape of ggplot2::geom_point(): the geom is fixed, the stat is
# flexible, and everything else goes into `params`.
geom_thing <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  shape = "circle",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    geom = GeomThing,
    stat = stat,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      shape = shape,
      ...
    )
  )
}

# 4. A Stat (optional) ------------------------------------------------------
# A Stat computes on the data before the geom draws it.  `compute_group()`
# is called once per group, and must return a data frame with the
# aesthetics the geom needs.
StatThing <- ggplot2::ggproto(
  "StatThing",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {
    data.frame(x = mean(data$x), y = mean(data$y))
  }
)

stat_thing <- function(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    geom = geom,
    stat = StatThing,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(na.rm = na.rm, ...)
  )
}

# 5. A default scale for a new aesthetic (optional) -------------------------
# When a discrete aesthetic called "thing" is mapped, ggplot2 looks for
# scale_thing_discrete() in the environment of the ggplot() call.  Return an
# identity scale when the values are meaningful as-is, or a discrete_scale()
# with a palette when they need translating.
scale_thing_discrete <- function(..., guide = "legend") {
  ggplot2::scale_discrete_identity(
    aesthetics = "thing",
    ...,
    guide = guide
  )
}

# 6. A position (optional) --------------------------------------------------
# Positions subclass ggplot2::Position and implement `compute_layer()`.
# Most geoms never need one.

# Checklist ----------------------------------------------------------------
# [ ] Rename the class, the functions and the scale.
# [ ] Add roxygen documentation to every exported object, including an
#     @section Aesthetics block.
# [ ] Export the Geom and the draw_key if other packages should reuse them.
# [ ] Add the topics to _pkgdown.yml.
# [ ] Add tests in tests/testthat/test-geom-thing.R:
#       - ggplot_build() to check the render-ready data
#       - layer_grob() to check the number of grobs
#       - expect_snapshot(error = TRUE, ...) for error messages
# [ ] Add a NEWS.md bullet.
# [ ] Run devtools::document(), devtools::test() and devtools::check().
