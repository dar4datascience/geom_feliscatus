#' Cat geoms
#'
#' `geom_cat()` draws a cat at each observation.  Cats come in five coats:
#' `"black"`, `"white"`, `"tuxedo"`, `"calico"` and `"tortoise"` (see
#' [cat_coats()]).
#'
#' @section Aesthetics:
#' `geom_cat()` understands the following aesthetics (required aesthetics are
#' in bold):
#'
#' - **`x`**
#' - **`y`**
#' - `coat`: one of [cat_coats()].  Mapped values are used as-is, so
#'   `aes(coat = coat_name)` needs no manual scale.  Defaults to `"black"`.
#' - `size`: the height of the cat in mm.  Defaults to `8`.
#' - `colour`: outline colour.  `NA` (the default) picks a rim that contrasts
#'   with the fur.
#' - `fill`: overrides the base fur colour.  Coat markings are kept.
#' - `alpha`
#' - `linewidth`: multiplier for the outline width.  Defaults to `1`.
#' - `angle`: rotation of the cat in degrees, counter-clockwise.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data.  The default,
#'   `"identity"`, leaves the data unchanged.
#' @param position A position adjustment.
#' @param ... Other arguments passed on to [ggplot2::layer()].  These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like
#'   `coat = "calico"` or `size = 12`.
#' @param eyes `"open"` (default) or `"closed"`.
#' @param whiskers Should the cat have whiskers?  Defaults to `TRUE`.
#' @param seed Integer used to vary the markings of calico and tortoise cats.
#' @param na.rm If `FALSE` (the default), missing values are removed with a
#'   warning.  If `TRUE`, they are silently removed.
#' @param show.legend Logical.  Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#'
#' @return A `ggplot2` layer that draws cats.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_cat(coat = "tuxedo", size = 10)
#'
#' # The coat can also be mapped from data
#' cats <- data.frame(
#'   x = 1:5,
#'   y = 1,
#'   coat = cat_coats()
#' )
#'
#' ggplot(cats, aes(x, y, coat = coat)) +
#'   geom_cat(size = 16) +
#'   coord_equal()
geom_cat <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  eyes = c("open", "closed"),
  whiskers = TRUE,
  seed = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  eyes <- match.arg(eyes)
  layer(
    geom = GeomCat,
    stat = stat,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      eyes = eyes,
      whiskers = whiskers,
      seed = seed,
      ...
    )
  )
}

#' @rdname geom_cat
#' @format NULL
#' @usage NULL
#' @export
GeomCat <- ggproto(
  "GeomCat",
  Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "coat"),
  default_aes = aes(
    coat = "black",
    size = 8,
    colour = NA,
    fill = NA,
    alpha = NA,
    linewidth = 1,
    angle = 0
  ),
  extra_params = "na.rm",
  draw_key = draw_key_cat,
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    eyes = "open",
    whiskers = TRUE,
    seed = 1
  ) {
    coords <- coord$transform(data, panel_params)
    linewidth <- coords$linewidth
    linewidth[is.na(linewidth)] <- 1
    angle <- coords$angle
    angle[is.na(angle)] <- 0

    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      cat_grob(
        x = grid::unit(coords$x[i], "npc"),
        y = grid::unit(coords$y[i], "npc"),
        width = grid::unit(coords$size[i], "mm"),
        height = grid::unit(coords$size[i], "mm"),
        coat = coords$coat[i],
        colour = coords$colour[i],
        fill = coords$fill[i],
        alpha = coords$alpha[i],
        angle = angle[i],
        eyes = eyes,
        whiskers = whiskers,
        seed = seed + i - 1L,
        stroke = coords$size[i] * 0.11 * linewidth[i]
      )
    })

    grid::gTree(
      children = do.call(grid::gList, grobs),
      name = "geom_cat"
    )
  }
)
