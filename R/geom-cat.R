#' Cat geoms
#'
#' `geom_cat()` draws a cat at each observation.  Cats come in seven coats:
#' `"black"`, `"white"`, `"tuxedo"`, `"calico"`, `"tortoise"`, `"tabby"` and
#' `"siamese"` (see [cat_coats()]), four poses (see [cat_poses()]) and six
#' tails (see [cat_tails()]).
#'
#' @section Aesthetics:
#' `geom_cat()` understands the following aesthetics (required aesthetics are
#' in bold):
#'
#' - **`x`**
#' - **`y`**
#' - `coat`: one of [cat_coats()].  Mapped values are used as-is, so
#'   `aes(coat = coat_name)` needs no manual scale.  Defaults to `"black"`.
#' - `pose`: one of [cat_poses()].  Defaults to `"sitting"`.
#' - `tail`: one of [cat_tails()].  Defaults to `"up"`.
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
#' @param seed Integer used to vary the markings of calico, tortoise and tabby
#'   cats.
#' @param accessory One of [cat_accessories()].  Defaults to `"none"`.
#' @param mood One of [cat_moods()].  Defaults to `"none"`.
#' @param silhouette If `TRUE`, draw the cat as a single solid shape with no
#'   face, markings, whiskers or accessory.  Defaults to `FALSE`.
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
#'   coat = cat_coats()[1:5]
#' )
#'
#' ggplot(cats, aes(x, y, coat = coat)) +
#'   geom_cat(size = 16) +
#'   coord_equal()
#'
#' # Pose and tail are aesthetics, so they can vary by row
#' poses <- data.frame(x = 1:4, y = 1, pose = cat_poses())
#'
#' ggplot(poses, aes(x, y, pose = pose)) +
#'   geom_cat(size = 20, tail = "curl") +
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
  accessory = "none",
  mood = "none",
  silhouette = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  eyes <- match.arg(eyes)
  accessory <- match_accessory(accessory)
  mood <- match_mood(mood)
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
      accessory = accessory,
      mood = mood,
      silhouette = silhouette,
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
  non_missing_aes = c("size", "coat", "pose", "tail"),
  default_aes = aes(
    coat = "black",
    pose = "sitting",
    tail = "up",
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
    seed = 1,
    accessory = "none",
    mood = "none",
    silhouette = FALSE
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
        stroke = coords$size[i] * 0.11 * linewidth[i],
        pose = coords$pose[i],
        tail = coords$tail[i],
        accessory = accessory,
        mood = mood,
        silhouette = silhouette
      )
    })

    # Leave the gTree unnamed: grid only draws the first of several siblings
    # that share a name, so two geom_cat() layers would otherwise collapse
    # into one.
    grid::gTree(
      children = do.call(grid::gList, grobs)
    )
  }
)
