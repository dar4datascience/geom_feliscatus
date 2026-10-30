#' Identity scale for the `coat` aesthetic
#'
#' Coat names are also the values the aesthetic takes, so the default scale
#' passes them through unchanged.  Use this scale to set a title, labels or
#' limits for a mapped `coat`.
#'
#' @param ... Arguments passed on to [ggplot2::scale_discrete_identity()],
#'   such as `name` or `labels`.
#' @param guide A guide specification.  Defaults to `"legend"`.
#'
#' @return A `ggplot2` scale.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' cats <- data.frame(x = 1:5, y = 1, coat = cat_coats())
#'
#' ggplot(cats, aes(x, y, coat = coat)) +
#'   geom_cat(size = 14) +
#'   scale_coat_discrete(labels = toupper) +
#'   coord_equal()
scale_coat_discrete <- function(..., guide = "legend") {
  scale_discrete_identity(aesthetics = "coat", ..., guide = guide)
}
