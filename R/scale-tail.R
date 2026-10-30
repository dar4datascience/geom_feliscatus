#' Identity scale for the `tail` aesthetic
#'
#' Tail names are also the values the aesthetic takes, so the default scale
#' passes them through unchanged.  Use this scale to set a title, labels or
#' limits for a mapped `tail`.
#'
#' @param ... Arguments passed on to [ggplot2::scale_discrete_identity()],
#'   such as `name` or `labels`.
#' @param guide A guide specification.  Defaults to `"legend"`.
#'
#' @return A `ggplot2` scale.
#' @export
#' @examples
#' library(ggplot2)
#'
#' tails <- data.frame(x = 1:6, y = 1, tail = cat_tails())
#'
#' ggplot(tails, aes(x, y, tail = tail)) +
#'   geom_cat(size = 18) +
#'   scale_tail_discrete(labels = toupper) +
#'   coord_equal()
scale_tail_discrete <- function(..., guide = "legend") {
  scale_discrete_identity(aesthetics = "tail", ..., guide = guide)
}
