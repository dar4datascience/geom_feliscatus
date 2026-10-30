#' Identity scale for the `pose` aesthetic
#'
#' Pose names are also the values the aesthetic takes, so the default scale
#' passes them through unchanged.  Use this scale to set a title, labels or
#' limits for a mapped `pose`.
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
#' poses <- data.frame(x = 1:4, y = 1, pose = cat_poses())
#'
#' ggplot(poses, aes(x, y, pose = pose)) +
#'   geom_cat(size = 18) +
#'   scale_pose_discrete(labels = toupper) +
#'   coord_equal()
scale_pose_discrete <- function(..., guide = "legend") {
  scale_discrete_identity(aesthetics = "pose", ..., guide = guide)
}
