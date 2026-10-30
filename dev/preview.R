# Quick visual check while developing the cat grob.
#
#   Rscript dev/preview.R
#
# Output lands in dev/output/.

library(ggplot2)
devtools::load_all(quiet = TRUE)

dir.create("dev/output", showWarnings = FALSE)

save_cats <- function(plot, file, width, height) {
  ggsave(
    file.path("dev/output", file),
    plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "white"
  )
}

cats <- data.frame(
  x = seq_along(cat_coats()),
  y = 1,
  coat = cat_coats()
)

p <- ggplot(cats, aes(x, y, coat = coat)) +
  geom_cat(size = 26) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(legend.position = "none")

save_cats(p, "coats.png", 10, 2.6)

p2 <- ggplot(cats, aes(x, y, coat = coat)) +
  geom_cat(size = 60) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(legend.position = "none")

save_cats(p2, "big.png", 13, 3.2)

p3 <- ggplot(cats, aes(x, y, coat = coat)) +
  geom_cat(size = 20) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(legend.position = "right")

save_cats(p3, "legend.png", 12, 2.6)

p4 <- ggplot(cats, aes(x, y, coat = coat)) +
  geom_cat(size = 26, angle = c(0, 15, -20, 30, 0), eyes = "closed") +
  coord_equal(clip = "off") +
  theme_void() +
  theme(legend.position = "none")

save_cats(p4, "variants.png", 10, 2.6)

message("wrote previews to dev/output/")
