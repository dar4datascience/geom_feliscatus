# Render the images used by the Quarto website.
#
#   Rscript dev/site-images.R
#
# Output lands in website/images/.  Re-run this after changing the drawing
# code, then render the site with `quarto render website`.

library(ggplot2)
devtools::load_all(quiet = TRUE)

dir.create("website/images", recursive = TRUE, showWarnings = FALSE)

save_plot <- function(plot, file, width, height, dpi = 220) {
  ggsave(
    file.path("website/images", file),
    plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

blank <- function() {
  theme_void() + theme(legend.position = "none")
}

# Room at the edges so cats at the first and last position are not clipped.
pad_scales <- function(plot) {
  plot +
    scale_x_continuous(expand = expansion(mult = 0.14)) +
    scale_y_continuous(expand = expansion(mult = 0.14))
}

coats <- data.frame(
  x = seq_along(cat_coats()),
  y = 1,
  coat = cat_coats()
)

# Hero and coat portraits --------------------------------------------------

hero <- pad_scales(
  ggplot(coats, aes(x, y, coat = coat)) +
    geom_cat(size = 42) +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(hero, "hero.png", 12, 3.2)

for (coat in cat_coats()) {
  one <- ggplot(data.frame(x = 1, y = 1, coat = coat), aes(x, y, coat = coat)) +
    geom_cat(size = 58) +
    coord_cartesian(clip = "off") +
    blank()
  save_plot(one, paste0("coat-", coat, ".png"), 3, 3)
}

# Expression and rotation --------------------------------------------------

variants <- pad_scales(
  ggplot(coats, aes(x, y, coat = coat)) +
    geom_cat(
      size = c(26, 30, 26, 30, 26),
      angle = c(0, 15, -20, 30, 0),
      eyes = "closed",
      whiskers = FALSE
    ) +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(variants, "variants.png", 10, 3)

# Marking variation across seeds ------------------------------------------

seeds <- expand.grid(
  seed = 1:4,
  coat = c("calico", "tortoise"),
  stringsAsFactors = FALSE
)
seeds$x <- seeds$seed
seeds$y <- 1

markings <- pad_scales(
  ggplot(seeds, aes(x, y, coat = coat)) +
    facet_wrap(~coat, ncol = 1) +
    coord_cartesian(clip = "off") +
    blank()
)

for (s in 1:4) {
  markings <- markings +
    geom_cat(data = seeds[seeds$seed == s, ], size = 38, seed = s)
}

save_plot(markings, "seeds.png", 10, 4.8)

# Sizes --------------------------------------------------------------------

sizes <- data.frame(x = 1:5, y = 1, coat = "tuxedo")

size_plot <- pad_scales(
  ggplot(sizes, aes(x, y, coat = coat)) +
    geom_cat(size = c(8, 14, 20, 26, 32)) +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(size_plot, "sizes.png", 10, 2.6)

# Custom fur colours -------------------------------------------------------

pastels <- data.frame(
  x = 1:3,
  y = 1,
  fur = c("#f6d5e0", "#d5e8f6", "#e6f6d5")
)

colours <- pad_scales(
  ggplot(pastels, aes(x, y)) +
    geom_cat(coat = "white", fill = pastels$fur, size = 40) +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(colours, "colours.png", 8, 3)

# Legend -------------------------------------------------------------------

legend_plot <- pad_scales(
  ggplot(coats, aes(x, y, coat = coat)) +
    geom_cat(size = 20) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(legend.key.size = unit(9, "mm"))
)

save_plot(legend_plot, "legend.png", 8, 3.2)

legend_labels <- pad_scales(
  ggplot(coats, aes(x, y, coat = coat)) +
    geom_cat(size = 20) +
    scale_coat_discrete(name = "Coat", labels = toupper) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(legend.key.size = unit(9, "mm"))
)

save_plot(legend_labels, "legend-labels.png", 8, 3.2)

# Cats in a plot -----------------------------------------------------------

cars <- transform(mtcars[1:10, ], coat = rep(cat_coats(), length.out = 10))

scatter <- ggplot(cars, aes(wt, mpg, coat = coat, size = hp)) +
  geom_cat() +
  scale_size_area(max_size = 14) +
  theme_minimal()

save_plot(scatter, "scatter.png", 8, 5)

# Favicon ------------------------------------------------------------------

favicon <- ggplot(
  data.frame(x = 1, y = 1, coat = "black"),
  aes(x, y, coat = coat)
) +
  geom_cat(size = 18) +
  coord_cartesian(clip = "off") +
  blank()

save_plot(favicon, "favicon.png", 1, 1, dpi = 72)

message("wrote images to website/images/")
