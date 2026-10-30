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
  ggplot(coats[1:5, ], aes(x, y, coat = coat)) +
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

# Poses --------------------------------------------------------------------

for (pose in cat_poses()) {
  one <- ggplot(
    data.frame(x = 1, y = 1, pose = pose),
    aes(x, y, pose = pose)
  ) +
    geom_cat(size = 58, tail = "curl") +
    coord_cartesian(clip = "off") +
    blank()
  save_plot(one, paste0("pose-", pose, ".png"), 3, 3)
}

poses <- data.frame(x = seq_along(cat_poses()), y = 1, pose = cat_poses())

poses_plot <- pad_scales(
  ggplot(poses, aes(x, y, pose = pose)) +
    geom_cat(size = 34, tail = "curl") +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(poses_plot, "poses.png", 10, 3)

# Tails --------------------------------------------------------------------

for (tail in cat_tails()) {
  one <- ggplot(
    data.frame(x = 1, y = 1, tail = tail),
    aes(x, y, tail = tail)
  ) +
    geom_cat(size = 58, coat = "tabby") +
    coord_cartesian(clip = "off") +
    blank()
  save_plot(one, paste0("tail-", tail, ".png"), 3, 3)
}

tails <- data.frame(x = seq_along(cat_tails()), y = 1, tail = cat_tails())

tails_plot <- pad_scales(
  ggplot(tails, aes(x, y, tail = tail)) +
    geom_cat(size = 34, coat = "tabby") +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(tails_plot, "tails.png", 12, 3)

# Accessories --------------------------------------------------------------

accessories <- setdiff(cat_accessories(), "none")

for (accessory in accessories) {
  one <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_cat(size = 58, coat = "tabby", accessory = accessory) +
    coord_cartesian(clip = "off") +
    blank()
  save_plot(one, paste0("accessory-", accessory, ".png"), 3, 3)
}

accessory_data <- data.frame(x = seq_along(accessories), y = 1)

accessory_plot <- pad_scales(
  ggplot(accessory_data, aes(x, y)) +
    coord_cartesian(clip = "off") +
    blank()
)

for (i in seq_along(accessories)) {
  accessory_plot <- accessory_plot +
    geom_cat(
      data = accessory_data[i, , drop = FALSE],
      size = 24,
      coat = "tabby",
      accessory = accessories[i]
    )
}

save_plot(accessory_plot, "accessories.png", 14, 3)

# Moods --------------------------------------------------------------------

moods <- setdiff(cat_moods(), "none")

for (mood in moods) {
  one <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_cat(size = 58, coat = "tuxedo", mood = mood) +
    coord_cartesian(clip = "off") +
    blank()
  save_plot(one, paste0("mood-", mood, ".png"), 3, 3)
}

mood_data <- data.frame(x = seq_along(moods), y = 1)

mood_plot <- pad_scales(
  ggplot(mood_data, aes(x, y)) +
    coord_cartesian(clip = "off") +
    blank()
)

for (i in seq_along(moods)) {
  mood_plot <- mood_plot +
    geom_cat(
      data = mood_data[i, , drop = FALSE],
      size = 22,
      coat = "tuxedo",
      mood = moods[i]
    )
}

save_plot(mood_plot, "moods.png", 14, 4)

# Silhouette ---------------------------------------------------------------

silhouette <- data.frame(x = 1:2, y = 1)

silhouette_plot <- pad_scales(
  ggplot(silhouette, aes(x, y)) +
    geom_cat(data = silhouette[1, , drop = FALSE], coat = "calico", size = 40) +
    geom_cat(
      data = silhouette[2, , drop = FALSE],
      coat = "calico",
      size = 40,
      silhouette = TRUE
    ) +
    coord_cartesian(clip = "off") +
    blank()
)

save_plot(silhouette_plot, "silhouette.png", 6, 3)

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
