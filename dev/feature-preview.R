# Render the new poses, tails, coats, accessories and moods for review.
#
#   Rscript dev/feature-preview.R
#
# Output lands in dev/references/features/.

library(grid)
devtools::load_all(quiet = TRUE)

dir.create("dev/references/features", recursive = TRUE, showWarnings = FALSE)

sheet <- function(file, labels, cols, size = 34, title = NULL, ...) {
  rows <- ceiling(length(labels) / cols)
  width <- 2.2 * cols
  height <- 2.2 * rows + 0.6
  png(
    file.path("dev/references/features", file),
    width = width,
    height = height,
    units = "in",
    res = 200,
    bg = "white"
  )
  on.exit(dev.off())
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows + 1, cols, heights = unit(c(0.6, rep(2.2, rows)), "null"))))
  if (!is.null(title)) {
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:cols))
    grid.text(title, gp = gpar(fontsize = 18, fontface = "bold"))
    popViewport()
  }
  for (i in seq_along(labels)) {
    r <- (i - 1) %/% cols + 2
    c <- (i - 1) %% cols + 1
    pushViewport(viewport(layout.pos.row = r, layout.pos.col = c))
    grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
    pushViewport(viewport(width = unit(size, "mm"), height = unit(size, "mm"), y = 0.56))
    grid.draw(cat_grob(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(size, "mm"),
      height = unit(size, "mm"),
      stroke = size * 0.11,
      ...
    ))
    popViewport()
    grid.text(labels[i], y = 0.08, gp = gpar(fontsize = 11, fontface = "bold"))
    popViewport()
  }
  popViewport()
}

# Poses --------------------------------------------------------------------
for (pose in cat_poses()) {
  sheet(
    paste0("pose-", pose, ".png"),
    labels = pose,
    cols = 1,
    size = 70,
    title = "Pose",
    coat = "tuxedo",
    pose = pose,
    tail = "up"
  )
}

# All poses together -------------------------------------------------------
png("dev/references/features/poses.png", width = 9, height = 3.2, units = "in", res = 200, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 4)))
for (i in seq_along(cat_poses())) {
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = i))
  grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
  pushViewport(viewport(width = unit(38, "mm"), height = unit(38, "mm"), y = 0.55))
  grid.draw(cat_grob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(38, "mm"), height = unit(38, "mm"),
    stroke = 38 * 0.11, coat = "tuxedo", pose = cat_poses()[i]
  ))
  popViewport()
  grid.text(cat_poses()[i], y = 0.08, gp = gpar(fontsize = 12, fontface = "bold"))
  popViewport()
}
popViewport()
dev.off()

# Tails --------------------------------------------------------------------
png("dev/references/features/tails.png", width = 12, height = 2.8, units = "in", res = 200, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 6)))
for (i in seq_along(cat_tails())) {
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = i))
  grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
  pushViewport(viewport(width = unit(34, "mm"), height = unit(34, "mm"), y = 0.55))
  grid.draw(cat_grob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(34, "mm"), height = unit(34, "mm"),
    stroke = 34 * 0.11, coat = "tabby", tail = cat_tails()[i]
  ))
  popViewport()
  grid.text(cat_tails()[i], y = 0.08, gp = gpar(fontsize = 12, fontface = "bold"))
  popViewport()
}
popViewport()
dev.off()

# Coats --------------------------------------------------------------------
png("dev/references/features/coats.png", width = 14, height = 3, units = "in", res = 200, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 7)))
for (i in seq_along(cat_coats())) {
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = i))
  grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
  pushViewport(viewport(width = unit(34, "mm"), height = unit(34, "mm"), y = 0.55))
  grid.draw(cat_grob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(34, "mm"), height = unit(34, "mm"),
    stroke = 34 * 0.11, coat = cat_coats()[i]
  ))
  popViewport()
  grid.text(cat_coats()[i], y = 0.08, gp = gpar(fontsize = 12, fontface = "bold"))
  popViewport()
}
popViewport()
dev.off()

# Accessories --------------------------------------------------------------
acc <- setdiff(cat_accessories(), "none")
png("dev/references/features/accessories.png", width = 14, height = 4.6, units = "in", res = 200, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 5)))
for (i in seq_along(acc)) {
  pushViewport(viewport(layout.pos.row = (i - 1) %/% 5 + 1, layout.pos.col = (i - 1) %% 5 + 1))
  grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
  pushViewport(viewport(width = unit(32, "mm"), height = unit(32, "mm"), y = 0.55))
  grid.draw(cat_grob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(32, "mm"), height = unit(32, "mm"),
    stroke = 32 * 0.11, coat = "tabby", accessory = acc[i]
  ))
  popViewport()
  grid.text(acc[i], y = 0.08, gp = gpar(fontsize = 12, fontface = "bold"))
  popViewport()
}
popViewport()
dev.off()

# Moods --------------------------------------------------------------------
moods <- setdiff(cat_moods(), "none")
png("dev/references/features/moods.png", width = 14, height = 5.4, units = "in", res = 200, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4)))
for (i in seq_along(moods)) {
  pushViewport(viewport(layout.pos.row = (i - 1) %/% 4 + 1, layout.pos.col = (i - 1) %% 4 + 1))
  grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
  pushViewport(viewport(width = unit(32, "mm"), height = unit(32, "mm"), y = 0.55))
  grid.draw(cat_grob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(32, "mm"), height = unit(32, "mm"),
    stroke = 32 * 0.11, coat = "tuxedo", mood = moods[i]
  ))
  popViewport()
  grid.text(moods[i], y = 0.08, gp = gpar(fontsize = 12, fontface = "bold"))
  popViewport()
}
popViewport()
dev.off()

# Silhouette ---------------------------------------------------------------
png("dev/references/features/silhouette.png", width = 6, height = 3, units = "in", res = 200, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
for (i in 1:2) {
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = i))
  grid.rect(gp = gpar(fill = "#faf8f5", col = "#ece7e0"))
  pushViewport(viewport(width = unit(36, "mm"), height = unit(36, "mm"), y = 0.55))
  grid.draw(cat_grob(
    x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    width = unit(36, "mm"), height = unit(36, "mm"),
    stroke = 36 * 0.11, coat = "calico", silhouette = i == 2
  ))
  popViewport()
  grid.text(if (i == 1) "full" else "silhouette", y = 0.08, gp = gpar(fontsize = 12, fontface = "bold"))
  popViewport()
}
popViewport()
dev.off()

message("wrote feature previews to dev/references/features/")
