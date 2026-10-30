# geomfeliscatus 0.0.0.9000

* Initial development version.
* `geom_cat()` draws cats in seven coats: black, white, tuxedo, calico,
  tortoise, tabby and siamese.
* New `pose` aesthetic with four bodies: sitting, loaf, curled and standing.
* New `tail` aesthetic with six shapes: up, curl, plume, kink, bob and none.
* New `accessory` parameter: collar, bowtie, party hat, wizard hat, scarf,
  glasses, headphones, crown and Santa hat.
* New `mood` parameter: zzz, heart, sparkle, anger, sweat, music, fish, yarn,
  paw prints, shadow and box.
* New `silhouette` parameter draws the cat as a single solid shape.
* The package now uses a sticker identity: a white die-cut border behind the
  outline, so cats read on light and dark backgrounds.
* `cat_poses()`, `cat_tails()`, `cat_accessories()` and `cat_moods()` list
  the supported values.
* `scale_pose_discrete()` and `scale_tail_discrete()` provide identity scales
  for the new aesthetics.
* `geom_cat()` now draws every layer when the geom is added to a plot more
  than once.
* `cat_coats()` lists the supported coats.
* `scale_coat_discrete()` provides the identity scale for the `coat`
  aesthetic.
* `GeomCat` is exported for reuse by other packages.
