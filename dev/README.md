# Development scaffold

This directory holds the local loop used to build and check the geom. It is
not part of the installed package (see `.Rbuildignore`).

## The loop

```r
# 1. Load the package without installing it
devtools::load_all()

# 2. Look at the cats
source("dev/preview.R")   # writes dev/output/*.png

# 3. Change something in R/, go back to 1

# 4. Run the tests
devtools::test()

# 5. Update documentation and NAMESPACE
devtools::document()

# 6. Full check
devtools::check()
```

`devtools::load_all()` attaches the package, which matters here: ggplot2
finds `scale_coat_discrete()` on the search path when the `coat` aesthetic
is mapped. Without it, mapping `coat` fails because ggplot2 cannot find a
default scale.

## Where things live

| Path | What it is |
|:--|:--|
| `R/geom-cat.R` | `GeomCat`, `geom_cat()` |
| `R/draw-cat.R` | `cat_grob()`, `draw_key_cat()`, grid helpers |
| `R/cat-coats.R` | coat palettes, markings, deterministic variation |
| `R/scale-coat.R` | `scale_coat_discrete()` |
| `tests/testthat/` | unit tests for the data, the layer and the scale |
| `vignettes/` | the user guide and the extension guide |
| `inst/templates/geom-template.R` | scaffold for a new geom |

## Adding a new geom

```r
usethis::use_template(
  "geom-template.R",
  save_as = "R/geom-mything.R",
  package = "geomfeliscatus"
)
```

Then follow the checklist at the bottom of the template. The short version:

1. Declare the Geom: `required_aes`, `default_aes`, `extra_params`,
   `draw_key`, `draw_panel`.
2. Draw one grob per row in `draw_panel()`, after
   `coord$transform(data, panel_params)`.
3. Draw the legend key in a 0-1 viewport with `draw_key`.
4. Write the `geom_*()` wrapper with `layer()`.
5. If you add an aesthetic, add its default scale
   (`scale_<aesthetic>_discrete()` or `_continuous()`).
6. Test data, drawing, legend and errors; document; `check()`.

## Rendering cats while you iterate

`dev/preview.R` writes four images:

- `coats.png` - all five coats at a readable size
- `big.png` - the same, large, for detail
- `legend.png` - the legend keys
- `variants.png` - rotation and closed eyes

Open them with any image viewer; the script prints the directory when it
finishes.
