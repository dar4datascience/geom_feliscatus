# geomfeliscatus

`geomfeliscatus` is a small, complete example of extending ggplot2 with a
new geom. It draws cats.

Cats come in five coats: **black**, **white**, **tuxedo**, **calico** and
**tortoise**. The `coat` aesthetic is an identity scale, so the value you
supply is the cat you get, whether you fix it in the layer or map it from
your data.

## Installation

```r
# install.packages("pak")
pak::pak("dar4datascience/geom_feliscatus")
```

## A first cat

```r
library(ggplot2)
library(geomfeliscatus)

ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
  geom_cat(coat = "tuxedo", size = 30) +
  coord_equal() +
  theme_void()
```

## The five coats

```r
cats <- data.frame(x = 1:5, y = 1, coat = cat_coats())

ggplot(cats, aes(x, y, coat = coat)) +
  geom_cat(size = 22) +
  coord_equal() +
  theme_void()
```

![Five cats, one in each coat: black, white, tuxedo, calico and tortoise.](man/figures/README-cats.png)

| Coat | Description |
|:--|:--|
| `"black"` | Solid black fur with amber eyes. |
| `"white"` | Cream-white fur with blue eyes. |
| `"tuxedo"` | Black with a white bib, chin and paws. |
| `"calico"` | White with orange and black patches. |
| `"tortoise"` | Brown with dark and ginger mottling (also `"tortoiseshell"`). |

## Aesthetics

`geom_cat()` understands `x`, `y`, `coat`, `size` (the height of the cat in
mm), `colour`, `fill`, `alpha`, `linewidth` and `angle`. Layer parameters
`eyes` (`"open"` or `"closed"`), `whiskers` and `seed` control expression
and the markings of calico and tortoise cats.

```r
ggplot(cats, aes(x, y, coat = coat)) +
  geom_cat(size = 20, angle = c(0, 15, -20, 30, 0), eyes = "closed") +
  scale_coat_discrete(labels = toupper) +
  coord_equal() +
  theme_void()
```

## Learning to extend ggplot2

The package is written to be read. The
[vignette](vignettes/extending-ggplot2.Rmd) walks through the layer from
`ggproto()` to `draw_panel()`, the legend key, the user-facing function and
the custom scale.

The repository also ships a scaffold for your own geoms:

- `dev/preview.R` renders every coat to `dev/output/` for a quick visual
  check while you work.
- `inst/templates/geom-template.R` is a commented Geom + Stat + scale
  template.
- `dev/README.md` describes the edit, preview, test, document loop.

```r
usethis::use_template(
  "geom-template.R",
  save_as = "R/geom-mything.R",
  package = "geomfeliscatus"
)
```

## Further reading

- [How geom_cat() works](vignettes/extending-ggplot2.Rmd), a guided tour of
  the code in this package.
- [ggplot2: extending ggplot2](https://ggplot2-book.org/ext-ggplot2.html).
- [Easy geom recipes](https://evamaerey.github.io/easy-geom-recipes/).
- The [ggplot2 extenders club](https://ggplot2-extenders.github.io/ggplot-extension-club/).
