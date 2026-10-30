#' Cat coats understood by [geom_cat()]
#'
#' `cat_coats()` lists the coats that [geom_cat()] knows how to draw.  The
#' names are also the values the `coat` aesthetic accepts, which makes the
#' `coat` scale an identity scale: the value you supply is the coat you get.
#'
#' @return A character vector of coat names.
#'
#' @export
#' @examples
#' cat_coats()
cat_coats <- function() {
  c("black", "white", "tuxedo", "calico", "tortoise")
}

coat_aliases <- c(
  tortoiseshell = "tortoise",
  tortie = "tortoise"
)

# Normalise a coat name, with a helpful error for unknown coats.
match_coat <- function(coat, call = caller_env()) {
  key <- tolower(trimws(as.character(coat)))
  key <- ifelse(key %in% names(coat_aliases), coat_aliases[key], key)
  unknown <- setdiff(unique(key), cat_coats())
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown cat coat{?s}: {.val {unknown}}.",
        "i" = "Supported coats are {.val {cat_coats()}}."
      ),
      call = call
    )
  }
  key
}

# Base colours, markings and eye colours for each coat.  Patches are blobs
# described in unit-disc coordinates; `coat_spec()` turns them into polygons.
coat_specs <- list(
  black = list(
    base = "#2b2b2b",
    eye = "#d8c24a",
    muzzle = "#4d4d4d",
    ear = "#e9a7b4",
    tail = NULL,
    paw = NULL,
    patches = list()
  ),
  white = list(
    base = "#f6f1e6",
    eye = "#8fb8d9",
    muzzle = "#ece4d4",
    ear = "#f2bcc6",
    tail = NULL,
    paw = NULL,
    patches = list()
  ),
  tuxedo = list(
    base = "#232323",
    eye = "#a8c94a",
    muzzle = "#f7f3ea",
    ear = "#e9a7b4",
    tail = NULL,
    paw = "#f7f3ea",
    patches = list(
      list(
        part = "body",
        colour = "#f7f3ea",
        centre = c(0, -0.40),
        radii = c(0.55, 0.72),
        angle = 0
      ),
      list(
        part = "body",
        colour = "#f7f3ea",
        centre = c(0, 0.55),
        radii = c(0.30, 0.25),
        angle = 0
      ),
      list(
        part = "head",
        colour = "#f7f3ea",
        centre = c(0, -0.85),
        radii = c(0.65, 0.40),
        angle = 0
      )
    )
  ),
  calico = list(
    base = "#f7f3ea",
    eye = "#c9a227",
    muzzle = "#fbf7ee",
    ear = "#f2bcc6",
    tail = "#d97a2b",
    paw = "#f7f3ea",
    patches = list(
      list(
        part = "head",
        colour = "#d97a2b",
        centre = c(-0.45, 0.25),
        radii = c(0.55, 0.55),
        angle = 0.3
      ),
      list(
        part = "head",
        colour = "#262626",
        centre = c(0.45, 0.10),
        radii = c(0.50, 0.60),
        angle = -0.2
      ),
      list(
        part = "body",
        colour = "#d97a2b",
        centre = c(0.35, -0.25),
        radii = c(0.50, 0.60),
        angle = -0.4
      ),
      list(
        part = "body",
        colour = "#262626",
        centre = c(-0.40, 0.15),
        radii = c(0.45, 0.50),
        angle = 0.2
      )
    )
  ),
  tortoise = list(
    base = "#7a4f2f",
    eye = "#a3b545",
    muzzle = "#a97b52",
    ear = "#d99a8f",
    tail = "#3a2418",
    paw = "#5c3a22",
    patches = list(
      list(
        part = "head",
        colour = "#3a2418",
        centre = c(-0.25, 0.10),
        radii = c(0.50, 0.55),
        angle = 0.4
      ),
      list(
        part = "head",
        colour = "#b5651d",
        centre = c(0.40, 0.30),
        radii = c(0.45, 0.40),
        angle = -0.3
      ),
      list(
        part = "body",
        colour = "#3a2418",
        centre = c(0.25, 0.05),
        radii = c(0.55, 0.55),
        angle = 0.5
      ),
      list(
        part = "body",
        colour = "#b5651d",
        centre = c(-0.30, -0.25),
        radii = c(0.50, 0.45),
        angle = -0.5
      ),
      list(
        part = "body",
        colour = "#3a2418",
        centre = c(-0.10, 0.55),
        radii = c(0.35, 0.30),
        angle = 0.1
      )
    )
  )
)

# A tiny deterministic generator, so markings vary per cat without touching
# the global random seed.  Returns `n` values in [0, 1).
coat_runif <- function(n, seed) {
  modulus <- 2147483647
  multiplier <- 48271
  state <- (as.numeric(seed)[[1]] %% modulus) + 1
  out <- numeric(n)
  for (i in seq_len(n)) {
    state <- (multiplier * state) %% modulus
    out[i] <- state / modulus
  }
  out
}

# An organic, wobbling blob in unit-disc coordinates.
coat_blob <- function(
  centre,
  radii,
  angle = 0,
  wobble = 0.18,
  seed = 1,
  n = 24
) {
  theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  r <- 1 + wobble * (coat_runif(n, seed) - 0.5) * 2
  x <- radii[1] * r * cos(theta)
  y <- radii[2] * r * sin(theta)
  cos_a <- cos(angle)
  sin_a <- sin(angle)
  x <- centre[1] + x * cos_a - y * sin_a
  y <- centre[2] + x * sin_a + y * cos_a
  # Keep every vertex inside the unit disc, which keeps patches inside the
  # body and head ellipses once they are scaled.
  dist <- sqrt(x^2 + y^2)
  if (max(dist) > 0.97) {
    x <- x * (0.97 / max(dist))
    y <- y * (0.97 / max(dist))
  }
  cbind(x, y)
}

# Resolve a coat to base colours plus polygon patches.
coat_spec <- function(coat, seed = 1) {
  spec <- coat_specs[[match_coat(coat)]]
  spec$patches <- lapply(seq_along(spec$patches), function(i) {
    patch <- spec$patches[[i]]
    jitter <- coat_runif(4, seed * 97 + i)
    angle <- patch$angle + (jitter[1] - 0.5) * 0.4
    scale <- 1 + (jitter[2] - 0.5) * 0.15
    offset <- c((jitter[3] - 0.5) * 0.12, (jitter[4] - 0.5) * 0.12)
    patch$poly <- coat_blob(
      centre = patch$centre + offset,
      radii = patch$radii * scale,
      angle = angle,
      seed = seed * 31 + i
    )
    patch
  })
  spec
}
