test_that("cat_coats() lists the five coats", {
  expect_equal(
    cat_coats(),
    c("black", "white", "tuxedo", "calico", "tortoise")
  )
})

test_that("coat aliases are normalised", {
  expect_equal(match_coat("tortoiseshell"), "tortoise")
  expect_equal(match_coat(" Tortie "), "tortoise")
  expect_equal(match_coat(c("black", "white")), c("black", "white"))
})

test_that("unknown coats are an error", {
  expect_snapshot(error = TRUE, match_coat("dalmatian"))
})

test_that("each coat has the expected number of markings", {
  expect_length(coat_spec("black")$patches, 0)
  expect_length(coat_spec("white")$patches, 0)
  expect_length(coat_spec("tuxedo")$patches, 3)
  expect_length(coat_spec("calico")$patches, 4)
  expect_length(coat_spec("tortoise")$patches, 5)
})

test_that("markings stay inside the body and head", {
  for (coat in cat_coats()) {
    for (patch in coat_spec(coat, seed = 3)$patches) {
      expect_lte(max(sqrt(rowSums(patch$poly^2))), 0.9701)
    }
  }
})

test_that("markings are deterministic per seed", {
  expect_identical(
    coat_spec("calico", seed = 2)$patches[[1]]$poly,
    coat_spec("calico", seed = 2)$patches[[1]]$poly
  )
  expect_false(identical(
    coat_spec("calico", seed = 2)$patches[[1]]$poly,
    coat_spec("calico", seed = 3)$patches[[1]]$poly
  ))
})
