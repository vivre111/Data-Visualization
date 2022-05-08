library("glyphs")
context("make glyphs")

data <- list()
for (i in 1:9){
  data[[i]] <- rt(100, df = 10)
}
width=c(1,6,1,12,1)
height=c(1,1,4,1,3)

test_that("make_glyphs is in right form", {
  expect_match(class(make_glyphs(data)), "list")
  expect_match(class(make_glyphs(data, glyph_type = "Morton")), "list")
  expect_match(class(make_glyphs(data, glyph_type = "rectangle", width = width, height = height)), "list")
  expect_match(class(make_glyphs(data)[[1]]), "array")
  expect_match(class(make_glyphs(data, glyph_type = "Morton")[[1]]), "array")
  expect_match(class(make_glyphs(data, glyph_type = "rectangle", width = width, height = height)[[1]]), "array")
})

test_that("make_glyphs_draw in in right form", {
  expect_match(class(make_glyphs_draw(data, hist, type = "png", mar = rep(4, 4), width = 200, height = 200)), "list")
  expect_match(class(make_glyphs_draw(data, hist, type = "png", mar = rep(4, 4), width = 200, height = 200)[[1]]), "array")
  expect_match(class(make_glyphs_draw(data, hist, type = "jpeg", mar = rep(4, 4), width = 200, height = 200)), "list")
  expect_match(class(make_glyphs_draw(data, hist, type = "jpeg", mar = rep(4, 4), width = 200, height = 200)[[1]]), "array")
  expect_match(class(make_glyphs_draw(data, hist, type = "tiff", mar = rep(4, 4), width = 200, height = 200)), "list")
  expect_match(class(make_glyphs_draw(data, hist, type = "tiff", mar = rep(4, 4), width = 200, height = 200)[[1]]), "array")
  expect_match(class(make_glyphs_draw(data, hist, type = "pixmap", mar = rep(4, 4), width = 200, height = 200)), "list")
  expect_match(class(make_glyphs_draw(data, hist, type = "pixmap", mar = rep(4, 4), width = 200, height = 200)[[1]]), "array")
})
