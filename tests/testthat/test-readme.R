test_that("the README quick start runs through the public API", {
  script <- system.file("examples", "getting-started.R", package = "rcards")
  code <- readLines(script)
  readme_path <- testthat::test_path("..", "..", "README.md")
  if (file.exists(readme_path)) {
    readme <- readLines(readme_path)
    heading <- match("## Quick start", readme)
    start <- which(seq_along(readme) > heading & readme == "```r")[1]
    end <- which(seq_along(readme) > start & readme == "```")[1]
    expect_identical(readme[seq.int(start + 1L, end - 1L)], code)
  }
  e <- new.env(parent = baseenv())
  expect_output(eval(parse(text = code), e), "Alice|Player")
  expect_length(e$returned$deck, 49)
  expect_length(e$returned$player$hand, 3)
})
