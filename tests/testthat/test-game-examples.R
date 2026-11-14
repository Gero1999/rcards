test_that("installed game examples execute through exported APIs", {
  for (name in c("holdem.R","mus.R","mus-bots.R")) {
    path <- system.file("examples",name,package="rcards")
    expect_true(nzchar(path))
    e <- new.env(parent=globalenv())
    expect_error(source(path,local=e),NA)
  }
})
