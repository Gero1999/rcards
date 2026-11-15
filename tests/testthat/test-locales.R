test_that("locales validate and fall back without changing user values", {
  expect_equal(supported_languages(),c("en","es","de","it"))
  expect_equal(console_language(),"en")
  expect_error(console_language("xx"),"Supported languages")
  expect_equal(ui("unknown %s", "it", "Alice"),"unknown Alice")
})
