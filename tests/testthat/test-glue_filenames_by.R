test_that("expand glue works", {
  expect_equal(.expand_glue("sound/{stimulus}_{contour}_{context}", c('contour','context'), c("audio_file_1", "audio_file_2"), F),
               c("sound/{stimulus}_{contour_1}_{context_1}", "sound/{stimulus}_{contour_2}_{context_2}"))
  expect_equal(.expand_glue("sound/{stimulus}_{contour}_{context}", 'contour', c("audio_file_1", "audio_file_2"), F),
               c("sound/{stimulus}_{contour_1}_{context}", "sound/{stimulus}_{contour_2}_{context}"))
})
