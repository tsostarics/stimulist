test_that("counterbalance with mismatching manipulation and ordering index", {
  expect_equal(new_design("cb") %>%
                 add_manipulations(contour = 1:2,
                                   position = 1:3,
                                   context = 1:2) %>%
                 add_items(critical = 12) %>%
                 present_n_of(position, 2) %>%
                 add_stimuli_by(position*contour*context ~ audio_file,
                                context ~ context_text) %>%
                 counterbalance() %>%
                 .$counterbalance,
               c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L,
                 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L,
                 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L,
                 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L,
                 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L,
                 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L,
                 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L, 6L,
                 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
                 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,
                 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L,
                 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L),
               ignore_attr = TRUE)
})

test_that("2x2 counterbalancing works", {
  expect_equal(new_design("test") %>%
                 add_manipulations(contour = c('hll','lll'),
                                   context = c('in','out')) %>%
                 add_items(critical = 2,
                           filler = 2) %>%
                 present_n_of(contour, 2) %>%
                 add_stimuli_by(contour*context ~ audio_file,
                                context ~ premable_text,
                                ~ center_audio) %>%
                 counterbalance() %>%
                 .$counterbalance,
               c(1L, 2L, 3L, 4L, 2L, 3L, 4L, 1L, 3L, 4L, 1L, 2L, 4L, 1L, 2L, 3L),
               ignore_attr = TRUE
  )
})
