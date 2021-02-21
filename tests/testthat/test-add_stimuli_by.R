test_that("create grid works", {
  expect_equal(new_design("test") %>%
                 add_manipulations(contour = c('hll','lll'),
                                   context = c('in','out')) %>%
                 add_items(critical = 12,
                           filler = 8) %>%
                 present_n_of(contour, 2) %>%
                 .create_grid("contour * context"),
               data.frame(
                 stringsAsFactors = TRUE,
                 contour = c("hll", "lll", "hll", "lll"),
                 context = c("in", "in", "out", "out"),
                 tojoin = c(1L, 1L, 1L, 1L)
               ), ignore_attr = T
  )
})

test_that("cross manipulations works", {
  expect_equal(new_design("test") %>%
                 add_manipulations(contour = c('hll','lll'),
                                   context = c('in','out')) %>%
                 add_items(critical = 12,
                           filler = 8) %>%
                 present_n_of(contour, 2) %>%
                 .cross_manipulations('contour x context'),
               data.frame(
                 stringsAsFactors = FALSE,
                 tojoin = c(1L, 1L, 1L, 1L),
                 context = c("in", "in", "out", "out"),
                 contour_1 = c("hll","lll",
                               "hll","lll"),
                 contour_2 = c("lll","hll",
                               "lll","hll")
               ), ignore_attr = T
  )
}
)

test_that("make presentation works", {
  expect_equal(new_design("test") %>%
                 add_manipulations(contour = c('hll','lll'),
                                   context = c('in','out')) %>%
                 add_items(critical = 12,
                           filler = 8) %>%
                 present_n_of(contour, 2) %>%
                 .make_presentation('contour','audio_file'),
               data.frame(
                 stringsAsFactors = FALSE,
                 contour_1 = c("hll", "lll"),
                 contour_2 = c("lll", "hll"),
                 audio_file_1 = c(NA, NA),
                 audio_file_2 = c(NA, NA),
                 tojoin = c(1L, 1L)
               ))
})


test_that("make crossed presentation works", {
  expect_equal(new_design("test") %>%
                 add_manipulations(contour = c('hll','lll'),
                                   context = c('in','out')) %>%
                 add_items(critical = 12,
                           filler = 8) %>%
                 present_n_of(contour, 2) %>%
                 .make_crossed_presentation("contour x context", "audio_file"),
               data.frame(
                 stringsAsFactors = FALSE,
                 tojoin = c(1L, 1L, 1L, 1L),
                 context = c("in", "in", "out", "out"),
                 contour_1 = c("hll", "lll", "hll", "lll"),
                 contour_2 = c("lll", "hll", "lll", "hll"),
                 audio_file_1 = c(NA, NA, NA, NA),
                 audio_file_2 = c(NA, NA, NA, NA)
               )
  )
})
