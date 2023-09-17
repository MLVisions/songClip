
testthat::test_that("plot_wave_audio", {

  audio_obj <- tuneR::readMP3(file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3"))

  pl <- plot_wave_audio(audio_obj)
  testthat::expect_true(inherits(pl, "plotly"))
})
