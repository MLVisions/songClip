EXAMPLE_MP3 <- system.file(file.path("examples", "flowers.mp3"), package = "songClip", mustWork = TRUE)


download_youtube <- function(url = "https://youtu.be/F8Zt3mYlOqU", filename = "temp.mp4"){
  checkmate::assert_true(fs::path_ext(filename) == "mp4")

  # You need youtube-dl: https://ytdl-org.github.io/youtube-dl/download.html
  # MacOS: brew install youtube-dl
  # Replace this with R/python package that does not require this
  system(glue::glue("youtube-dl {url} -o {filename}"))
}



#' Plot the audio bins (looks cool)
#'
#' @param mp3_file file path to an MP3 file
#'
#' @details
#' This takes a while for long videos
plot_mp3 <- function(mp3_file){
  pcm_data <- av::read_audio_bin(mp3_file)
  plot(pcm_data, type = 'l')
}


spec_mp3 <- function(mp3_file){
  media_info <- av::av_media_info(mp3_file)
  crop_limits <- c(0, media_info$duration)

  audio_info <- c(media_info, crop_limits = list(crop_limits))
  return(audio_info)
}



#' Crop mp3 file to specified limits
#'
#' @param audio a `tuneR` audio object
#' @param limits vector specifying the start and end time
#'
#' @keywords internal
crop_mp3 <- function(audio, limits = c(0, 10)){

  # Convert time limits to sample indices
  start_sample <- round(limits[1] * audio@samp.rate)
  end_sample <- round(limits[2] * audio@samp.rate)

  # Crop audio by sample indices
  cropped_audio <- audio[start_sample:end_sample]

  return(cropped_audio)
}


#' Speed up or slow down mp3 file
#'
#' @param audio a `tuneR` audio object
#' @param speed multiplier of song frequency
#'
#' @keywords internal
mp3_speed <- function(audio, speed = 2){
  # Multiply sampling rate by speed
  audio@samp.rate <- audio@samp.rate * speed

  return(audio)
}
