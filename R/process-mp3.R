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


#' Get duration of audio file
#'
#' @param audio_obj a `tuneR` audio object
#'
#' @returns a vector of `c(0, duration)`
#'
#' @keywords internal
get_audio_dur <- function(audio_obj){
  # Calculate duration
  duration <- length(audio_obj@left) / audio_obj@samp.rate

  # Round -down- to two decimal places
  duration <- floor(duration * 100) / 100

  return(c(0, duration))
}



#' Crop mp3 file to specified limits
#'
#' @inheritParams get_audio_dur
#' @param limits vector specifying the start and end time
#'
#' @keywords internal
crop_mp3 <- function(audio_obj, limits = c(0, 10)){

  # Convert time limits to sample indices
  start_sample <- round(limits[1] * audio_obj@samp.rate)
  end_sample <- round(limits[2] * audio_obj@samp.rate)

  # Crop audio by sample indices
  cropped_audio <- audio_obj[start_sample:end_sample]

  return(cropped_audio)
}


#' Speed up or slow down mp3 file
#'
#' @inheritParams get_audio_dur
#' @param speed_factor multiplier of song frequency
#'
#' @keywords internal
adjust_audio_speed <- function(audio_obj, speed_factor = 2){

  # Extract the audio data
  audio_data <- audio_obj@left

  # Multiply sampling rate by speed
  new_sample_rate <- audio_obj@samp.rate * speed_factor

  # Calculate the number of samples in the adjusted audio
  num_samples <- length(audio_data)

  # Create a time vector for the adjusted audio
  adjusted_time <- seq(0, (num_samples - 1) / new_sample_rate, 1 / new_sample_rate)

  # Interpolate the audio data to adjust speed
  adjusted_audio_data <- approx(x = adjusted_time, y = audio_data, xout = seq(0, max(adjusted_time), 1 / audio_obj@samp.rate))$y

  # Create a new Wave object with adjusted audio data and sampling rate
  adjusted_audio_obj <- tuneR::Wave(adjusted_audio_data, samp.rate = new_sample_rate)


  return(adjusted_audio_obj)
}
