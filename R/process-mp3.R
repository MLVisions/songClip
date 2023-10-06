


download_youtube <- function(url = "https://youtu.be/F8Zt3mYlOqU", filename = "temp.mp4"){
  checkmate::assert_true(fs::path_ext(filename) == "mp4")

  # You need youtube-dl: https://ytdl-org.github.io/youtube-dl/download.html
  # MacOS: brew install youtube-dl
  # Replace this with R/python package that does not require this
  system(glue::glue("youtube-dl {url} -o {filename}"))
}



#' Get duration and limits of audio file
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


#' Get y-axis limits of audio wave channel
#' @rdname get_audio_dur
#'
#'
#' @returns a vector y-axis limits
#' @keywords internal
get_audio_limits <- function(audio_obj){
  ylim <- range(audio_obj@left, audio_obj@right)
  if(audio_obj@bit == 8)
    ylim <- c(-1, 1) * max(abs(ylim - 127)) + 127
  else
    ylim <- c(-1, 1) * max(abs(ylim))

  return(ylim)
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

  # Resample the audio data to adjust speed and pitch
  adjusted_audio_data <- signal::resample(audio_data, p = speed_factor)

  # Create a new Wave object with adjusted audio data and sampling rate
  adjusted_audio_obj <- tuneR::Wave(adjusted_audio_data, samp.rate = new_sample_rate, bit = audio_obj@bit)

  # Normalize the adjusted audio
  adjusted_audio_obj_norm <- tuneR::normalize(adjusted_audio_obj, unit = as.character(audio_obj@bit))

  return(adjusted_audio_obj_norm)
}
