## ---- peakpickR

#' Peak picking function based on the peakPick package
#'
#' @param raw.data A data frame with an m/z column called \code{mz} and an intensity column called \code{intensum}.
#' @param neighlim An integer limit for how far apart peaks must be. Peak pairs closer than or equal to neighlim to each other have the lesser peak eliminated.
#' @param deriv.lim A numeric upper limit for the estimated derivative for a point to be considered for a peak call
#' @param int.thresh A numeric intensity threshold value, relative to a maximum intensity of 1, to discard low intensity false positives.
#' @return A dataframe with an additional column \code{peak} equal to 0 in absence of peak or the peak intensity if a peak is detected at that position
#' @examples
#' peakpickR(raw.data = raw.data,neighlim = 5,deriv.lim = 10000,int.thresh = 0.00000)




peakpickR <- function(raw.data, neighlim = 5, deriv.lim = 10000, int.thresh = 0.02) {

  library('tidyverse')
  # library("peakPick")
  source('R/peakpicking.R')

  unpicked <- data.frame(raw.data)

  pp <- unpicked %>%
    # group_by(time.scale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
    mutate( #applies pickpicking function from peakPick package
      peak = peakpick(
        matrix(.$intensum %>% #prepare input data as matrix
                 magrittr::set_colnames(NULL),
               ncol = 1),
        neighlim = neighlim, #decrease when peaks are closer (higher z)
        deriv.lim = deriv.lim)
    ) %>%
    # magrittr::set_colnames(c('mz', 'int', 'Species', 'time.scale', 'peak')) %>%
    ungroup() %>%
    mutate( #removes peak below intensity threshold
      peak = if_else(
        peak == TRUE & intensum > int.thresh*max(intensum),
        intensum,
        0
      )
    )

  # return(pp$peak)

}

