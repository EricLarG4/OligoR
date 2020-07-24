#' Peak picking function based on the peakPick package
#'
#' @param raw.data A data frame with an m/z column called \code{mz} and an intensity column called \code{int}.
#' @param neighlim An integer limit for how far apart peaks must be. Peak pairs closer than or equal to neighlim to each other have the lesser peak eliminated.
#' @param deriv.lim A numeric upper limit for the estimated derivative for a point to be considered for a peak call
#' @param int.thresh A numeric intensity threshold value, relative to a maximum intensity of 1, to discard low intensity false positives.
#' @return A dataframe with an additional column \code{peak} equal to 0 in absence of peak or the peak intensity if a peak is detected at that position
#' @examples
#' peakpicking(raw.data = a.data.frame, neighlim = 5, deriv.lim = 10000, int.thresh = 0.02)neighlim = 5, deriv.lim = 10000, int.thresh = 0.02

#
# raw.data <- read_xlsx(path = "C:/Users/Eric/Downloads/23TAG-binom-6.xlsx") %>%
#   magrittr::set_colnames(c('mz', 'int'))
#
# pp <- peakpicking(raw.data = raw.data)


peakpicking <- function(raw.data, neighlim = 5, deriv.lim = 10000, int.thresh = 0.02) {

  library("peakPick")

  pp <- raw.data %>%
    add_column(
      peak = peakPick::peakpick(
        matrix(raw.data$int %>%
                 magrittr::set_colnames(NULL),
               ncol = 1),
        neighlim = neighlim, #decrease when peaks are closer (higher z)
        deriv.lim = deriv.lim)
    ) %>%
    magrittr::set_colnames(c('mz', 'int', 'peak')) %>%
    mutate(
      peak = if_else(
        peak == TRUE & int > int.thresh*max(int),
        int,
        0
      )
    )

  return(pp)

}







# library(tidyverse)
# library(readxl)


# raw.data <- read_xlsx(path = "C:/Users/Eric/Downloads/23TAG-binom-6.xlsx") %>%
#   magrittr::set_colnames(c('mz', 'int'))  #USE FILTERS FOR NEW DATA
#   # filter(mz>1537.5 & mz<1542.5) %>%
#   # mutate(int = int/max(int))
#
# # writexl::write_xlsx(raw.data, #WRITE SMALLER FILES AFTER FILTER USE
#                     # path = "C:/Users/Eric/Downloads/msinput2.xlsx")
#
#
# pp <- raw.data %>%
#   add_column(
#   peak = peakPick::peakpick(
#     matrix(raw.data$int %>%
#              magrittr::set_colnames(NULL),
#            ncol = 1),
#     neighlim = 5, #decrease when peaks are closer (higher z)
#     deriv.lim = 10000)
# ) %>%
#   magrittr::set_colnames(c('mz', 'int', 'peak')) %>%
#   mutate(
#     peak = if_else(
#       peak == TRUE & int > 0.02*max(int),
#       int,
#         0
#     )
#   )
#
# p.pp <- ggplot() +
#   geom_line(data = pp,
#              aes(x = mz, y = int),
#              color = 'grey',
#              inherit.aes = F) +
#   geom_point(data = pp %>%
#                filter(peak > 0),
#              aes(mz, int, color = peak),
#              size = 3,
#              inherit.aes = F)
#
# p.pp
#
#
# distri <- pp %>%
#   filter(peak > 0)
#
# p.distri <- distri %>%
#   ggplot(aes(mz, int)) +
#   geom_point()
#
# p.distri


