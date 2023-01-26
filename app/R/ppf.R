# ppf <- function(raw, neigh = 5, deriv = 10000, thresh = 0){
#   raw %>%
#     mutate(time.scale = round(time.scale, 2)) %>%
#     group_by(Species, time.scale) %>%
#     nest() %>%
#     mutate(
#       peak = map(
#         data,
#         ~peakpickR(
#           raw.data = .,
#           neighlim = neigh,
#           deriv.lim = deriv,
#           int.thresh = thresh
#         )
#       )
#     ) %>%
#     unnest(cols = peak) %>%
#     select(-data) %>%
#     as_tibble()
# }

ppf <- function(raw, neigh = 5, deriv = 10000, thresh = 0){
  
  raw %>%
    mutate(time.scale = round(time.scale, 2)) %>%
    dplyr::group_by(Species, time.scale) %>% 
    tidyr::nest() %>%
    mutate(
      peak = map(
        data,
        ~peakpickR(
          raw.data = .,
          neighlim = neigh,
          deriv.lim = deriv,
          int.thresh = thresh
        )
      )
    ) %>%
    tidyr::unnest(peak) %>%
    as_tibble()
  
}


