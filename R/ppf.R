ppf <- function(raw, neigh = 5, deriv = 10000, thresh = 0){
  raw %>%
    mutate(colorscale = round(colorscale, 2)) %>%
    group_by(Species, colorscale) %>%
    nest() %>%
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
    unnest(cols = peak) %>%
    select(-data) %>%
    as_tibble()
}
