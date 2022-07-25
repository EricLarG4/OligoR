opt.f <- function(opt.0, seq, massr, peaks=72){

  opt.0 %>%
    select(-data) %>%
    mutate(distrib.group = distrib) %>%
    group_by(Species, time.scale) %>%
    mutate(
      #F statistic
      #how much of the variance cannot be explained by monomodal compared to bimodal
      F.test = ((RSS[distrib == 'mono']-RSS[distrib=='bi'])/RSS[distrib=='bi'])/
        ((df[distrib == 'mono']-df[distrib=='bi'])/df[distrib=='bi']),
      #p value of the F statistic
      #null hypothesis: the bimodal model does not explain the variance
      #alternate hypothesis: the bimodal model does a better job than the monomodal model
      p.F = pf(F.test, df[distrib == 'mono'], df[distrib == 'bi'], lower.tail = FALSE)
    ) %>%
    group_by(time.scale, Species, distrib.group) %>%
    nest() %>%
    mutate(
      theo = map(
        data,
        ~distributR(
          opt.data = .,
          seq = seq,
          massr = massr,
          peaks = peaks)
      )
    ) %>%
    unnest(theo) %>%
    unnest(data) %>%
    #sort population (DC1: low mass, DC2: high mass)
    mutate(
      DCX1 = DC1, DCX2 = DC2,
      DC1 = if_else(distrib == 'bi' & DCX2 < DCX1, DC2, DC1),
      DC2 = if_else(distrib == 'bi' & DCX2 < DCX1, DCX1, DC2),
      abX = ab1,
      ab1 = if_else(distrib == 'bi' & DCX2 < DCX1, ab2, ab1),
      ab2 = if_else(distrib == 'bi' & DCX2 < DCX1, abX, ab2),
      centroid.X = centroid.1,
      centroid.1 = if_else(distrib == 'bi' & DCX2 < DCX1, centroid.2, centroid.1),
      centroid.2 = if_else(distrib == 'bi' & DCX2 < DCX1, centroid.X, centroid.2),
      iso.X = iso.1,
      iso.1 = if_else(distrib == 'bi' & DCX2 < DCX1, iso.2, iso.1),
      iso.2 = if_else(distrib == 'bi' & DCX2 < DCX1, iso.X, iso.2)
    ) %>%
    select(-c(DCX2, DCX1, abX, centroid.X, iso.X)) %>%
    ungroup()

}
