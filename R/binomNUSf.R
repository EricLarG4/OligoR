binom.NUS.f <- function(opt, seq, DC.init = 90, DC.final = 9, ref = 0){

  load("data/listMass.Rda")

  opt %>%
    select(
      colorscale, Species, distrib,
      DC1, DC2, ab1, ab2, centroid, centroid.1, centroid.2,
      p.F
    ) %>%
    unique() %>%
    group_by(colorscale, Species, distrib) %>%
    mutate(
      fraction.1 = ab1/(ab1+ab2),
      fraction.2 = ab2/(ab1+ab2),
      DC.mean = if_else(
        distrib == 'bi',
        (DC1*ab1+DC2*ab2)/(ab1+ab2),
        DC1
      ),
      USE.1 = if_else(
        ref == 0,
        (DC1-DC.final)*seq$nX/(DC.init-DC.final),
        (centroid.1 - ref)*seq$z/((DC.init - DC.final)/100*(listMass$H[2]-listMass$H[1]))
      ),
      USE.2 = if_else(
        ref == 0,
        (DC2-DC.final)*seq$nX/(DC.init-DC.final),
        (centroid.2 - ref)*seq$z/((DC.init - DC.final)/100*(listMass$H[2]-listMass$H[1]))
      ),
      USE.mean = if_else(
        ref == 0,
        (DC.mean-DC.final)*seq$nX/(DC.init-DC.final),
        (centroid - ref)*seq$z/((DC.init - DC.final)/100*(listMass$H[2]-listMass$H[1]))
      ),
      delta.USE = USE.2-USE.1
    )

}
