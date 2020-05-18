# OligoR
Software suite for oligonucleotide native MS data treatment

## Installation

In R, run:

```{r install_online}
install.packages("devtools")
devtools::install_github('EricLarG4/OligoR', build_vignettes = T, build_manual = T)
```
Restart your R session before use.

## Use

To use oligor, in R, run:

```{r use}
library(OligoR)
oligor()
```
When working in R without IDE, OligoR will open in the default web browser. OligoR can also be used in a web browser when using Rstudio, if preferred to the IDE's interface.

## HDX workflow

The HDX workflow and main features are presented below:

![blabla](https://github.com/EricLarG4/OligoR/blob/master/man/ressources/HDX_workflow.png)

## License

GPL-3 [Eric Largy](figures/https://github.com/EricLarG4)
