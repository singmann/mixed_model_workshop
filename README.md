# Statistical Models for Dependent Data: An Introduction to Mixed Models in R

This repo contains the slides and exercise materials for my mixed model workshop. The first instance of this workshop was held as part of the [Data on the Mind 2017](http://www.dataonthemind.org/2017-workshop).

This workshop is loosely based on my chapter: [An introduction to linear mixed modeling in experimental psychology.](http://singmann.org/download/publications/singmann_kellen-introduction-mixed-models.pdf)
Read the chapter to get a more comprehensive overview.

The repo currently contains two `html` presentations:
- [Part 1: Statistical Modeling in R](https://htmlpreview.github.io/?https://github.com/singmann/mixed_model_workshop/blob/master/part1-statistical-modeling-in-r/statistical_modeling.html)
- [Part 2: Mixed Models in R](https://htmlpreview.github.io/?https://github.com/singmann/mixed_model_workshop/blob/master/part2-mixed-models-in-r/mixed_models.html)

In addition, the repo contains a [`pdf` handout](https://github.com/singmann/mixed_model_workshop/raw/master/handout/mixed_model_handout.pdf) providing a concise overview. 

### Requirements
- A recent version of `R` (currently `R 3.4.2`): `https://cran.rstudio.com/`
- `R` packages necessary for the analysis (install with `install.packages("package")` at `R` prompt): `afex` (which automatically installs the additional requirements `lsmeans`, `lme4`, and `car`), `psych` (for example data)
- `R` packages `dplyr`, `broom`, `tidyr`, `purrr`, and `ggplot2` for the exercise (not all might be necessary).
- `R` package `xaringan` to compile the slides.
- `R` package `sjstats` for Intraclass Correlation Coefficient (ICC)
- A html 5 compatible browser to view the slides.
- `RStudio`: https://www.rstudio.com/products/rstudio/download3/#download

### Other Comments



---

All code in this repository is released under the [GPL v2 or later license](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html). All non-code materials is released under the [CC-BY-SA license](https://creativecommons.org/licenses/by-sa/4.0/).
