# Statistical Modeling and Mixed Models with R

This repo contains slides and exercise materials for my workshop on statistical modeling and mixed models with R. Previous instances of this workshop:

- The first instance of this workshop was held as part of the [Data on the Mind 2017](http://www.dataonthemind.org/2017-workshop). Title: *Statistical Models for Dependent Data: An Introduction to Mixed Models in R*
- One day workshop at the University of Freiburg in June 2018. Title: *Mixed Models in R – An Applied Introduction*
- One day tutorial at CogSci 2018 in Madison (Wisconsin). Title: *Mixed Models in R – An Applied Introduction*

The mixed model part of the workshop are loosely based on my chapter: [An introduction to linear mixed modeling in experimental psychology.](http://singmann.org/download/publications/singmann_kellen-introduction-mixed-models.pdf)
Read the chapter to get a more comprehensive overview.


The repo currently contains three `html` presentations:

- [Part 0: Introduction to Modern `R`](https://htmlpreview.github.io/?https://github.com/singmann/mixed_model_workshop/blob/master/part0-introduction/introduction.html)
- [Part 1: Statistical Modeling in R](https://htmlpreview.github.io/?https://github.com/singmann/mixed_model_workshop/blob/master/part1-statistical-modeling-in-r/statistical_modeling.html)
- [Part 2: Mixed Models in R](https://htmlpreview.github.io/?https://github.com/singmann/mixed_model_workshop/blob/master/part2-mixed-models-in-r/mixed_models.html)

In addition, the repo contains a [`pdf` handout](https://github.com/singmann/mixed_model_workshop/raw/master/handout/mixed_model_handout.pdf) providing a concise overview. 

### Requirements
- A recent version of `R` (currently `R 3.5.1`): `https://cran.rstudio.com/`
- `R` packages necessary for the analysis (install with `install.packages("package")` at `R` prompt): `afex` (which automatically installs the additional requirements `emmeans`, `lme4`, and `car`) and `psych` and `MEMSS` (for example data)
- `R` package `tidyverse` as well as `broom` for the exercises (we mainly need `dplyr`, `broom`, `tidyr`, `purrr`, and `ggplot2`).
- `R` package `xaringan` to compile the slides.
- `R` package `sjstats` for Intraclass Correlation Coefficient (ICC)
- Possibly `R` packages `sjPlot` and `MuMIn` for some examples.
- A html 5 compatible browser to view the slides.
- `RStudio`: https://www.rstudio.com/products/rstudio/download3/#download

### Overview

In order to increase statistical power and precision, many data sets in cognitive and behavioral sciences contain more than one data point from each unit of observation (e.g., participant), often across different experimental conditions. Such *repeated-measures* pose a problem to most standard statistical procedures such as ordinary least-squares regression, (between-subjects) ANOVA, or generalized linear models (e.g., logistic regression) as these procedures assume that the data points are *independent and identically distributed*. In case of repeated measures, the independence assumption is expected to be violated. For example, observations coming from the same participant are usually correlated - they are more likely to be similar to each other than two observations coming from two different participants. 

The goal of this workshop is to introduce a class of statistical models that is able to account for most of the cases of non-independence that are typically encountered in cognitive science – *linear mixed-effects models* (Baayen, Davidson, & Bates, 2008), or mixed models for short. Mixed models are a generalization of ordinary regression that explicitly capture the dependency among data points via random-effects parameters.  Compared to traditional analyses approaches that ignore these dependencies, mixed models provide more accurate (and generalizable) estimates of the effects, improved statistical power, and non-inflated Type I errors (e.g., Barr, Levy, Scheepers, & Tily, 2013).

In recent years, mixed models have become increasingly popular. One of the main reason for this is that a number of software packages have appeared that allow to estimate large classes of mixed models in a relatively convenient manner. The workshop will focus on `lme4` (Bates, Mächler, Bolker, & Walker, 2015), the gold standard for estimating mixed models in `R` (R Core Team, 2018). In addition, it will introduce the functionality of `afex` (Singmann, Bolker, Westfall, & Aust, 2017), which simplifies many aspects of using `lme4`, such as the calculation of p-values for mixed models. `afex` was specifically developed with a focus on factorial designs that are common in cognitive and behavioral sciences.

Despite a number of high impact publications that introduce mixed models to a wide variety of audiences (e.g., Baayen et al., 2008; Judd, Westfall, & Kenny, 2012) the application of mixed models in practice is far from trivial. Applying mixed models requires a number of steps and decisions that are not necessarily part of the methodological arsenal of every researcher. The goal of the workshop is to change this and to introduce mixed models in such a way that they can be effectively used and the results communicated.

The workshop is split into two parts main parts and one interlude. The focus of the first part is not on mixed models, but on the basic knowledge in statistical modeling with R that necessary for competently using mixed models. The second part focuses exclusively on mixed models. It introduces the key concepts and simultaneously shows how to fit mixed models of increasing complexity. Each part will take approximately 3 hours (including breaks). The time between the two parts will be used to provide a short introduction to the `tidyverse` (Wickham & Grolemund, 2017), a modern set of tools for data science in R that are especially useful in this context.

Participants of the workshop need some basic knowledge of R. For example, they should be able to read in data, select subsets of the data, and estimate a linear regression model. Participants without any R knowledge will likely nor profit from the workshop. 

### References

- Baayen, H., Davidson, D. J., & Bates, D. (2008). Mixed-effects modeling with crossed random effects for subjects and items. *Journal of Memory and Language*, 59(4), 390–412. https://doi.org/10.1016/j.jml.2007.12.005
- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. *Journal of Statistical Software*, 67(1). https://doi.org/10.18637/jss.v067.i01
- Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects structure for confirmatory hypothesis testing: Keep it maximal. *Journal of Memory and Language*, 68(3), 255–278. https://doi.org/10.1016/j.jml.2012.11.001 
- Judd, C. M., Westfall, J., & Kenny, D. A. (2012). Treating stimuli as a random factor in social psychology: A new and comprehensive solution to a pervasive but largely ignored problem. *Journal of Personality and Social Psychology*, 103(1), 54–69. https://doi.org/10.1037/a0028347
- Singmann, H., Bolker, B., Westfall, J., & Aust, F. (2017). *afex: Analysis of Factorial Experiments.* R package version 0.18-0. http://cran.r-project.org/package=afex 
- R Core Team. (2017). *R: A Language and Environment for Statistical Computing*. Vienna, Austria: R Foundation for Statistical Computing. http://www.R-project.org/
- Wickham, H., & Grolemund, G. (2017). *R for Data Science: Import, Tidy, Transform, Visualize, and Model Data.* Sebastopol  CA: O’Reilly.

---

Last edited: June 2018

---

All code in this repository is released under the [GPL v2 or later license](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html). All non-code materials is released under the [CC-BY-SA license](https://creativecommons.org/licenses/by-sa/4.0/).
