## ---- message=FALSE, warning=FALSE---------------------------------------
require(dplyr)
require(tidyr)
require(ggplot2)
require(broom)


## ------------------------------------------------------------------------
# Run complete chunk: Ctrl+Shift+Enter

# You might need to set the correct working directory via the menu: 
# Session - Set Working Directory - To Source File Location

afex::set_sum_contrasts() # just in case we set orthogonal contrasts

load("ssk16_dat_preapred.rda") # data preapred in 'prepare_data.R'
str(dat)


## ------------------------------------------------------------------------
m0 <- lm(dv~c_given_a, dat)
summary(m0)

## ---- fig.width=7, fig.height=5------------------------------------------
ggplot(data = dat) + 
  geom_point(mapping = aes(x = CgivenA, y = DV), alpha = 0.2, pch = 16) + 
  facet_grid(dv_question ~ rel_cond) + 
  coord_fixed()

## ------------------------------------------------------------------------
# Your task is to calculate the regression parameter (and potentially also the intercept) for each participant and within-subject and between-subject condition (i.e., relationship of `dv` and `c_given_a` for each `p_id`, `rel_cond`, and `dv_question` combination).
# Then compare the individual regression parameters across conditions (i.e., `rel_cond` and `dv_question` combinations). Do this comparison in a graphical way and also statistically (i.e., ANOVA).
# The goal of this exercise is to combine your knowledge of the `tidyverse` and use it to solve the aforementioned task.
## go...

