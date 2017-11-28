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
options(digits = 3)


## ------------------------------------------------------------------------
m0 <- lm(dv~c_given_a, dat)
summary(m0)

## ---- fig.width=7, fig.height=3------------------------------------------
ggplot(data = dat) + 
  geom_point(mapping = aes(x = CgivenA, y = DV), alpha = 0.2, pch = 16) + 
  facet_grid(. ~ rel_cond) + 
  coord_fixed()

## ------------------------------------------------------------------------
## Your task is to calculate the regression parameter (and potentially also the intercept) for each participant and within-subject condition (i.e., relationship of `dv` and `c_given_a` for each `p_id` and `rel_cond`).
## Then compare the individual regression parameters across conditions (i.e., `rel_cond` and `dv_question` combinations). Do this comparison in a graphical way and also statistically (i.e., ANOVA).
## The goal of this exercise is to combine your knowledge of the `tidyverse` and use it to solve the aforementioned task.
## I would suggest you use package `dplyr` and potentially `broom` or `tidyr` and `purrr`.
## In case you need some inspiration for `tidyr` and `purr`, you might want to take a look at chapter 25 (especially 25.2.1, 25.2.2, 25.2.3) of Wickham and Grolemund (2017) see: http://r4ds.had.co.nz/many-models.html


no_pooling_estimates <- dat %>% 
  group_by(p_id, rel_cond) %>% 
  do(tidy(lm(dv ~ c_given_a, .)))

knitr::kable(head(no_pooling_estimates))
# |p_id           |rel_cond |term        | estimate| std.error| statistic| p.value|
# |:--------------|:--------|:-----------|--------:|---------:|---------:|-------:|
# |102_P(if,then) |PO       |(Intercept) |   -0.011|     0.026|    -0.414|   0.719|
# |102_P(if,then) |PO       |c_given_a   |    0.979|     0.078|    12.506|   0.006|
# |102_P(if,then) |NE       |(Intercept) |   -0.273|     0.007|   -38.295|   0.001|
# |102_P(if,then) |NE       |c_given_a   |    0.435|     0.015|    28.270|   0.001|
# |102_P(if,then) |IR       |(Intercept) |   -0.473|     0.001|  -477.922|   0.000|
# |102_P(if,then) |IR       |c_given_a   |    0.056|     0.002|    26.500|   0.001|

slopes <- no_pooling_estimates %>% 
  filter(term == "c_given_a")

require(afex)

(a1 <- aov_car(estimate ~ rel_cond + Error(p_id/rel_cond), slopes))
# Anova Table (Type 3 tests)
# 
# Response: estimate
#     Effect           df  MSE         F ges p.value
# 1 rel_cond 1.85, 170.57 0.38 14.89 *** .10  <.0001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1
# 
# Sphericity correction method: GG 
# Warning messages:
# 1: Missing values for following ID(s):
# 219_P(if,then)
# Removing those cases from the analysis. 
# 2: In aov(formula(paste(dv.escaped, "~", paste(c(between.escaped, within.escaped),  :
#   Error() model is singular

lsmeans(a1, "rel_cond")
# NOTE: Results are based on intra-block estimates.
#  rel_cond lsmean     SE  df lower.CL upper.CL
#  PO        0.875 0.0623 277    0.752    0.997
#  NE        0.548 0.0623 277    0.425    0.671
#  IR        0.406 0.0625 277    0.283    0.529
# 
# Confidence level used: 0.95 

ggplot(slopes, aes(estimate)) +
  geom_histogram(binwidth = 0.2) + 
  facet_grid(. ~ rel_cond)
