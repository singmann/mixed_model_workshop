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

no_pooling_estimates <- dat %>% 
  group_by(p_id, rel_cond, dv_question) %>% 
  do(tidy(lm(dv ~ c_given_a, .)))

knitr::kable(head(no_pooling_estimates))
  # |p_id           |rel_cond |dv_question |term        |   estimate| std.error|    statistic|   p.value|
  # |:--------------|:--------|:-----------|:-----------|----------:|---------:|------------:|---------:|
  # |102_P(if,then) |PO       |probability |(Intercept) | -0.0105887| 0.0255984|   -0.4136475| 0.7192692|
  # |102_P(if,then) |PO       |probability |c_given_a   |  0.9785920| 0.0782475|   12.5063698| 0.0063328|
  # |102_P(if,then) |NE       |probability |(Intercept) | -0.2733652| 0.0071385|  -38.2945228| 0.0006812|
  # |102_P(if,then) |NE       |probability |c_given_a   |  0.4347993| 0.0153801|   28.2702955| 0.0012489|
  # |102_P(if,then) |IR       |probability |(Intercept) | -0.4726837| 0.0009890| -477.9216145| 0.0000044|
  # |102_P(if,then) |IR       |probability |c_given_a   |  0.0564430| 0.0021299|   26.5000000| 0.0014210|

slopes <- no_pooling_estimates %>% 
  filter(term == "c_given_a")

require(afex)

(a1 <- aov_car(estimate ~ dv_question*rel_cond + Error(p_id/rel_cond), slopes))
# Anova Table (Type 3 tests)
# 
# Response: estimate
#                 Effect           df  MSE         F   ges p.value
# 1          dv_question       1, 173 0.35      0.05 .0001     .82
# 2             rel_cond 1.86, 321.47 0.30 25.29 ***   .08  <.0001
# 3 dv_question:rel_cond 1.86, 321.47 0.30      1.79  .006     .17
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
# 
# Sphericity correction method: GG 

lsmeans(a1, "rel_cond")
# rel_cond    lsmean         SE     df  lower.CL  upper.CL
# PO       0.8158869 0.04072991 525.28 0.7358733 0.8959004
# NE       0.5934296 0.04061517 524.72 0.5136413 0.6732179
# IR       0.4105888 0.04094906 526.27 0.3301451 0.4910325
# 
# Results are averaged over the levels of: dv_question 
# Confidence level used: 0.95 

ggplot(slopes, aes(estimate)) +
  geom_histogram(binwidth = 0.2) + 
  facet_grid(dv_question ~ rel_cond)
