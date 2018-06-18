## ---- message=FALSE, warning=FALSE---------------------------------------
library("tidyverse")
library("broom") # not automatically loaded

## ------------------------------------------------------------------------
# Run complete chunk: Ctrl+Shift+Enter

# You might need to set the correct working directory via the menu: 
# Session -> Set Working Directory -> To Source File Location

afex::set_sum_contrasts() # just in case we set orthogonal contrasts

load("ssk16_dat_preapred_ex1.rda") # data preapred in 'prepare_data.R'
str(dat)


## ------------------------------------------------------------------------
m0 <- lm(dv~c_given_a, dat)
summary(m0)

## ---- fig.width=7, fig.height=3------------------------------------------
ggplot(data = dat) + 
  geom_point(mapping = aes(x = CgivenA, y = DV), alpha = 0.2, pch = 16) + 
  coord_fixed()

## ------------------------------------------------------------------------

## Full instructions:
## Your task is to calculate the regression parameter (i.e., slope, potentially 
# also the intercept) for each participant (i.e., relationship of `dv` and 
# `c_given_a` for each `p_id`).
## Then investigate the distribution of resulting regression parameters. Perform 
# this investigation in a graphical way and also statistically (i.e., using 
# `lm`).
## The goal of this exercise is to combine your knowledge of the `tidyverse` and 
# use it to solve the aforementioned task.
## In case you need some inspiration for `tidyr` and `purr`, you might want to 
# take a look at chapter 25 (especially 25.2.1, 25.2.2, 25.2.3) of Wickham and 
# Grolemund (2017) see: http://r4ds.had.co.nz/many-models.html

# go


no_pooling_estimates <- dat %>% 
  group_by(p_id) %>% 
  do(tidy(lm(dv ~ c_given_a, .)))

knitr::kable(head(no_pooling_estimates))
# |p_id           |term        |   estimate| std.error| statistic|   p.value|
# |:--------------|:-----------|----------:|---------:|---------:|---------:|
# |102_P(if,then) |(Intercept) | -0.2034362| 0.0679868| -2.992290| 0.0135206|
# |102_P(if,then) |c_given_a   |  0.5469525| 0.1605328|  3.407107| 0.0066907|
# |107_P(if,then) |(Intercept) | -0.2271141| 0.0819747| -2.770540| 0.0197690|
# |107_P(if,then) |c_given_a   |  0.6395180| 0.2459363|  2.600340| 0.0264760|
# |108_P(if,then) |(Intercept) | -0.1591294| 0.1245732| -1.277397| 0.2303217|
# |108_P(if,then) |c_given_a   |  0.4874299| 0.3553438|  1.371713| 0.2001417|

slopes <- no_pooling_estimates %>% 
  filter(term == "c_given_a")

### Graphical solution:
ggplot(slopes, aes(estimate)) +
  geom_histogram(bins = 45) +
  theme_light()

m1 <- lm(estimate ~ 1, slopes)
car::Anova(m1, type = 3)
# Anova Table (Type III tests)
# 
# Response: estimate
#             Sum Sq Df F value    Pr(>F)    
# (Intercept) 45.427  1  719.52 < 2.2e-16 ***
# Residuals    5.872 93                      
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(m1)
# Call:
# lm(formula = estimate ~ 1, data = slopes)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.59425 -0.18756  0.03037  0.21153  0.38994 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.69517    0.02592   26.82   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2513 on 93 degrees of freedom
