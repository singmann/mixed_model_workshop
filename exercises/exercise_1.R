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

