## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
# see: https://github.com/yihui/xaringan
# install.packages("xaringan")
# see: 
# https://github.com/yihui/xaringan/wiki
# https://github.com/gnab/remark/wiki/Markdown
options(width=110)
options(digits = 4)

## ---- message=FALSE------------------------------------------------------
require(psych)
data(sat.act)
sat.act$gender <- factor(sat.act$gender, 1:2, labels = c("male", "female"))
sat.act$education <- factor(sat.act$education)
summary(sat.act) # alternatively: psych::describe(sat.act)
sat.act <- na.omit(sat.act)

## ---- fig.height=4, dev='svg'--------------------------------------------
par(mfrow=c(1,2))
plot(sat.act$SATV, sat.act$ACT)
plot(sat.act$SATQ, sat.act$ACT)

## ---- fig.height=3.5, fig.width=3.5, dev='svg', results='hide', message=FALSE, include=FALSE, eval=FALSE----
## library("tidyverse")
## ggplot(sat.act, aes(x = ACT, y = SATV)) +
##   geom_point() +
##   theme_light()

## ---- fig.height=3.5, fig.width=3.5, dev='svg', include=FALSE, eval=FALSE----
## ggplot(sat.act, aes(x = ACT, y = SATQ)) +
##   geom_point() +
##   theme_light()

## ------------------------------------------------------------------------
m1 <- lm(ACT ~ SATQ, sat.act)
summary(m1)

## ------------------------------------------------------------------------
coef(m1)

## ---- fig.height=3.7, fig.width=4, dev='svg'-----------------------------
plot(sat.act$SATQ, sat.act$ACT)
abline(m1)

## ------------------------------------------------------------------------
sat.act$SATQ_c <- sat.act$SATQ - mean(sat.act$SATQ)
sat.act$SATV_c <- sat.act$SATV - mean(sat.act$SATV)
m2 <- lm(ACT ~ SATQ_c, sat.act)
summary(m2)

## ------------------------------------------------------------------------
coef(m2)

## ---- fig.height=3.7, fig.width=4, dev='svg'-----------------------------
plot(sat.act$SATQ_c, sat.act$ACT)
abline(m2)

## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ SATQ_c + SATV_c, sat.act)   # a
## lm(ACT ~ SATQ_c : SATV_c, sat.act)   # b
## lm(ACT ~ 0 + SATQ_c:SATV_c, sat.act) # c
## lm(ACT ~ SATQ_c*SATV_c, sat.act)     # d
## lm(ACT ~ 0+SATQ_c*SATV_c, sat.act)   # e

## ------------------------------------------------------------------------
coef(lm(ACT ~ SATQ_c + SATV_c, sat.act))   # a
coef(lm(ACT ~ SATQ_c : SATV_c, sat.act))   # b
coef(lm(ACT ~ 0 + SATQ_c:SATV_c, sat.act)) # c

## ------------------------------------------------------------------------
coef(lm(ACT ~ SATQ_c*SATV_c, sat.act))     # d
coef(lm(ACT ~ 0+SATQ_c*SATV_c, sat.act))   # e

## ---- eval=FALSE, include=FALSE------------------------------------------
## summary(lm(ACT ~ SATQ + SATV, sat.act))   # a
## summary(lm(ACT ~ SATQ : SATV, sat.act))   # b
## summary(lm(ACT ~ 0 + SATQ:SATV, sat.act)) # c
## summary(lm(ACT ~ SATQ*SATV, sat.act))     # d
## summary(lm(ACT ~ 0+SATQ*SATV, sat.act))   # e

## ------------------------------------------------------------------------
str(sat.act) ## alternatively tibble::glimpse(sat.act)

## ------------------------------------------------------------------------
m3 <- lm(ACT ~ gender, sat.act)
summary(m3)

## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)
require(dplyr)

## ------------------------------------------------------------------------
mean(sat.act$ACT)
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT))

## ------------------------------------------------------------------------
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT)) %>%
  {.$m[2] - .$m[1]}

## ---- include=FALSE------------------------------------------------------
options(op)

## ------------------------------------------------------------------------
model.matrix(ACT ~ gender, sat.act[1:5,])

## ------------------------------------------------------------------------
model.matrix(ACT ~ gender, sat.act[1:5,])

## ------------------------------------------------------------------------
afex::set_sum_contrasts()

## ------------------------------------------------------------------------
model.matrix(ACT ~ gender, sat.act[1:5,])

## ------------------------------------------------------------------------
m4 <- lm(ACT ~ gender, sat.act)
summary(m4)

## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)

## ------------------------------------------------------------------------
mean(sat.act$ACT)
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT))
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT)) %>% 
  summarise(mean(m))


## ---- include=FALSE------------------------------------------------------
options(op)

## ------------------------------------------------------------------------
afex::set_default_contrasts() # or set_treatment_contrasts()

## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)

## ------------------------------------------------------------------------
m5 <- lm(ACT ~ gender*education, sat.act)
coef(m5)

## ------------------------------------------------------------------------
sat.act %>% 
  group_by(gender,education) %>%
  summarise(mean(ACT))

## ---- include=FALSE------------------------------------------------------
options(op)

## ------------------------------------------------------------------------
afex::set_sum_contrasts() # or set_effects_contrasts() or set_deviation_contrasts()

## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)

## ------------------------------------------------------------------------
m6 <- lm(ACT ~ gender*education, sat.act)
coef(m6)

## ------------------------------------------------------------------------
sat.act %>% 
  group_by(gender,education) %>%
  summarise(m = mean(ACT)) %>% 
  ungroup() %>% 
  summarise(mean(m))

## ---- include=FALSE------------------------------------------------------
options(op)

## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ SATQ + SATV, sat.act)   # a: 3
## lm(ACT ~ SATQ : SATV, sat.act)   # b: 2
## lm(ACT ~ 0 + SATQ:SATV, sat.act) # c: 1
## lm(ACT ~ SATQ*SATV, sat.act)     # d: 4
## lm(ACT ~ 0+SATQ*SATV, sat.act)   # e: 3
## 
## lm(ACT ~ SATQ, sat.act)          # f: 2
## lm(ACT ~ 0 + SATQ, sat.act)      # g: 1

## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ gender, sat.act)                  # a
## lm(ACT ~ 0+gender, sat.act)                # b
## lm(ACT ~ gender+education, sat.act)        # c
## lm(ACT ~ 0+gender+education, sat.act)      # d
## lm(ACT ~ gender:education, sat.act)        # e
## lm(ACT ~ 0+gender:education, sat.act)      # f
## lm(ACT ~ gender*education, sat.act)        # g
## lm(ACT ~ 0+gender*education, sat.act)      # h
## lm(ACT ~ gender+gender:education, sat.act) # i

## ------------------------------------------------------------------------
levels(sat.act$gender)
levels(sat.act$education)

## ------------------------------------------------------------------------
coef(lm(ACT ~ gender, sat.act))                  # a: 2
coef(lm(ACT ~ 0+gender, sat.act))                # b: 2
coef(lm(ACT ~ gender+education, sat.act))        # c: 7
coef(lm(ACT ~ 0+gender+education, sat.act))      # d: 7

## ------------------------------------------------------------------------
coef(lm(ACT ~ gender:education, sat.act))        # e: 13
coef(lm(ACT ~ 0+gender:education, sat.act))      # f: 12

## ---- eval = FALSE-------------------------------------------------------
## coef(lm(ACT ~ gender*education, sat.act))        # g: 12
## coef(lm(ACT ~ 0+gender*education, sat.act))      # h: 12
## coef(lm(ACT ~ gender+gender:education, sat.act)) # i: 12

## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)

## ---- message=FALSE------------------------------------------------------
afex::set_sum_contrasts()
m6 <- lm(ACT ~ gender*education, sat.act)
summary(m6)

## ------------------------------------------------------------------------
sat.act %>% 
  group_by(gender, education) %>%
  summarise(m = mean(ACT)) %>% 
  ungroup() %>% 
  summarise(mean(m))

## ---- include=FALSE------------------------------------------------------
options(op)

## ---- message=FALSE------------------------------------------------------
require(car) # Companion to Applied Regression (Fox & Weisberg, 2011)
Anova(m6, type = 3)

## ---- message=FALSE, warning=FALSE---------------------------------------
library("emmeans")    
(emms <- emmeans(m6, ~education))

## ---- message=FALSE------------------------------------------------------
pairs(emms, adjust='holm')

## ---- message=FALSE, warning=FALSE---------------------------------------
library("emmeans")  
(emms <- emmeans(m6, "education")) 

## ---- message=FALSE------------------------------------------------------
cs <- list(
  "12-45" = c(0, -0.5, -0.5, 0, 0.5, 0.5),
  "0-3" = c(-1, 0, 0, 1, 0, 0),
  "all-last" = c(-rep(0.2, 5), 1)
)
contrast(emms, cs, adjust = "holm")

## ---- message=FALSE, comment='#'-----------------------------------------
library("afex")
sat.act$id <- factor(1:nrow(sat.act))
(a1 <- aov_car(ACT ~ gender+Error(id), sat.act))


## ------------------------------------------------------------------------
sat_long <- tidyr::gather(
  sat.act, key = "SAT_type", 
  value = "SAT_value", SATV, SATQ)

## ---- message=FALSE, comment='#'-----------------------------------------
(a2 <- aov_car(SAT_value ~ gender*SAT_type+
                 Error(id/SAT_type), sat_long))
emmeans(a2, c("gender", "SAT_type"))

## ------------------------------------------------------------------------
data("Machines", package = "MEMSS")
str(Machines)

## ---- include=FALSE------------------------------------------------------
library("tidyverse")

## ------------------------------------------------------------------------
library("tidyverse")
Machines %>% group_by(Machine) %>% 
  summarise(m = mean(score), se = sd(score)/sqrt(n()))

## ---- fig.height=4, dev='svg'--------------------------------------------
ggplot(Machines, aes(x = Machine, y = score)) +
  geom_point() + 
  facet_wrap(~ Worker) + 
  theme_light()

## ------------------------------------------------------------------------
mach_agg <- Machines %>% 
  group_by(Worker, Machine) %>% 
  summarise(score = mean(score))

## ---- include=FALSE------------------------------------------------------
ggplot(mach_agg, aes(x = Machine, y = score)) + geom_point()

## ---- message=FALSE------------------------------------------------------
afex::set_sum_contrasts()
mmach <- lm(score ~ Machine, mach_agg)
car::Anova(mmach, type = 3)

## ------------------------------------------------------------------------
library("emmeans")
pairs(emmeans(mmach, "Machine"), 
      adjust = "holm")

## ------------------------------------------------------------------------
dm1 <- Machines %>% 
  filter(Worker == "1")

## ------------------------------------------------------------------------
m1 <- lm(score ~ Machine, dm1)
car::Anova(m1, type = 3)

## ---- warning=FALSE------------------------------------------------------
a1 <- aov_car(score ~ Error(Worker/Machine), Machines)
a1

## ------------------------------------------------------------------------
pairs(emmeans(a1, "Machine"), 
      adjust = "holm")

## ------------------------------------------------------------------------
pairs(emmeans(mmach, "Machine"), 
      adjust = "holm")  ## no pooling results

## ------------------------------------------------------------------------
# Session -> Set Working Directory ->
# -> To Source File Location
load("ssk16_dat_tutorial.rda") 
# full data: https://osf.io/j4swp/
str(dat, width=50, strict.width = "cut")

## ---- fig.height=6, dev='svg'--------------------------------------------
ggplot(data = dat) + 
  geom_point(mapping = aes(x = B_given_A, 
                           y = if_A_then_B), 
             alpha = 0.2, pch = 16, size = 3) + 
  coord_fixed() +
  theme_light() +
  theme(text = element_text(size=20))


## ------------------------------------------------------------------------
m1 <- lm(if_A_then_B~B_given_A, dat)
broom::tidy(m1)

## ------------------------------------------------------------------------
dat_p <- dat %>% 
  group_by(p_id) %>% 
  summarise_if(is.numeric, mean)
  
m2 <- lm(if_A_then_B~B_given_A, dat_p)
broom::tidy(m2)

## ------------------------------------------------------------------------
dat_i <- dat %>% 
  group_by(i_id) %>% 
  summarise_if(is.numeric, mean)
  
m3 <- lm(if_A_then_B~B_given_A, dat_i)
broom::tidy(m3)

## ------------------------------------------------------------------------
no_pooling_estimates <- dat %>% 
  group_by(p_id) %>% 
  do(broom::tidy(lm(if_A_then_B ~ B_given_A, .)))
## see: https://stackoverflow.com/a/30015869/289572

no_pooling_estimates

## ---- fig.height=5, dev='svg'--------------------------------------------
slopes <- no_pooling_estimates %>% 
  filter(term == "B_given_A")

ggplot(slopes, aes(estimate)) +
  geom_histogram(bins = 35) +
  theme_light() +
  theme(text = element_text(size=20))


## ------------------------------------------------------------------------
m_no <- lm(estimate ~ 1, slopes)
car::Anova(m_no, type = 3)
broom::tidy(m_no)


