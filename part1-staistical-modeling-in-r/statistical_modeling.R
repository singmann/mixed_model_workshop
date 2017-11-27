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

## ------------------------------------------------------------------------
m1 <- lm(ACT ~ SATQ, sat.act)
summary(m1)

## ------------------------------------------------------------------------
coef(m1)

## ---- fig.height=3.7, fig.width=4, dev='svg'-----------------------------
plot(sat.act$SATV, sat.act$ACT)
abline(m1)

## ------------------------------------------------------------------------
sat.act$SATQ_c <- sat.act$SATQ - mean(sat.act$SATQ, na.rm = TRUE)
sat.act$SATV_c <- sat.act$SATV - mean(sat.act$SATV)
m2 <- lm(ACT ~ SATQ_c, sat.act)
summary(m2)

## ------------------------------------------------------------------------
coef(m2)

## ---- fig.height=3.7, fig.width=4, dev='svg'-----------------------------
plot(sat.act$SATV_c, sat.act$ACT)
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
str(sat.act)

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
require(lsmeans)
lsmeans(m6, ~education)

## ---- message=FALSE------------------------------------------------------
pairs(lsmeans(m6,~education),adjust='holm')

## ---- message=FALSE, comment='#'-----------------------------------------
require(afex)
sat.act$id <- factor(1:nrow(sat.act))
(a1 <- aov_car(ACT ~ gender+Error(id), sat.act))


## ------------------------------------------------------------------------
sat_long <- tidyr::gather(
  sat.act, key = "SAT_type", 
  value = "SAT_value", SATV, SATQ)

## ---- message=FALSE, comment='#'-----------------------------------------
(a2 <- aov_car(SAT_value ~ gender*SAT_type+
                 Error(id/SAT_type), sat_long))
lsmeans(a2, c("gender", "SAT_type"))

