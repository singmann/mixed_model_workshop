## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
# see: https://github.com/yihui/xaringan
# install.packages("xaringan")
# see: 
# https://github.com/yihui/xaringan/wiki
# https://github.com/gnab/remark/wiki/Markdown
options(width=110)
options(digits = 4)

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
load("ssk16_dat_tutorial.rda") 
str(dat)
datr <- droplevels(dat[dat$rel_cond != "NE",])
library("ggplot2")
afex::set_sum_contrasts()
library("lme4")

## ------------------------------------------------------------------------
m_fixed <- lm(if_A_then_B_c ~ B_given_A_c, datr)
summary(m_fixed)

## ---- echo=FALSE, dpi=500, fig.width=3.5, fig.height=3.5-----------------
par(pty="s")
limits <- c(-0.5, 0.5)
plot(if_A_then_B_c ~ B_given_A_c, datr, asp = 1, ylim=limits, xlim=limits)
abline(m_fixed)

## ---- echo=FALSE, dpi=300, fig.width=3.5, fig.height=4-------------------
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, asp = 1, ylim=limits, xlim=limits)
abline(m_fixed)

## ---- echo=FALSE, dpi=500, fig.width=3.5, fig.height=4, warning=FALSE----
m_tmp <- lmer(if_A_then_B_c ~ B_given_A_c + (0+B_given_A_c|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, 
     asp = 1, ylim=limits, xlim=limits)
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1], 
         b = rnd_coefs[i,2],
         col = "lightgrey")
abline(m_fixed)

## ---- echo=FALSE, dpi=300, fig.width=3.5, fig.height=3.5, warning=FALSE, out.width='25%'----
m_tmp <- lmer(if_A_then_B_c ~ B_given_A_c + (1+B_given_A_c|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
rnd_coefs <- coef(m_tmp)$p_id
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr,
     asp = 1, ylim=limits, xlim=limits)
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1], 
         b = rnd_coefs[i,2],
         col = "lightgrey")
abline(m_fixed)

## ---- echo=FALSE, dpi=300, fig.width=3.5, fig.height=4, warning=FALSE , out.width='25%'----
m_tmp <- lmer(if_A_then_B_c ~ B_given_A_c + (1+B_given_A_c|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
rnd_coefs <- coef(m_tmp)$p_id
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr,
     asp = 1, ylim=limits, xlim=limits)
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1], 
         b = rnd_coefs[i,2],
         col = "lightgrey")
abline(m_fixed)

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE, out.width='80%'----
ggplot(datr, aes(y = if_A_then_B_c, x = B_given_A_c)) +
  geom_point() +
  facet_wrap(~ rel_cond) + 
  theme_light() + coord_fixed()

## ------------------------------------------------------------------------
m_fixed <- lm(if_A_then_B_c ~ 
                B_given_A_c*rel_cond, datr)
summary(m_fixed)

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4---------------------
par(mfrow = c(1,2))
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4---------------------
par(mfrow = c(1,2))
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
m_tmp <- lmer(if_A_then_B_c ~ B_given_A_c*rel_cond + (0+B_given_A_c|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=3.5, warning=FALSE----
m_tmp <- lmer(if_A_then_B_c ~ B_given_A_c*rel_cond + (1+B_given_A_c|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ------------------------------------------------------------------------
library("lme4")
m_p_max <- 
  lmer(if_A_then_B_c ~ B_given_A_c*rel_cond + 
         (B_given_A_c*rel_cond|p_id), datr)
summary(m_p_max)$varcor
summary(m_p_max)$coefficients


## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
m_tmp <- lmer(if_A_then_B_c ~ B_given_A_c*rel_cond + (B_given_A_c*rel_cond|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ------------------------------------------------------------------------
m_max <- lmer(if_A_then_B_c ~ B_given_A_c*rel_cond + 
                (B_given_A_c*rel_cond|p_id) + 
                (B_given_A_c*rel_cond|i_id), 
              datr)

## ------------------------------------------------------------------------
summary(m_max)

## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------
library("dplyr")
library("broom")
library("ggplot2")
library("tidyr")
no_pooling_estimates <- datr %>% 
  group_by(p_id, rel_cond) %>% 
  do(tidy(lm(if_A_then_B_c~B_given_A_c, .))) %>% 
  filter(term == "B_given_A_c") %>% 
  rename(no_pooling = estimate)

partial_pooling_estimates <- data.frame(p_id = rownames(coef(m_max)$p_id),
           PO = coef(m_max)$p_id[,2] + coef(m_max)$p_id[,4],
           IR = coef(m_max)$p_id[,2] - coef(m_max)$p_id[,4])
partial_pooling_estimates <- tidyr::gather(partial_pooling_estimates, key = "rel_cond", value = "partial_pooling", PO, IR)

estimates <- left_join(no_pooling_estimates, partial_pooling_estimates)


## ---- echo=FALSE, out.width='500px', out.height='300px', dpi = 500, fig.width=7, fig.height=7*3/5----

ggplot(data = estimates) + 
  geom_point(mapping = aes(x = no_pooling, y = partial_pooling), alpha = 1.0, pch = 16) + 
  facet_grid(rel_cond ~ .) + 
  coord_fixed() + 
  geom_abline(slope = 1, intercept = 0) + 
  theme(text=element_text(size=18))


## ---- echo=FALSE, out.width='400px', out.height='350px', dpi = 500, fig.width=7, fig.height=7*35/40----
estimates_l <- estimates %>% 
  gather("key","estimate",no_pooling, partial_pooling) 

ggplot(data = estimates_l, aes(estimate)) + 
  geom_histogram(binwidth = 0.2) + 
  facet_grid(key ~ rel_cond) +
  theme(text=element_text(size=18))

## ---- echo=FALSE, out.width='1000px', out.height='500px', dpi = 500, fig.width=10, fig.height=5----

df_gravity <- as.data.frame(summary(emmeans::emtrends(m_fixed, "rel_cond", var = "B_given_A_c")))
df_gravity <- df_gravity %>% 
  select(rel_cond, B_given_A_c.trend) %>% 
  spread(rel_cond, B_given_A_c.trend) %>% 
  mutate(key = "complete_pooling")

estimates_l %>% 
  select(-std.error, -statistic, -p.value) %>% 
  spread(rel_cond, estimate) %>% 
  na.omit() %>% 
  ungroup %>% 
  ggplot() + 
  aes(x = PO, y = IR, color = key) + 
  geom_point(size = 2) + 
  geom_point(data = df_gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = p_id, color = NULL, alpha = 0.1), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters") + 
  scale_color_brewer(palette = "Dark2") 


## ---- eval=FALSE---------------------------------------------------------
## library("afex")
## mixed(if_A_then_B ~ B_given_A*rel_cond + (B_given_A*rel_cond|p_id), datr, method = "KR")
## mixed(if_A_then_B ~ B_given_A*rel_cond + (B_given_A*rel_cond|p_id), datr, method = "S")
## mixed(if_A_then_B ~ B_given_A*rel_cond + (B_given_A*rel_cond|p_id), datr, method = "LRT")
## # mixed(if_A_then_B ~ B_given_A*rel_cond + (B_given_A*rel_cond|p_id), datr, method = "PB")

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
library("afex")

## ---- results='hide', message=FALSE--------------------------------------
m_red <- mixed(
  if_A_then_B_c ~ B_given_A_c*rel_cond + 
    (B_given_A_c*rel_cond||p_id), 
  datr, method = "S", 
  expand_re = TRUE)

## ------------------------------------------------------------------------
summary(m_red)$varcor

## ------------------------------------------------------------------------
m_red

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
rnd_coefs <- coef(m_red$full_model)$p_id
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,4] + rnd_coefs[i,6] + rnd_coefs[i,2], 
         b = rnd_coefs[i,5] + rnd_coefs[i,1] + rnd_coefs[i,7] + rnd_coefs[i,3],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(if_A_then_B_c ~ B_given_A_c, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,4] - (rnd_coefs[i,6] + rnd_coefs[i,2]), 
         b = rnd_coefs[i,5] + rnd_coefs[i,1] - (rnd_coefs[i,7] + rnd_coefs[i,3]),
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- eval=FALSE---------------------------------------------------------
## m_fhch <- mixed(log_rt ~ task*stimulus*density*frequency*length +
##                   (stimulus*density*frequency*length||id) +
##                   (task||item), fhch2010,
##                 method = "S", expand_re = TRUE)

## ---- message=FALSE------------------------------------------------------
m_max2 <- mixed(
  if_A_then_B_c ~ B_given_A_c*rel_cond + 
    (B_given_A_c*rel_cond||p_id) + 
    (B_given_A_c*rel_cond||i_id), 
  datr, method = 'S', expand_re = TRUE)
nice(m_max2) %>% as.data.frame()

## ------------------------------------------------------------------------
emm_options(lmer.df = "asymptotic") 
# or "Kenward-Roger" or "Satterthwaite"
emmeans(m_max2, "rel_cond")

## ------------------------------------------------------------------------
emm_options(lmer.df = "asymptotic") 
# or "Kenward-Roger" or "Satterthwaite"
emtrends(m_max2, "rel_cond", var = "B_given_A_c")

## ------------------------------------------------------------------------
fixef(m_max2$full_model)[2] + fixef(m_max2$full_model)[4] 

## ------------------------------------------------------------------------
data("Machines", package = "MEMSS")

## ---- include=FALSE------------------------------------------------------
library("tidyverse")

## ---- fig.height=4, dev='svg', echo=FALSE--------------------------------
ggplot(Machines, aes(x = Machine, y = score)) +
  geom_point() + 
  facet_wrap(~ Worker) + 
  theme_light()

## ------------------------------------------------------------------------
mach1 <- lm(score ~ Machine, Machines)
car::Anova(mach1, type = 3)

## ------------------------------------------------------------------------
data("Machines", package = "MEMSS")

## ---- include=FALSE------------------------------------------------------
library("tidyverse")

## ------------------------------------------------------------------------
mach1 <- lm(score ~ Machine, Machines)
car::Anova(mach1, type = 3)

## ---- results="hide"-----------------------------------------------------
(mach2 <- mixed(score~Machine+
                (Machine|Worker), Machines))

## ---- echo=FALSE---------------------------------------------------------
mach2 

## ------------------------------------------------------------------------
pairs(emmeans(mach1, "Machine"),
      adjust = "holm")

## ------------------------------------------------------------------------
pairs(emmeans(mach2, "Machine"),
      adjust = "holm")

## ---- echo=FALSE, message=FALSE, results='hide'--------------------------
library("sjstats")

## ------------------------------------------------------------------------
m1 <- lmer(if_A_then_B_c ~ 1 + (1|p_id), datr)
# summary(m1)
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  p_id     (Intercept) 0.00572  0.0757  
#  Residual             0.14607  0.3822  
# Number of obs: 752, groups:  p_id, 94

0.00572 / (0.0057+0.1461)
library("sjstats")
icc(m1)

## ------------------------------------------------------------------------
m1 <- lmer(if_A_then_B_c ~ 1 + (1|p_id), datr)
# summary(m1)
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  p_id     (Intercept) 0.00572  0.0757  
#  Residual             0.14607  0.3822  
# Number of obs: 752, groups:  p_id, 94

icc(m1)

## ---- warning=FALSE, message=FALSE---------------------------------------
m2 <- lmer(if_A_then_B_c ~ 1 + 
             (rel_cond:B_given_A_c|p_id), datr)
# summary(m2)
 # Groups   Name                   Variance Std.Dev. Corr       
 # p_id     (Intercept)            0.0398   0.200               
 #          rel_condPO:B_given_A_c 1.0186   1.009    -0.94      
 #          rel_condIR:B_given_A_c 0.3262   0.571    -0.48  0.75
 # Residual                        0.0570   0.239               
icc(m2)
## Caution! ICC for random-slope-intercept models usually 
## not meaningful. See 'Note' in `?icc`.

## ---- eval=FALSE---------------------------------------------------------
## data("fhch2010")
## fhch2 <- droplevels(fhch2010[fhch2010$task == "lexdec", ] )
## gm1 <- mixed(correct ~ stimulus + (stimulus||id) + (stimulus||item),
##              fhch2, family = binomial,      # implies: binomial(link = "logit")
##              method = "LRT", expand_re = TRUE) # alt: binomial(link = "probit")
## gm1
## ## Mixed Model Anova Table (Type 3 tests, LRT-method)
## ##
## ## Model: correct ~ stimulus + (stimulus || id) + (stimulus | item)
## ## Data: fhch2
## ## Df full model: 7
## ##     Effect df Chisq p.value
## ## 1 stimulus  1  1.19     .28
## ## ---
## ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
## ## Warning messages: [...]
## emmeans(gm1, "stimulus", type = "response")
## ##  stimulus   prob       SE df asymp.LCL asymp.UCL
## ##  word     0.9907 0.002323 NA    0.9849    0.9943
## ##  nonword  0.9857 0.003351 NA    0.9774    0.9909
## ##
## ## Confidence level used: 0.95
## ## Intervals are back-transformed from the logit scale

## ---- fig.width=5, fig.height=4------------------------------------------
plot(m_max, 
     resid(.,scaled=TRUE) ~ B_given_A | rel_cond)

## ---- fig.width=4, fig.height=4------------------------------------------
lattice::qqmath(m_max)

## ---- eval=FALSE---------------------------------------------------------
## plot(m_max, p_id ~ resid(., scaled=TRUE) )
## plot(m_max, resid(., scaled=TRUE) ~ fitted(.) | rel_cond)
## ?plot.merMod

## ---- eval=FALSE, include=FALSE------------------------------------------
## library("afex")
## load("ssk16_dat_tutorial.rda")

## ---- eval=FALSE, include=FALSE------------------------------------------
## 
## m_full <- mixed(if_A_then_B_c ~ B_given_A_c*rel_cond +
##                        (rel_cond*B_given_A_c|p_id) +
##                        (rel_cond*B_given_A_c|i_id),
##                      dat,
##                      control = lmerControl(optCtrl = list(maxfun=1e8)),
##                      method = "S")
## 
## save(m_full, file = "fitted_lmms.rda", compress = "xz")

