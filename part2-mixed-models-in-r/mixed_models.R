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
load("../exercises/ssk16_dat_preapred.rda")
str(dat)
datr <- droplevels(dat[dat$rel_cond != "NE" & dat$dv_question == "probability",])
require(ggplot2)
afex::set_sum_contrasts()
require(lme4)

## ------------------------------------------------------------------------
m_fixed <- lm(dv ~ c_given_a*rel_cond, datr)
summary(m_fixed)

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4---------------------
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4---------------------
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
m_tmp <- lmer(dv ~ c_given_a*rel_cond + (0+c_given_a|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=3.5, warning=FALSE----
m_tmp <- lmer(dv ~ c_given_a*rel_cond + (1+c_given_a|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
m_tmp <- lmer(dv ~ c_given_a*rel_cond + (0+c_given_a|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
m_tmp <- lmer(dv ~ c_given_a*rel_cond + (1+c_given_a|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ------------------------------------------------------------------------
require(lme4)
m_p_max <- lmer(dv ~ c_given_a*rel_cond + 
                  (c_given_a*rel_cond|p_id), datr)
summary(m_p_max)$varcor
summary(m_p_max)$coefficients


## ---- echo=FALSE, dpi=500, fig.width=7, fig.height=4, warning=FALSE------
m_tmp <- lmer(dv ~ c_given_a*rel_cond + (c_given_a*rel_cond|p_id), datr)
rnd_coefs <- coef(m_tmp)$p_id
par(mfrow = c(1,2))
par(pty="s")
limits <- c(-0.5, 0.5)
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] + rnd_coefs[i,3], 
         b = rnd_coefs[i,2] + rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,1] - rnd_coefs[i,3], 
         b = rnd_coefs[i,2] - rnd_coefs[i,4],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ------------------------------------------------------------------------
m_max <- lmer(dv ~ c_given_a*rel_cond + 
                (c_given_a*rel_cond|p_id) + 
                (c_given_a*rel_cond|i_id), 
              datr)

## ------------------------------------------------------------------------
summary(m_max)

## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------
require(dplyr)
require(broom)
require(ggplot2)
require(tidyr)
no_pooling_estimates <- datr %>% 
  group_by(p_id, rel_cond) %>% 
  do(tidy(lm(dv~c_given_a, .))) %>% 
  filter(term == "c_given_a") %>% 
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

df_gravity <- as.data.frame(summary(lsmeans::lstrends(m_fixed, "rel_cond", var = "c_given_a")))
df_gravity <- df_gravity %>% 
  select(rel_cond, c_given_a.trend) %>% 
  spread(rel_cond, c_given_a.trend) %>% 
  mutate(key = "complete_pooling")

estimates_l %>% 
  select(-std.error, -statistic, -p.value) %>% 
  spread(rel_cond, estimate) %>% 
  na.omit() %>% 
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
## require(afex)
## mixed(dv ~ c_given_a*rel_cond + (c_given_a*rel_cond|p_id), datr, method = "KR")
## mixed(dv ~ c_given_a*rel_cond + (c_given_a*rel_cond|p_id), datr, method = "S")
## mixed(dv ~ c_given_a*rel_cond + (c_given_a*rel_cond|p_id), datr, method = "LRT")
## # mixed(dv ~ c_given_a*rel_cond + (c_given_a*rel_cond|p_id), datr, method = "PB")

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
require(afex)

## ---- results='hide', message=FALSE--------------------------------------
m_red <- mixed(dv ~ c_given_a*rel_cond + 
                 (c_given_a*rel_cond||p_id), 
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
plot(dv ~ c_given_a, datr, subset = rel_cond == "PO", 
     asp = 1, ylim=limits, xlim=limits, main ="PO")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,4] + rnd_coefs[i,6] + rnd_coefs[i,2], 
         b = rnd_coefs[i,5] + rnd_coefs[i,1] + rnd_coefs[i,7] + rnd_coefs[i,3],
         col = "lightgrey")
abline(a = coef(m_fixed)[1] + coef(m_fixed)[3], 
       b = coef(m_fixed)[2] + coef(m_fixed)[4])
plot(dv ~ c_given_a, datr, subset = rel_cond == "IR", 
     asp = 1, ylim=limits, xlim=limits, main ="IR")
for (i in seq_len(nrow(rnd_coefs))) 
  abline(a = rnd_coefs[i,4] - (rnd_coefs[i,6] + rnd_coefs[i,2]), 
         b = rnd_coefs[i,5] + rnd_coefs[i,1] - (rnd_coefs[i,7] + rnd_coefs[i,3]),
         col = "lightgrey")
abline(a = coef(m_fixed)[1] - coef(m_fixed)[3], 
       b = coef(m_fixed)[2] - coef(m_fixed)[4])

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
require(afex)
load("fitted_lmms.rda")

## ---- eval=FALSE---------------------------------------------------------
## require(afex)
## m_full <- mixed(dv ~ c_given_a*rel_cond*dv_question +
##                        (rel_cond*c_given_a|p_id) +
##                        (rel_cond*c_given_a*dv_question|i_id),
##                      dat,
##                      control = lmerControl(optCtrl = list(maxfun=1e8)),
##                      method = "S")
## m_full

## ---- echo=FALSE---------------------------------------------------------
m_full

## ------------------------------------------------------------------------
lsm.options(lmer.df = "asymptotic") # or "Kenward-Roger" or "Satterthwaite"
lstrends(m_full, "rel_cond", var = "c_given_a")

## ------------------------------------------------------------------------
# fixef(m_full$full_model)[2] + fixef(m_full$full_model)[6]
# fixef(m_full$full_model)[2] + fixef(m_full$full_model)[7]
fixef(m_full$full_model)[2] - fixef(m_full$full_model)[6] - fixef(m_full$full_model)[7]

## ---- echo=FALSE, message=FALSE, results='hide'--------------------------
require(sjstats)

## ------------------------------------------------------------------------
m1 <- lmer(dv ~ 1 + (1|p_id), datr)
# summary(m1)
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  p_id     (Intercept) 0.00572  0.0757  
#  Residual             0.14607  0.3822  
# Number of obs: 752, groups:  p_id, 94

0.00572 / (0.0057+0.1461)
require(sjstats)
icc(m1)

## ------------------------------------------------------------------------
m1 <- lmer(dv ~ 1 + (1|p_id), datr)
# summary(m1)
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  p_id     (Intercept) 0.00572  0.0757  
#  Residual             0.14607  0.3822  
# Number of obs: 752, groups:  p_id, 94

icc(m1)

## ------------------------------------------------------------------------
m2 <- lmer(dv ~ 1 + (rel_cond:c_given_a|p_id), 
           datr)
# summary(m2)
 # Groups   Name                 Variance Std.Dev. Corr       
 # p_id     (Intercept)          0.0398   0.200               
 #          rel_condPO:c_given_a 1.0186   1.009    -0.94      
 #          rel_condIR:c_given_a 0.3262   0.571    -0.48  0.75
 # Residual                      0.0570   0.239 
icc(m2)

## ---- fig.width=5, fig.height=4------------------------------------------
plot(m_max, 
     resid(.,scaled=TRUE) ~ c_given_a | rel_cond)

## ---- fig.width=4, fig.height=4------------------------------------------
lattice::qqmath(m_max)

## ---- eval=FALSE---------------------------------------------------------
## plot(m_max, p_id ~ resid(., scaled=TRUE) )
## plot(m_max, resid(., scaled=TRUE) ~ fitted(.) | rel_cond)
## ?plot.merMod

## ---- eval=FALSE, include=FALSE------------------------------------------
## require(afex)
## load("../exercises/ssk16_dat_preapred.rda")

## ---- eval=FALSE, include=FALSE------------------------------------------
## 
## m_full <- mixed(dv ~ c_given_a*rel_cond*dv_question +
##                        (rel_cond*c_given_a|p_id) +
##                        (rel_cond*c_given_a*dv_question|i_id),
##                      dat,
##                      control = lmerControl(optCtrl = list(maxfun=1e8)),
##                      method = "S")
## 
## save(m_full, file = "fitted_lmms.rda", compress = "xz")

