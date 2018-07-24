## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
# see: https://github.com/yihui/xaringan
# install.packages("xaringan")
# see: 
# https://github.com/yihui/xaringan/wiki
# https://github.com/gnab/remark/wiki/Markdown
options(width=110)
options(digits = 4)

## ---- eval = FALSE-------------------------------------------------------
## ---
## title: "My Title"
## author: "Henrik Singmann"
## date: "`r format(Sys.time(), '%d %B, %Y')`"
## output:
##   html_document:
##     toc: TRUE
##     toc_float: true
##     theme: paper
##     highlight: espresso
## ---

## ---- echo=FALSE---------------------------------------------------------
1 + 1

## ---- eval=FALSE---------------------------------------------------------
## iris

## ---- eval=TRUE, echo=FALSE----------------------------------------------
options(width = 50)
iris[1:5, 1:3] # [...]

## ---- eval=TRUE----------------------------------------------------------
iris$Spec

## ---- eval=TRUE----------------------------------------------------------
library("tibble") 
iris2 <- as_tibble(iris)
iris2
iris2$Spec

## ---- eval=FALSE---------------------------------------------------------
## x %>% f
## x %>% f(y)
## x %>% f %>% g %>% h
## 
## x %>% f(y, .)
## x %>% f(y, z = .)

## ---- eval=FALSE---------------------------------------------------------
## f(x)
## f(x, y)
## h(g(f(x)))
## 
## f(y, x)
## f(y, z = x)

## ---- eval=FALSE---------------------------------------------------------
## library(magrittr)
## iris2$Sepal.Length %>%
##   mean

## ---- message=FALSE------------------------------------------------------
library("dplyr")
iris2 %>% 
  filter(Species == "setosa") %>% 
  summarise(mean(Sepal.Length))

## ------------------------------------------------------------------------
iris2 %>%
  group_by(Species) %>% 
  summarise(mean_l = mean(Sepal.Length),
            max_l = max(Sepal.Length),
            min_l = min(Sepal.Length),
            sd_l = sd(Sepal.Length))

## ---- eval=FALSE---------------------------------------------------------
## library("ggplot2")
## ggplot(iris2, aes(x = Petal.Width, y = Petal.Length)) +
##   geom_point()

## ---- eval=FALSE---------------------------------------------------------
## ggplot(iris2, aes(x = Petal.Width, y = Petal.Length, color = Species)) +
##   geom_point()

## ---- eval=FALSE---------------------------------------------------------
## ggplot(iris2, aes(x = Species, y = Petal.Length)) +
##   geom_jitter(width = 0.2) +
##   geom_boxplot(fill = "transparent") +
##   theme_bw()

