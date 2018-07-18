# This file prepares the data as described in https://osf.io/j4swp/
# With a few additional additions

# you might need to set the correct working directory via the menu: 
# Session - Set Working Directory - To Source File Location

load("ssk16_dat_online.rda") # data comes in 4 data frames per
# dw_1$group <-"P(if,then)"
# dw_2$group <-"Acc(if,then)"
# dw_3$group <-"P(Even)"
# dw_4$group <-"Acc(Even)"

dw_1$dv_question <- "probability"
dw_2$dv_question <- "acceptability"
dw_3$dv_question <- "probability"
dw_4$dv_question <- "acceptability"

dw_1$conditional <- "indicative"
dw_2$conditional <- "indicative"
dw_3$conditional <- "concessive"
dw_4$conditional <- "concessive"

dw_1$lfdn <- factor(paste(as.character(dw_1$lfdn), "P(if,then)", sep ="_"))
dw_2$lfdn <- factor(paste(as.character(dw_2$lfdn), "Acc(if,then)", sep ="_"))
dw_3$lfdn <- factor(paste(as.character(dw_3$lfdn), "P(Even)", sep ="_"))
dw_4$lfdn <- factor(paste(as.character(dw_4$lfdn), "Acc(Even)", sep ="_"))

names(dw_1)[names(dw_1) == 'P'] <- 'DV'
names(dw_2)[names(dw_2) == 'ACC'] <- 'DV'
names(dw_3)[names(dw_3) == 'PEven'] <- 'DV'
names(dw_4)[names(dw_4) == 'ACCEven'] <- 'DV'

dw <- rbind(dw_1, dw_2, dw_3, dw_4) 

# center IVs and DV at midpoint of scale
dat <- within(dw, {
  c_given_a <- (CgivenA-50)/100
  dv <- (DV-50)/100
  #group <- factor(group, levels = c("P(if,then)", "Acc(if,then)", "P(Even)", "Acc(Even)"))
  dv_question <- factor(dv_question, levels = c("probability", "acceptability"))
  conditional <- factor(conditional, levels = c("indicative", "concessive"))
})

dat$AC <- NULL
dat$conclusion <- NULL

dat <- droplevels(dat[ dat$conditional == "indicative", ])
dat$conditional <- NULL
dat$type <- NULL

dat <- dplyr::rename(dat, p_id = lfdn, i_id = le_nr)
length(levels(dat$p_id))

save(dat, file="ssk16_dat_preapred.rda")

dat <- droplevels(dat[ dat$dv_question == "probability", ])
dat$dv_question <- NULL

save(dat, file="ssk16_dat_preapred_ex1.rda")

### latest preparation (July 2018)

library("tidyverse")

dat <- dat %>% 
  rename(B_given_A = CgivenA,
         if_A_then_B = DV,
         B_given_A_c = c_given_a,
         if_A_then_B_c = dv) %>% 
  select(p_id, i_id, B_given_A, B_given_A_c, if_A_then_B, if_A_then_B_c, rel_cond) 

save(dat, file = "ssk16_dat_tutorial.rda")