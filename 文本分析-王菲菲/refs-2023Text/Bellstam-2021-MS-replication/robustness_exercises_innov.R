## Script with robustness tests for "A Text-Based Analysis of Corporate Innovation" ##

library(foreign)
library("data.table")

## Reading in and merging different data sets
## dataset.csv is just the main innovation measure
## dft2 is the innovation measure merged with other standard firm characteristics
## dft2-no_grow_rev contains a version of the innovation measure without growth and revenue words in the corpus.
## dft2.K10 contains a version of the innov measure based on a 10-topic LDA.

innov = read.csv("dataset.csv", header=TRUE)

load("dft2.RData")
df_trim = dft2[,c("fyear", "gvkey", "ln_npats3_f","roa_l", "roa_l2", "roa_l3","roa_l4","roa_l5", "K12posmroot_z", "K5posmroot_z", "K6negmroot", "ln_ncites", "ln_mv", "n.analysts", "mforward_lookingness.z",
                  "rdat_l", "rdat_l2", "rdat_l3", "rdat_l4", "rdat_l5", "ln_ncites_l", "ln_ncites_l2", "ln_ncites_l3", "msent.z",
                  "ln_ncites_l4", "ln_ncites_l5", "ln_npats_l", "ln_npats_l2", "ln_npats_l3", "ln_npats_l4", "ln_npats_l5", "sentiment")]  ## tb-financing K5posmroot_z

load("dft2-no_gro_rev.RData")
df_trim_nogro = dft2[,c("fyear", "gvkey", "K6posmroot_z")]  ## innovation measure without "growth" or "revenue" in the corpus.

load("dft2.K10.RData")
df_trim_K10 = dft2[,c("fyear", "gvkey", "K6posmroot_z")]  ## 10-topic LDA
names(df_trim_K10) = c("fyear", "gvkey", "tb_innov10")

load("dft2.RData")

innov_mrg = merge(innov, df_trim)
innov_mrg = merge(innov_mrg, df_trim_nogro)
innov_mrg = merge(innov_mrg, df_trim_K10)
innov_mrg$sic2 = substring(innov_mrg$sic4, first=1,last=2)
innov_mrg$ind_yr = paste(innov_mrg$fyear, innov_mrg$sic2)
library(lfe)

## ------------------------------------ ##
## Main specification +
## +++ Robustness to re-estimating LDA  ##
## without "growth" and "revenue" in    ##
## corpus                               ##
## +++ Robustness to 10-topic LDA       ##
## ------------------------------------ ##

## Main Spec
myfelm1 = felm(roa_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                       lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(roa_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(roa_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## Dropping revenue and growth words 
myfelm1 = felm(roa_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(roa_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(roa_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## 10 topic LDA
myfelm1 = felm(roa_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(roa_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(roa_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)



## Main Spec
myfelm1 = felm(ln_q_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(ln_q_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(ln_q_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## Dropping revenue and growth words 
myfelm1 = felm(ln_q_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(ln_q_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(ln_q_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## 10 topic LDA
myfelm1 = felm(ln_q_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(ln_q_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(ln_q_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## Main Spec
myfelm1 = felm(salesgrowth_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(salesgrowth_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(salesgrowth_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## Dropping revenue and growth words 
myfelm1 = felm(salesgrowth_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(salesgrowth_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(salesgrowth_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)


## 10 topic LDA
myfelm1 = felm(salesgrowth_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(salesgrowth_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(salesgrowth_f ~ tb_innov10 + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## ------------------------------------ ##
## Horse race with "financing" topic    ##
## ------------------------------------ ##


## Main Spec
myfelm1 = felm(roa_f ~ tb_innov +K5posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(roa_f ~ tb_innov +K5posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(roa_f ~ tb_innov +K5posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)


summary(myfelm1)
summary(myfelm2)
summary(myfelm3)


## Main Spec
myfelm1 = felm(ln_q_f ~ tb_innov +K5posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(ln_q_f ~ tb_innov +K5posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(ln_q_f ~ tb_innov +K5posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)


## Main Spec
myfelm1 = felm(salesgrowth_f ~ tb_innov +K5posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(salesgrowth_f ~ tb_innov +K5posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(salesgrowth_f ~ tb_innov +K5posmroot_z + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)


## ------------------------------------ ##
## Test on the Negative Analyst Reports ##
## ------------------------------------ ##

innov_mrg$K6negmroot_z = scale(innov_mrg$K6negmroot)

## Main Spec
myfelm1 = felm(roa_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(roa_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(roa_f ~ scale(K6negmroot)+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)



myfelm1 = felm(roa_f ~ scale(K6negmroot)+K6posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(roa_f ~ scale(K6negmroot)+K6posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(roa_f ~ scale(K6negmroot)+K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## Main Spec
myfelm1 = felm(ln_q_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(ln_q_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(ln_q_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

## ------------------------------ ##
## Negative Text-Based Innovation ##
## ------------------------------ ##

load("dft2.RData")
dft2$K6negmroot_z = scale(dft2$K6negmroot)
dft2$neg_patent   = ifelse(dft2$patenter==1, dft2$K6negmroot_z, 0)
dft2$neg_nonpatent   = ifelse(dft2$patenter==0, dft2$K6negmroot_z, 0)
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## stargazer neg table
myfelm1 = felm(roa_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

myfelm4 = felm(ln_q_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6negmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)

summary(myfelm4)
summary(myfelm5)
summary(myfelm6)


myfelm7 = felm(salesgrowth_f ~ K6negmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)

summary(myfelm7)
summary(myfelm8)
summary(myfelm9)

library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6, myfelm7, myfelm8, myfelm9),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv)),
                          sqrt(diag(myfelm9$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv)),
                          myfelm9$coefficients/sqrt(diag(myfelm9$clustervcv))),
                order = c('K6negmroot_z','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$',
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$',
                                     'R\\&D/Assets (Z)$_t$',
                                     'Leverage$_t$',
                                     'Log(Assets)$_t$',
                                     'Log(Age)$_t$',
                                     'Cash/Assets$_t$',
                                     'Asset Tangibility$_t$', 'patenter'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)


## stargazer neg table
myfelm1 = felm(roa_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

myfelm4 = felm(ln_q_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)

summary(myfelm4)
summary(myfelm5)
summary(myfelm6)


myfelm7 = felm(salesgrowth_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)

summary(myfelm7)
summary(myfelm8)
summary(myfelm9)

## stargazer neg table x patenting firm
library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6, myfelm7, myfelm8, myfelm9),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv)),
                          sqrt(diag(myfelm9$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv)),
                          myfelm9$coefficients/sqrt(diag(myfelm9$clustervcv))),
                order = c('K6negmroot','K6negmroot:nonpatenter','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ No Patents',
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$',
                                     'R\\&D/Assets (Z)$_t$',
                                     'Leverage$_t$',
                                     'Log(Assets)$_t$',
                                     'Log(Age)$_t$',
                                     'Cash/Assets$_t$',
                                     'Asset Tangibility$_t$', 'patenter'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)





## Main Spec
myfelm1 = felm(salesgrowth_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = innov_mrg)
myfelm2 = felm(salesgrowth_f ~ scale(K6negmroot)+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = innov_mrg)
myfelm3 = felm(salesgrowth_f ~ scale(K6negmroot)+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)



## ------------------------- ##
## Forward looking split     ## 
## ------------------------- ##

innov_mrg$high_fwd = as.numeric(innov_mrg$mforward_lookingness.z>median(innov_mrg$mforward_lookingness.z))


myfelm1 = felm(roa_f ~ tb_innov +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])
myfelm2 = felm(roa_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])
myfelm3 = felm(roa_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])

myfelm4 = felm(ln_q_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])
myfelm5 = felm(ln_q_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])
myfelm6 = felm(ln_q_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])

myfelm7 = felm(salesgrowth_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])
myfelm8 = felm(salesgrowth_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])
myfelm9 = felm(salesgrowth_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |   gvkey,      data = innov_mrg[innov_mrg$high_fwd==1,])

library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6, myfelm7, myfelm8, myfelm9),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv)),
                          sqrt(diag(myfelm9$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv)),
                          myfelm9$coefficients/sqrt(diag(myfelm9$clustervcv))),
                order = c('tb_innov','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$',
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$',
                                     'R\\&D/Assets (Z)$_t$',
                                     'Leverage$_t$',
                                     'Log(Assets)$_t$',
                                     'Log(Age)$_t$',
                                     'Cash/Assets$_t$',
                                     'Asset Tangibility$_t$', 'patenter'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)


## ------------------------- ##
## Sentiment split           ##
## ------------------------- ##

innov_mrg$low_sent = as.numeric(innov_mrg$sentiment<median(innov_mrg$sentiment))


myfelm1 = felm(roa_f ~ tb_innov +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])
myfelm2 = felm(roa_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])
myfelm3 = felm(roa_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])

myfelm4 = felm(ln_q_f ~ tb_innov + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])
myfelm5 = felm(ln_q_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])
myfelm6 = felm(ln_q_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])

myfelm7 = felm(salesgrowth_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])
myfelm8 = felm(salesgrowth_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])
myfelm9 = felm(salesgrowth_f ~ tb_innov  + ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |   gvkey,      data = innov_mrg[innov_mrg$low_sent==1,])

library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6, myfelm7, myfelm8, myfelm9),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv)),
                          sqrt(diag(myfelm9$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv)),
                          myfelm9$coefficients/sqrt(diag(myfelm9$clustervcv))),
                order = c('tb_innov','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$',
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$',
                                     'R\\&D/Assets (Z)$_t$',
                                     'Leverage$_t$',
                                     'Log(Assets)$_t$',
                                     'Log(Age)$_t$',
                                     'Cash/Assets$_t$',
                                     'Asset Tangibility$_t$', 'patenter'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)


## ------------------------------------ ##
## Evidence on Dynamics of R&D & patenting    ##
## ------------------------------------ ##
innov_mrg = as.data.table(innov_mrg)
setkey(innov_mrg, gvkey)

## fill in zeros for non-patenting firms ##
innov_mrg$ln_npats_l[is.na(innov_mrg$ln_npats_l) & !is.na(innov_mrg$roa_l)]=0
innov_mrg$ln_ncites_l[is.na(innov_mrg$ln_ncites_l) & !is.na(innov_mrg$roa_l)]=0

innov_mrg$ln_npats_l2[is.na(innov_mrg$ln_npats_l2) & !is.na(innov_mrg$roa_l2)]=0
innov_mrg$ln_ncites_l2[is.na(innov_mrg$ln_ncites_l2) & !is.na(innov_mrg$roa_l2)]=0

innov_mrg$ln_npats_l3[is.na(innov_mrg$ln_npats_l3) & !is.na(innov_mrg$roa_l3)]=0
innov_mrg$ln_ncites_l3[is.na(innov_mrg$ln_ncites_l3) & !is.na(innov_mrg$roa_l3)]=0

innov_mrg$ln_npats_l4[is.na(innov_mrg$ln_npats_l4) & !is.na(innov_mrg$roa_l4)]=0
innov_mrg$ln_ncites_l4[is.na(innov_mrg$ln_ncites_l4) & !is.na(innov_mrg$roa_l4)]=0

## regressions
i_reg = innov_mrg[!is.na(innov_mrg$ln_npats_l4),]
i_reg$sic2 = substring(i_reg$sic4, first=1, last=2)
i_reg$ind_yr =  paste(i_reg$sic2, i_reg$fyear)    ## make sure this is all defined before running it

myfelm1 = felm(tb_innov~roa_l+roa_l2+roa_l3+roa_l4+ln_npats_l+ln_npats_l2+ln_npats_l3+ln_npats_l4+rdat_l+
                 +rdat_l2+rdat_l3++rdat_l4+patenter| fyear + sic4 | 0 |  gvkey, data = i_reg)

myfelm2 = felm(tb_innov~roa_l+roa_l2+roa_l3+roa_l4+ln_npats_l+ln_npats_l2+ln_npats_l3+ln_npats_l4+rdat_l+
                 +rdat_l2+rdat_l3++rdat_l4+patenter+ ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey, data = i_reg)

myfelm3 = felm(tb_innov~roa_l+roa_l2+roa_l3+roa_l4+ln_npats_l+ln_npats_l2+ln_npats_l3+ln_npats_l4+rdat_l+
                 +rdat_l2+rdat_l3++rdat_l4+patenter| fyear + gvkey | 0 | gvkey, data = i_reg)

myfelm4 = felm(tb_innov~roa_l+roa_l2+roa_l3+roa_l4+ln_npats_l+ln_npats_l2+ln_npats_l3+ln_npats_l4+rdat_l+
                 +rdat_l2+rdat_l3++rdat_l4+patenter+ ln_at + ppentat +      
                   lev + ln_age + cashat| fyear + gvkey | 0 | gvkey, data = i_reg)

myfelm5 = felm(tb_innov~roa_l+roa_l2+roa_l3+roa_l4+ln_npats_l+ln_npats_l2+ln_npats_l3+ln_npats_l4+rdat_l+
                 +rdat_l2+rdat_l3++rdat_l4+patenter + ln_at + ppentat +      
                   lev + ln_age + cashat| ind_yr + gvkey | 0 | gvkey, data = i_reg)


## Dynamics Table 3
library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5),
                dep.var.labels = c('Text-Based Innovation (Z)$_{t}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv))),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)


## Build the table out of this.
summary(myfelm3)
summary(myfelm3_0)
summary(myfelm3_1)
summary(myfelm3_2)
summary(myfelm3_3)

## Correlations with other topics regarding ROA & Q ##

load("dft2.RData")
namz = c("ln_npats_f","rdat_f","roa_f", "ln_q_f", "K1m","K2m","K3m","K4m","K5m","K6m","K7m", "K8m",
  "K9m", "K10m", "K11m", "K12m", "K13m", "K14m","K15m")
dft2 = as.data.frame(dft2)
cor_dat = dft2[,namz]

corzz = cor(cor_dat[,  c("roa_f", "K1m","K2m","K3m","K4m","K5m","K6m","K7m", "K8m",
                      "K9m", "K10m", "K11m", "K12m", "K13m", "K14m","K15m")], use="complete.obs")[1,]

sort(corzz)


## R&D Specifications in Appendix ##

fmo1 = felm(rdat ~ K6posmroot_z | fyear+sic4 | 0 | gvkey + fyear ,data = dft2)
summary(fmo1)

fmo2 = felm(rdat ~ K6posmroot_z +  ln_npats + ln_ncites   + ln_at + ppentat +      
              lev + ln_age + cashat + roa+ln_q | fyear + sic4 | 0 | gvkey + fyear, data = dft2)
summary(fmo2)


fmo3 = felm(rdat_f~ K6posmroot_z | fyear+sic4 | 0 | gvkey + fyear ,data = dft2)
summary(fmo3)

fmo4 = felm(rdat_f~ K6posmroot_z +  ln_npats + ln_ncites   + ln_at + ppentat +      
              lev + ln_age + cashat + roa+ln_q +rdat | fyear + sic4 | 0 | gvkey + fyear, data = dft2)
summary(fmo4)

## --------------------- ##
## K=50 topic robustness ##
## --------------------- ##
load("dft2_50Kv1.RData")
dft2$ind_yr = paste(dft2$sic4, dft2$fyear)

## Main Spec
myfelm1 = felm(roa_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ patentk6rootz + nonpatentk6rootz+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)

myfelm3 = felm(ln_q_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm4 = felm(ln_q_f ~ patentk6rootz + nonpatentk6rootz+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)

myfelm5 = felm(salesgrowth_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm6 = felm(salesgrowth_f ~ patentk6rootz + nonpatentk6rootz+ ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)


summary(myfelm1)
summary(myfelm2)
summary(myfelm3)
summary(myfelm4)
summary(myfelm5)
summary(myfelm6)
