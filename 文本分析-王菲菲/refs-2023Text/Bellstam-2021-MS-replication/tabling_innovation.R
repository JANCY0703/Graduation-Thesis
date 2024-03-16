## ------------------------------------------------------------------------- ##
## Script to generate the main results and select appendix exercises from    ##
## "A Text-Based Analysis of Corporate Innovation                            ##
##                                                                           ##
## Given a data merge between our measure (provided in the data disclosure)  ##
## and the relevant Compustat and patenting variables, the researcher can    ##
## construct an R Workspace, with a data frame called dft2                   ##
##                                                                           ##
## With that appropriatedly merged data frame, the following commands will   ##
## reproduce the main tables in our paper.                                   ##
## ------------------------------------------------------------------------- ##

library(lfe)
library(stargazer)
library("data.table")

## load("dft2.RData") ## this is an R workspace with Compustat measures merged onto the innovation data file we
## have provided.
## K6posmroot_z is our tb_innov measure
## K6negmroot_z is the negative tb_innov measure
dft2$K6negmroot_z = scale(dft2$K6negmroot)   ## negative tb-innovation measure
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## ------------------------------ ##
## Table 2, Panel A               ##
## Positive Text-Based Innovation ##
## ------------------------------ ##

myfelm1 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |   gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = dft2)

myfelm4 = felm(ln_q_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6posmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm7 = felm(salesgrowth_f ~ K6posmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


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
                order = c('K6posmroot_z','ln_npats',
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

## -------------------- ##
## Table 2, Panel B     ##
## stargazer pos table  ##  R&R actions: cluster at firm level, drop ln_pats & ln_cites
## -------------------- ##

myfelm1 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


myfelm4 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)



myfelm7 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


## stargazer pos table x patenting firm
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
                order = c('K6posmroot','K6posmroot:nonpatenter','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ No Patents',
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


## ------------------------------------##
## Table 3                             ##
## ------------------------------------##

## innov_mrg is the same as dft2 above, but merging several different "robustness" versions of 
## the innovation measure into the same data frame.

## regressions
i_reg = innov_mrg[!is.na(innov_mrg$ln_npats_l4),]  ## focus on complete data with respect to lags.

myfelm3 = felm(tb_innov~ln_npats_l+rdat_l+roa_l+
                 +ln_npats_l2+rdat_l2+roa_l2+
                 +ln_npats_l3+rdat_l3+roa_l3++ln_npats_l4+rdat_l4+roa_l4+patenter| fyear + sic4 | 0 | fyear + gvkey, data = i_reg)

myfelm3_0 = felm(tb_innov~ln_npats_l+rdat_l+roa_l+
                   +ln_npats_l2+rdat_l2+roa_l2+
                   +ln_npats_l3+rdat_l3+roa_l3++ln_npats_l4+rdat_l4+roa_l4+patenter+ ln_at + ppentat +      
                   lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey, data = i_reg)

myfelm3_1 = felm(tb_innov~ln_npats_l+rdat_l+roa_l+
                   +ln_npats_l2+rdat_l2+roa_l2+
                   +ln_npats_l3+rdat_l3+roa_l3++ln_npats_l4+rdat_l4+roa_l4+patenter| fyear + gvkey | 0 | fyear + gvkey, data = i_reg)

myfelm3_2 = felm(tb_innov~ln_npats_l+rdat_l+roa_l+
                   +ln_npats_l2+rdat_l2+roa_l2+
                   +ln_npats_l3+rdat_l3+roa_l3++ln_npats_l4+rdat_l4+roa_l4+patenter+ ln_at + ppentat +      
                   lev + ln_age + cashat| fyear + gvkey | 0 | fyear + gvkey, data = i_reg)

## Build the table out of this.
summary(myfelm3)
summary(myfelm3_0)
summary(myfelm3_1)
summary(myfelm3_2)

## ------------------------------ ##
## Table 6, Panel A               ##
## Negative Text-Based Innovation ##
## ------------------------------ ##

myfelm1 = felm(roa_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


myfelm4 = felm(ln_q_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6negmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = dft2)


myfelm7 = felm(salesgrowth_f ~ K6negmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6negmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


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

## -------------------- ##
## Table 6, Panel B     ##
## -------------------- ##

myfelm1 = felm(roa_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |   gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6negmroot_z + K6negmroot_z:nonpatenter+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm4 = felm(ln_q_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)



myfelm7 = felm(salesgrowth_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | fyear + gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear + gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6negmroot_z + K6negmroot_z:nonpatenter + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear + gvkey,      data = dft2)


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


## Robustness exercises on main result follow.. ##

# load("dft2rolling.RData")  ## load workspace with Compustat measures and a rolling version of 
# the innovation measure merged together.
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## ------------------------------ ##
## Table 4, Panel A               ##
## Positive Text-Based Innovation ##
## Rolling...                     ##
## ------------------------------ ##

myfelm1 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z +K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


myfelm3 = felm(ln_q_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |   gvkey,      data = dft2)
myfelm4 = felm(ln_q_f ~ K6posmroot_z +K6posmroot_z:nonpatenter+ rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm5 = felm(salesgrowth_f ~ K6posmroot_z +ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(salesgrowth_f ~ K6posmroot_z +K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv))),
                order = c('K6posmroot_z','K6posmroot_z:nonpatenter','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'nonpatenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ Non-Patenting Firm',
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


## ------------------------------ ##
## Table 4, Panel B               ##
## Positive Text-Based Innovation ##
## K=10 topics                    ##
## ------------------------------ ##

## load("dft2.K10.RData") R Workspace with tb_innov measure constructed from 10-topic LDA ##
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

myfelm1 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |   gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z +K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm3 = felm(ln_q_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = dft2)
myfelm4 = felm(ln_q_f ~ K6posmroot_z +K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm5 = felm(salesgrowth_f ~ K6posmroot_z +ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(salesgrowth_f ~ K6posmroot_z +K6posmroot_z:nonpatenter+ rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv))),
                order = c('K6posmroot_z','K6posmroot_z:nonpatenter','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'nonpatenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ Non-Patenting Firm',
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

## ---------------------------- ##
## Table 4, Panel C             ##
## Controlling for other topics ##
## ---------------------------- ##

# load("dft2.RData") ## Original R Workspace, which has controls for other topic loadings ##
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

myfelm1 = felm(roa_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +K1mroot4+K2mroot4+K3mroot4+K4mroot4+K5mroot4+K7mroot4+
                 +K8mroot4+K9mroot4+K10mroot4+K11mroot4+K12mroot4+K13mroot4+K14mroot4+
                 K15mroot4| ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z +K6posmroot_z:nonpatenter+ rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +K1mroot4+K2mroot4+K3mroot4+K4mroot4+K5mroot4+K7mroot4+
                 +K8mroot4+K9mroot4+K10mroot4+K11mroot4+K12mroot4+K13mroot4+K14mroot4+
                 K15mroot4| ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm3 = felm(ln_q_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +K1mroot4+K2mroot4+K3mroot4+K4mroot4+K5mroot4+K7mroot4+
                 +K8mroot4+K9mroot4+K10mroot4+K11mroot4+K12mroot4+K13mroot4+K14mroot4+
                 K15mroot4 | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm4 = felm(ln_q_f ~ K6posmroot_z +K6posmroot_z:nonpatenter+ rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +K1mroot4+K2mroot4+K3mroot4+K4mroot4+K5mroot4+K7mroot4+
                 +K8mroot4+K9mroot4+K10mroot4+K11mroot4+K12mroot4+K13mroot4+K14mroot4+
                 K15mroot4 | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm5 = felm(salesgrowth_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +K1mroot4+K2mroot4+K3mroot4+K4mroot4+K5mroot4+K7mroot4+
                 +K8mroot4+K9mroot4+K10mroot4+K11mroot4+K12mroot4+K13mroot4+K14mroot4+
                 K15mroot4 | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(salesgrowth_f ~ K6posmroot_z +K6posmroot_z:nonpatenter+rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +K1mroot4+K2mroot4+K3mroot4+K4mroot4+K5mroot4+K7mroot4+
                 +K8mroot4+K9mroot4+K10mroot4+K11mroot4+K12mroot4+K13mroot4+K14mroot4+
                 K15mroot4| ind_yr + gvkey | 0 |   gvkey,      data = dft2)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv))),
                order = c('K6posmroot_z','K6posmroot_z:nonpatenter','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'nonpatenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ Non-Patenting Firm',
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


## ----------------------------- ##
## Table 5                       ##
## Alternative Interpretations   ##
## ----------------------------- ##

## load("dft2.RData")  ## Re-load original R Workspace to safeguard against overwrite issues

dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## compute standardized versions of revenue words, growth words and technology words ##
dft2[, revm.z := revm/sd(revm, na.rm=T)]
dft2[, grom.z := grom/sd(grom, na.rm=T)]
dft2[, techm.z := techm/sd(techm, na.rm=T)]


## Main Spec ## all word counts
myfelm1 = felm(roa_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +revm.z+grom.z+msent.z+techm.z| ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +revm.z+grom.z+msent.z+techm.z| ind_yr + gvkey | 0 | gvkey,      data = dft2)

myfelm3 = felm(ln_q_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +revm.z+grom.z+msent.z+techm.z| ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm4 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +revm.z+grom.z+msent.z+techm.z| ind_yr + gvkey | 0 | gvkey,      data = dft2)

myfelm5 = felm(salesgrowth_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +revm.z+grom.z+msent.z+techm.z| ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat +revm.z+grom.z+msent.z+techm.z| ind_yr + gvkey | 0 |  gvkey,      data = dft2)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6),
                dep.var.labels = c('Return on Assets$_{t+1}$', 'Log(Q)$_{t+1}$','Sales Growth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv))),
                order = c('K6posmroot_z','K6posmroot_z:nonpatenter',"msent.z","revm.z","grom.z", "techm.z",
                          'ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'nonpatenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ Non-Patenting Firm',
                                     'Sentiment(Z)$_t$','Revenue Words (Z)$_t$',
                                     'Growth Words (Z)$_t$',
                                     'Technology Words (Z)$_t$',
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


## -------------------------- ##
## Table 7, Panels A & B      ##
## Text innovation            ##  
## Innovation outcomes        ##
## -------------------------- ##

## load("dft2.RData") ## Re-load original R Workspace to build in redundancy against variable overwrite issues.

dft2$K6negmroot_z = scale(dft2$K6negmroot)   ## negative tb-innovation measure
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## Panel A. patents and citation impact
myfelm1 = felm(ln_npats3_f ~ K6posmroot_z | fyear+sic4 | 0 | gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm1)

myfelm2 = felm(ln_npats3_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm2)


myfelm3 = felm(ln_npats3_f ~ K6posmroot_z | ind_yr+gvkey | 0 | gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm3)

myfelm4 = felm(ln_npats3_f ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm4)


myfelm5 = felm(ln_ncites_to_patents_f3 ~ K6posmroot_z | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm5)

myfelm6 = felm(ln_ncites_to_patents_f3 ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm6)


myfelm7 = felm(ln_ncites_to_patents_f3 ~ K6posmroot_z | ind_yr+gvkey | 0 | gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm7)

myfelm8 = felm(ln_ncites_to_patents_f3 ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm8)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6,myfelm7, myfelm8),
                dep.var.labels = c('Logged Patents$_{t+1 \\rightarrow t+3}$', 'Logged \\frac{Citations$_{t+1 \\rightarrow t+3}$}{Patents$_{t+1 \\rightarrow t+3}$}'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv))),
                order = c('K6posmroot_z',
                          'ln_npats',
                          'ln_ncites'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)

## Panel B.  innovation patent value and products
myfelm1 = felm(ln_xis ~ K6posmroot_z | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm1)

myfelm2 = felm(ln_xis ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm2)


myfelm3 = felm(ln_xis ~ K6posmroot_z | ind_yr+gvkey | 0 | gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm3)

myfelm4 = felm(ln_xis ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm4)


myfelm5 = felm(ln.pann ~ K6posmroot_z | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm5)

myfelm6 = felm(ln.pann ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm6)


myfelm7 = felm(ln.pann ~ K6posmroot_z | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm7)

myfelm8 = felm(ln.pann ~ K6posmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm8)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6,myfelm7, myfelm8),
                dep.var.labels = c('Logged Patent Value', 'Logged Product Announcements'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv))),
                order = c('K6posmroot_z',
                          'ln_npats',
                          'ln_ncites'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)





## -------------------------- ##
## Table A.16, Panels A & B   ##
## Negative innovation        ##
## Innovation outcomes        ##
## -------------------------- ##

## load("dft2.RData") ## Re-load original R Workspace to build in redundancy against variable overwrite issues.
dft2$K6negmroot_z = scale(dft2$K6negmroot)   ## negative tb-innovation measure
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## Panel A. patents and citation impact
myfelm1 = felm(ln_npats3_f ~ K6negmroot_z | fyear+sic4 | 0 | gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm1)

myfelm2 = felm(ln_npats3_f ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
              lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm2)


myfelm3 = felm(ln_npats3_f ~ K6negmroot_z | ind_yr+gvkey | 0 | gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm3)

myfelm4 = felm(ln_npats3_f ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
              lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm4)


myfelm5 = felm(ln_ncites_to_patents_f3 ~ K6negmroot_z | fyear+sic4 | 0 |  gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm5)

myfelm6 = felm(ln_ncites_to_patents_f3 ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
              lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm6)


myfelm7 = felm(ln_ncites_to_patents_f3 ~ K6negmroot_z | ind_yr+gvkey | 0 | gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm7)

myfelm8 = felm(ln_ncites_to_patents_f3 ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
              lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
            data = dft2[patenter == 1L])
summary(myfelm8)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6,myfelm7, myfelm8),
                dep.var.labels = c('Logged Patents$_{t+1 \\rightarrow t+3}$', 'Logged \\frac{Citations$_{t+1 \\rightarrow t+3}$}{Patents$_{t+1 \\rightarrow t+3}$}'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv))),
                order = c('K6negmroot_z',
                          'ln_npats',
                          'ln_ncites'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)

## Panel B. "negative" innovation patent value and products
myfelm1 = felm(ln_xis ~ K6negmroot_z | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm1)

myfelm2 = felm(ln_xis ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm2)


myfelm3 = felm(ln_xis ~ K6negmroot_z | ind_yr+gvkey | 0 | gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm3)

myfelm4 = felm(ln_xis ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm4)


myfelm5 = felm(ln.pann ~ K6negmroot_z | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm5)

myfelm6 = felm(ln.pann ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | fyear+sic4 | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm6)


myfelm7 = felm(ln.pann ~ K6negmroot_z | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm7)

myfelm8 = felm(ln.pann ~ K6negmroot_z + ln_npats + ln_ncites + rdatz  + ln_at + ppentat +      
                 lev + ln_age + cashat  | ind_yr+gvkey | 0 |  gvkey ,
               data = dft2[patenter == 1L])
summary(myfelm8)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6,myfelm7, myfelm8),
                dep.var.labels = c('Logged Patent Value', 'Logged Product Announcements'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv))),
                order = c('K6negmroot_z',
                          'ln_npats',
                          'ln_ncites'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 
                                     'Log(Patents) $_t$',
                                     'Log(Citations)$_t$'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)



## long-term appendix table
myfelm1 = felm(roa_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)

myfelm2 = felm(roa_f2 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)

myfelm3 = felm(roa_f3 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)

myfelm4 = felm(roa_f4 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)

myfelm5 = felm(ln_q_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)

myfelm6 = felm(ln_q_f2 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)

myfelm7 = felm(ln_q_f3 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)

myfelm8 = felm(ln_q_f4 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)




library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6,myfelm7, myfelm8),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv)),
                          sqrt(diag(myfelm7$clustervcv)),
                          sqrt(diag(myfelm8$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv)),
                          myfelm7$coefficients/sqrt(diag(myfelm7$clustervcv)),
                          myfelm8$coefficients/sqrt(diag(myfelm8$clustervcv))),
               
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)


## long-term appendix table
myfelm1 = felm(salesgrowth_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |   gvkey,      data = dft2)

myfelm2 = felm(salesgrowth_f2 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)

myfelm3 = felm(salesgrowth_f3 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)

myfelm4 = felm(salesgrowth_f4 ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 | gvkey,      data = dft2)

library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv))),
                
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)

## -------------------------- ##
## K=50 Topic Robustness      ##
## -------------------------- ##
# load("dft2_50Kv1.RData")
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

## Main Spec
myfelm1 = felm(roa_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm3 = felm(ln_q_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)
myfelm4 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

myfelm5 = felm(salesgrowth_f ~ K6posmroot_z+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = dft2)
myfelm6 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ ln_npats + ln_ncites + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | gvkey,      data = dft2)


summary(myfelm1)
summary(myfelm2)
summary(myfelm3)
summary(myfelm4)
summary(myfelm5)
summary(myfelm6)


library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6),
                dep.var.labels = c('ROA$_{t+1}$', 'Log(Q$_{t+1}$', 'Salesgrowth$_{t+1}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv))),
                order = c('K6posmroot_z',
                          'K6posmroot_z:nonpatenter',
                          'ln_ncites'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 
                                     '$\times$ Non-Patenting Firm',
                                     'Log(Citations)$_t$'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)

## --------------------------- ##
## Not controlling for patents ##
## --------------------------- ##


# load("dft2.RData")
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)


myfelm1 = felm(roa_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

myfelm4 = felm(ln_q_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6posmroot_z + + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

summary(myfelm4)
summary(myfelm5)
summary(myfelm6)


myfelm7 = felm(salesgrowth_f ~ K6posmroot_z + + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

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
                order = c('K6posmroot_z',
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$',
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


## ---------------------------------------------------- ##
## Controlling for patents in interactive specification ##
## ---------------------------------------------------- ##

## stargazer pos table
myfelm1 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites | fyear + sic4 | 0 |   gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+  rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | fyear + gvkey | 0 | gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | ind_yr + gvkey | 0 | gvkey,      data = dft2)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

myfelm4 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites | fyear + sic4 | 0 |  gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter++ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | fyear + gvkey | 0 | gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ +ln_npats + ln_ncites+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

summary(myfelm4)
summary(myfelm5)
summary(myfelm6)


myfelm7 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+  rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | fyear + sic4 | 0 |   gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+  rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | fyear + gvkey | 0 |  gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat + ln_npats + ln_ncites  | ind_yr + gvkey | 0 |  gvkey,      data = dft2)

summary(myfelm7)
summary(myfelm8)
summary(myfelm9)

## stargazer pos table x patenting firm
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
                order = c('K6posmroot','K6posmroot:nonpatenter',
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'nonpatenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ No Patents',
                                     
                                     'R\\&D/Assets (Z)$_t$',
                                     'Leverage$_t$',
                                     'Log(Assets)$_t$',
                                     'Log(Age)$_t$',
                                     'Cash/Assets$_t$',
                                     'Asset Tangibility$_t$', 'nonpatenter'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)

## Robustness -- continuous patenting interaction ##


## stargazer pos table
myfelm1 = felm(roa_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites  | fyear + sic4 | 0 |   gvkey,      data = dft2[patenter == 1L])
myfelm2 = felm(roa_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites   | fyear + gvkey | 0 | gvkey,      data = dft2[patenter == 1L])
myfelm3 = felm(roa_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites   | ind_yr + gvkey | 0 | gvkey,      data = dft2[patenter == 1L])

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)

myfelm4 = felm(ln_q_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites | fyear + sic4 | 0 |  gvkey,      data = dft2[patenter == 1L])
myfelm5 = felm(ln_q_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites  | fyear + gvkey | 0 | gvkey,      data = dft2[patenter == 1L])
myfelm6 = felm(ln_q_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites   | ind_yr + gvkey | 0 |  gvkey,      data = dft2[patenter == 1L])

summary(myfelm4)
summary(myfelm5)
summary(myfelm6)


myfelm7 = felm(salesgrowth_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites  | fyear + sic4 | 0 |   gvkey,      data = dft2[patenter == 1L])
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites  | fyear + gvkey | 0 |  gvkey,      data = dft2[patenter == 1L])
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z*scale(ln_npats)+ rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat  + ln_ncites | ind_yr + gvkey | 0 |  gvkey,      data = dft2[patenter == 1L])

summary(myfelm7)
summary(myfelm8)
summary(myfelm9)


## stargazer pos table x patenting firm
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
                order = c('K6posmroot','K6posmroot:nonpatenter',
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'nonpatenter'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)



## ------------------------------ ##
## Robustness Double clustering   ##
## Positive Text-Based Innovation ##
## ------------------------------ ##

# load("dft2.RData")
dft2$K6negmroot_z = scale(dft2$K6negmroot)   ## negative tb-innovation measure
dft2$ind_yr = paste(dft2$sic2, dft2$fyear)

myfelm1 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |   fyear+gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 | fyear+gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 | fyear+gvkey,      data = dft2)

myfelm4 = felm(ln_q_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  fyear+gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6posmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  fyear+gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  fyear+gvkey,      data = dft2)

myfelm7 = felm(salesgrowth_f ~ K6posmroot_z +ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  fyear+gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  fyear+gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z+ln_npats + ln_ncites + rdatz + patenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  fyear+gvkey,      data = dft2)


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
                order = c('K6posmroot_z','ln_npats',
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

## ------------------
## Double Clustering Panel b    ##
## stargazer pos table  ##  R&R actions: cluster at firm level, drop ln_pats & ln_cites
## -------------------- ##

myfelm1 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  fyear+gvkey,      data = dft2)
myfelm2 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  fyear+gvkey,      data = dft2)
myfelm3 = felm(roa_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  fyear+gvkey,      data = dft2)


myfelm4 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  fyear+gvkey,      data = dft2)
myfelm5 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  fyear+gvkey,      data = dft2)
myfelm6 = felm(ln_q_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  fyear+gvkey,      data = dft2)



myfelm7 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter+ rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + sic4 | 0 |  fyear+gvkey,      data = dft2)
myfelm8 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | fyear + gvkey | 0 |  fyear+gvkey,      data = dft2)
myfelm9 = felm(salesgrowth_f ~ K6posmroot_z + K6posmroot_z:nonpatenter + rdatz + nonpatenter + ln_at + ppentat +      
                 lev + ln_age + cashat | ind_yr + gvkey | 0 |  fyear+gvkey,      data = dft2)


## stargazer pos table x patenting firm
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
                order = c('K6posmroot','K6posmroot:nonpatenter','ln_npats',
                          'ln_ncites', 
                          'rdatz', 'lev', 'ln_at', 'ln_age',  'cashat', 'ppentat', 'patenter'),
                covariate.labels = c('Text-Based Innovation (Z)$_t$', 'Text-Based Innovation (Z)$_t$ $\\times$ No Patents',
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



## ------------------------------ ##
## Generalists vs. Specialists    ##
## ------------------------------ ##


library(foreign)
library("data.table")
gen_spec = read.dta("Custodio_Ferreira_Matos.dta")  ## downloaded from Custodio, Ferreira and Matos data disclosure
innov = read.csv("dataset.csv", header=TRUE)        ## "dataset.csv" is the data set with the text-based innovation measure in the shared folder

names(gen_spec) = c("exec_name", "coname", "gvkey", "execid", "fyear", "ga")
innov_mrg = merge(innov, gen_spec)

## load("dft2.RData")  Full R Workspace from before with compustat variables to merge on
dft2 = as.data.table(dft2)
df_trim = dft2[,c("fyear", "gvkey", "ln_ncites", "ln_mv", "n.analysts", "K6negmroot", "nonpatenter")]

innov_mrg = merge(innov_mrg, df_trim)
innov_mrg$sic2 = substring(innov_mrg$sic4, first=1, last=2)
innov_mrg$ind_yr = paste(innov_mrg$fyear, innov_mrg$sic2)
library(lfe)

## Panel A with firm FE
myfelm1 = felm(ln_npats~ga | ind_yr +fyear+gvkey| 0 | gvkey, data=innov_mrg)
myfelm2 = felm(ln_npats~ga +ln_at+ppentat+lev+cashat+patenter | ind_yr+fyear+gvkey | 0 | gvkey, data=innov_mrg)
myfelm3 = felm(ln_ncites~ga | ind_yr +fyear+gvkey| 0 | gvkey, data=innov_mrg)
myfelm4 = felm(ln_ncites~ga +ln_at+ppentat+lev+cashat+patenter  | ind_yr+fyear+gvkey  | 0 | gvkey, data=innov_mrg)
myfelm5 = felm(tb_innov~ga | ind_yr+fyear+gvkey | 0 | gvkey, data=innov_mrg)
myfelm6 = felm(tb_innov~ga +ln_at+ppentat+lev+cashat+patenter   | ind_yr+fyear+gvkey | 0 | gvkey, data=innov_mrg)

summary(myfelm1)
summary(myfelm2)
summary(myfelm3)
summary(myfelm4)
summary(myfelm5)
summary(myfelm6)

## Below for bootstrap tests described in Section 5 of the paper. ##
B = 500 
out_stat = rep(NA,B)

for(b in 1:B){
  this_boot_obs = sample(1:nrow(innov_mrg), replace=T)
  boot_dat = innov_mrg[this_boot_obs,]
  bmyfelm2 = felm(ln_npats~ga +ln_at+ppentat+lev+cashat+patenter | ind_yr+fyear+gvkey | 0 | gvkey, data=boot_dat)
  bmyfelm6 = felm(tb_innov~ga +ln_at+ppentat+lev+cashat+patenter   | ind_yr+fyear+gvkey | 0 | gvkey, data=boot_dat)
  b_stat = coef(bmyfelm6)["ga"]-coef(bmyfelm2)["ga"] 
  out_stat[b] = b_stat
}
diff_stat = coef(myfelm6)["ga"] - coef(myfelm2)["ga"]
se_diff = sd(out_stat)
t_stat = diff_stat/se_diff
p_val =2*pnorm(abs(t_stat), lower.tail=FALSE )

c(diff_stat, se_diff, t_stat, p_val)


## stargazer pos table x patenting firm
library(stargazer)
tbl = stargazer(list(myfelm1, myfelm2, myfelm3, myfelm4, myfelm5, myfelm6),
                dep.var.labels = c('log Patents$_{t}$', 'log Citations$_{t}$','tb innovation$_{t}$'),
                se = list(sqrt(diag(myfelm1$clustervcv)),
                          sqrt(diag(myfelm2$clustervcv)),
                          sqrt(diag(myfelm3$clustervcv)),
                          sqrt(diag(myfelm4$clustervcv)),
                          sqrt(diag(myfelm5$clustervcv)),
                          sqrt(diag(myfelm6$clustervcv))),
                t =  list(myfelm1$coefficients/sqrt(diag(myfelm1$clustervcv)),
                          myfelm2$coefficients/sqrt(diag(myfelm2$clustervcv)),
                          myfelm3$coefficients/sqrt(diag(myfelm3$clustervcv)),
                          myfelm4$coefficients/sqrt(diag(myfelm4$clustervcv)),
                          myfelm5$coefficients/sqrt(diag(myfelm5$clustervcv)),
                          myfelm6$coefficients/sqrt(diag(myfelm6$clustervcv))),
                order = c('ga'),
                covariate.labels = c('GA Index'),
                add.lines = list(
                  c("Copy Table Pattern Here", "X", "X", "X","X", "X", "X","X", "X", "X")),
                table.placement = 'H',
                digits = 3,
                no.space = TRUE,
                label = 'tbl:ccm',
                float = FALSE,
                omit.stat = c("ser", "rsq"),
                df= FALSE)




myfelm2 = felm(ln_npats~ga +ln_at+ppentat+lev+cashat+patenter | ind_yr+fyear | 0 | gvkey, data=innov_mrg)
myfelm6 = felm(tb_innov~ga +ln_at+ppentat+lev+cashat+patenter   | ind_yr+fyear | 0 | gvkey, data=innov_mrg)

B = 500 
out_stat = rep(NA,B)

for(b in 1:B){
  this_boot_obs = sample(1:nrow(innov_mrg), replace=T)
  boot_dat = innov_mrg[this_boot_obs,]
  bmyfelm2 = felm(ln_npats~ga +ln_at+ppentat+lev+cashat+patenter | ind_yr+fyear | 0 | gvkey, data=boot_dat)
  bmyfelm6 = felm(tb_innov~ga +ln_at+ppentat+lev+cashat+patenter   | ind_yr+fyear | 0 | gvkey, data=boot_dat)
  b_stat = coef(bmyfelm6)["ga"]-coef(bmyfelm2)["ga"] 
  out_stat[b] = b_stat
}
diff_stat = coef(myfelm6)["ga"] - coef(myfelm2)["ga"]
se_diff = sd(out_stat)
t_stat = diff_stat/se_diff
p_val =2*pnorm(abs(t_stat), lower.tail=FALSE )

c(diff_stat, se_diff, t_stat, p_val)


## ------------------------------------ ##
## Bonus: Correlations with ROA and Q   ##
## ------------------------------------ ##

namz = c("ln_q_f","roa_f", "ln_npats", "K1mroot4","K2mroot4", "K3mroot4","K4mroot4","K5mroot4", "K6mroot4","K7mroot4","K8mroot4", "K9mroot4", "K10mroot4",
  "K11mroot4", "K12mroot4","K13mroot4", "K14mroot4", "K15mroot4")

namz = c("ln_q_f","roa_f", "npats", "K1m","K2m", "K3m","K4m","K5m", "K6m","K7m","K8m", "K9m", "K10m",
         "K11m", "K12m","K13m", "K14m", "K15m")

dafty = as.data.frame(dft2)
dafty = dafty[dafty$patenter==1,]
daft = dafty[,namz]

daft = daft[complete.cases(daft),]
cor(daft)[1:3,]
cor(daft)[1:3,]^2

cor_tab = cor(daft)[,1:3]
cor_tab = cor_tab[4:18,]
cor_tab = cor_tab[order(cor_tab[,"npats"], decreasing=TRUE),]
xtable(cor_tab[,c("npats", "ln_q_f", "roa_f")],digits=3)



