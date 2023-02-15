#load packages
require(pacman)
p_load(MPTinR, TreeBUGS)

#check if mpt model well constructed -- ok
MPTinR::check.mpt("KW_wsw_model.eqn")

#try to reproduce Klauer & Wegener' 1998 Experiment 1 results (based on response frequencies in Appendix C p.1178)
##1: PosUSposcor
##2: PosUSposincor
##3: PosUSnegincor
##4: PosUSnew
##5: NegUSnegcor
##6: NegUSposincor
##7: NegUSnegincor
##8: NegUSnew
##9: Newposincor
##10: Newnegincor
##11: NewUSnew
##order here: 5, 7, 8, 6, 10, 11, 9, 3, 4, 1, 2 

KW_exp1_few_dist = c(105, 83, 36, 40, 13, 496, 19, 38, 29, 77, 120)
KW_exp1_many_dist = c(83, 67, 52, 38, 20, 1405, 15, 42, 48, 70, 80)

mod_few_dist = fit.mpt(data = KW_exp1_few_dist
        ,model.filename = "KW_wsw_model.eqn"
        ,restrictions.filename = list("Dn=Dpos=Dneg")
        ,ci = 90)
mod_few_dist

mod_many_dist = fit.mpt(data = KW_exp1_many_dist
                       ,model.filename = "KW_wsw_model.eqn"
                       ,restrictions.filename = list("Dn=Dpos=Dneg")
                       ,ci = 90)
mod_many_dist
#we reproduce Klauer & Wegener's 1998 Experiment 1 parameter estimates
