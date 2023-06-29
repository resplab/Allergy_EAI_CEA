library(tidyverse)
library(heemod)
library(diagram)


par_hiv<-define_parameters(
  p_AB_mono = 0.202,
  p_AC_mono = 0.067,
  p_AD_mono= 0.010,
  p_BC_mono= 0.407,
  p_BD_mono = 0.012,
  p_CD_mono =0.25
)

par_hiv<-modify(
  par_hiv,
  p_AB_comb =p_AB_mono *0.509,
  p_AC_comb =p_AC_mono *0.509,
  p_AD_comb=p_AD_mono * 0.509,
  p_BC_comb =p_BC_mono *0.509,
  p_BD_comb = p_BD_mono *0.509,
  p_CD_comb =p_CD_mono*0.509
)

par_hiv <-modify(
  par_hiv,
  cost_zido =2278,
  cost_lami = 2086,
  dr = 0.06
)

par_hiv
 
HIV_mono <- define_transition(
  state_names = c("State_A", "State_B","State_C","Death"),
  C, p_AB_mono,  p_AC_mono, p_AD_mono,
  0,    C,  p_BC_mono, p_BD_mono,
  0,    0,  C,  p_CD_mono,
  0,    0,    0,    1
)

plot(HIV_mono)
HIV_comb<-define_transition(
  state_names = c("State_A", "State_B","State_C","Death" ),
  C, p_AB_comb, p_AC_comb, p_AD_comb ,
  0, C,       p_BC_comb ,  p_BD_comb,
  0, 0,       C,       p_CD_comb,
  0, 0,       0,       1
)


state_A <-define_state(
  cost_medical = 2756,
  cost_meds= dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami),
  cost_total = discount(cost_medical + cost_meds, r=dr),
  qaly =1 
  )

state_B <-define_state(
  cost_medical = 3052,
  cost_meds= dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami),
  cost_total = discount(cost_medical + cost_meds, 0.06),
  qaly =1 
)

state_C <-define_state(
  cost_medical = 9007,
  cost_meds=  dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami),
  cost_total = discount(cost_medical + cost_meds, 0.06),
  qaly =1 
)

state_D <-define_state(
  cost_medical = 0,
  cost_meds= 0,
  cost_total = 0,
  qaly =0
)

strat_mono<- define_strategy(
  transition = HIV_mono,
  State_A = state_A,
  State_B = state_B,
  State_C = state_C,
  Death = state_D
)

strat_comb<- define_strategy(
  transition = HIV_comb,
  State_A = state_A,
  State_B = state_B,
  State_C = state_C,
  Death = state_D
)

HIV_mod<-run_model(
  parameters = par_hiv,
  mono = strat_mono,
  comb = strat_comb,
  cycles = 50,
  cost = cost_total,
  effect = qaly)

summary(HIV_mod,  threshold = c(1000, 5000, 6000, 1e4))

