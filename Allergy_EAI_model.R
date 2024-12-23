
library(tidyverse)
library(heemod)
library(diagram)
library(ggplot2)



# import the Canada 2020 life table with mortality value in all ages (both sex)

life_table<-read.csv('./life_table_CAN_2020.csv')

# remove row 111 from life_table (age older than 111 )

life_table <- life_table %>% 
                filter(Age.group != "110 years and over") %>% 
                separate(Age.group, into = c("Age", "Unit"), sep = " ") %>% 
                select(Age, VALUE) %>% 
                mutate(Age=as.numeric(Age), fatality_daily = rescale_prob(p = VALUE, from = 365 ))


# build age parameter,

par_allerg <-define_parameters(
                  age_initial = 1,
                  age = floor(age_initial + model_time/365)
)

# age-dependent remission probability

ar_age<-function(age) {
  if_else(age <=6, rescale_prob(0.058,from = 365), 0) 
}

#_ww: watch and wait scenario
#_ED: transfer to ED scenario 
#ns: non severe
#sw: severe watch and wait,
#sED: severe transfer to ED,
#sh: severe with hospitalized, 
#ar: food allergy remission, 
#faf: food allergy fatality,
#dr: discount rate
#acm: all-cause 

# parameter for X10 mortality in watch and wait 

#parameter for transition
par_allerg<-modify(
par_allerg,
p_severe    = 0.087,                                                     # transition from non-severe reaction to severe reaction 
p_ns_ar_original = 0.058,
p_ns_ar     = if_else(age <=6, rescale_prob(p_ns_ar_original,from = 365), 0), # transition from non-severe reaction to food allergy remission
p_biphas    = 0.046,                                                     # have biphasitic reaction
p_EAI       = 0.906,                                                     # patient with known food allergy having an EAI
p_ns_sw_ww  = (p_EAI-p_biphas) * rescale_prob(p =p_severe, from = 365) , # non-severe reaction to severe reaction for watch and wait
p_ns_sED_ww = (1-p_EAI+p_biphas) * rescale_prob(p = p_severe, from = 365),# non-severe reaction to severe reaction transfer to ED for watch and wait
p_faf = 0.00000069,                                                      # food allergy fatality among patients with food allergy
p_sh_faf    = 0.0045,                                                # transition from hospitalization to food allergy fatality
p_sh_ED      = rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365)/p_sh_faf,  # transition from severe state ED to hospitalization 
p_ns_sED_ED = rescale_prob(p =p_severe, from = 365),                 # transition from non-severe to ED in ED scenario
acm         = look_up(data = life_table, Age = age,                  #daily all-cause mortality
                      value = "fatality_daily"),
)


# parameter for cost and utility
par_allerg<-modify(
par_allerg,
p_EAI_ED                   = 0.54,                                 # Probability of using an EAI prior to ED visit 
treatment_cost_ED          = 0.8 +95 *p_EAI_ED,                    # epi treatment cost in ED 
treatment_cost_ww          = 95,                                   # EAI treatment cost in watch and wait 
treatment_cost_ww_ED       = 0.8,                                  # epi cost for ED transfer in watch and wait scenario 
remission_cost_all         = 569/365,                              # remission patient daily cost for all scenario
medical_cost_ns_original   = 1393,                                 # yearly direct medical cost of food allergy < 18 yrs 
medical_cost_original      = 304,                                  # Average cost of all-cause ED visit
medical_cost_ED_ED         = round(medical_cost_original *1.09,digits = 0), # daily medical cost in ED 
no_visit                   = 0.3,                                  # no.of ED visit per year
medical_cost_ns            = round(((medical_cost_ns_original*1.068)-(medical_cost_ED_ED *no_visit )), digits = 0)/365,# daily medical cost for Canadian with food allergy
ambulance_cost_ED_ED       = 848,                                  # ambulance cost 
medical_h                  = 3670,                                 # total medical cost hospitalization for allergy
length_stay                = 2.1,                                  # length of stay per admission 
medical_cost_hospital      = round(1.068*medical_h/length_stay,digits = 0),         # medical cost hospitalized
indirect_cost_allergy_original = 4173,                              # indirect cost for food allergy < 18 years
out_of_pocket_allergy_original = 2440,                             # out of pocket cost of food allergy per year 
indirect_cost_hospital      = 251,                                 # indirect cost in hospitalization - societal 
out_of_pocket_original      = 89,                                  # out of pocket cost 
out_of_pocket_emergency     = round(out_of_pocket_original * 1.068, digits = 0),    # out of pocket in ED-societal 
waiting_time_ED              = 3.6,                                #  waiting time in ED/ hours 
indirect_cost_emergency     = round(waiting_time_ED*31.37,digits = 0),              # indirect cost in ED -societal 
indirect_cost_allergy      = round(1.068*(indirect_cost_allergy_original-no_visit* indirect_cost_emergency), digits = 0) /365,   # indirect cost for food allergy-societal
out_of_pocket_cost_allergy = round(1.068* (out_of_pocket_allergy_original-no_visit* indirect_cost_emergency), digits = 0) /365,   # out of pocket cost for food allergy-societal 
utilify_far_original       = 0.93,
utility_far                = utilify_far_original /365,                             # utility -food allergy remission 
utility_fa_original        = 0.92,
utility_sa                 = 0.91,
utility_fa                 = utility_fa_original/365,                             # utility- food allergy non-severe
utility_sr                 = utility_fa -(1-utility_sa)/365        # utility-severe reaction 

)


# transition matrix for ED transfer 

Transition_ED <- define_transition(
  state_names = c("state_ar","state_ns", "state_sw", "state_sED", "state_sh", "state_faf","state_acm"),
  C,            0,      0,          0,           0,         0,           acm,
  p_ns_ar,      C,      0,          p_ns_sED_ED, 0,         0,           acm,
  0,            0,      0,          0,           0,         1,           0,
  0,            C,      0,          0,           p_sh_ED,   0,           acm,
  0,            C,      0,          0,           0,         p_sh_faf,    acm,
  0,            0,      0,          0,           0,         1,           0,
  0,            0,      0,          0,           0,         0,           1
)



#transition matrix for watch and wait -patient will not get hospitalized after watch and wait state
Transition_watch <- define_transition(
  state_names = c("state_ar","state_ns", "state_sw", "state_sED", "state_sh", "state_faf","state_acm"),
  C,            0,      0,          0,           0,           0,           acm,
  p_ns_ar,      C,      p_ns_sw_ww, p_ns_sED_ww, 0,           0,           acm,
  0,            C,      0,          0,           0,           p_ww_faf,    acm,
  0,            C,      0,          0,           p_sh_ED,     0,           acm,
  0,            C,      0,          0,           0,           p_sh_faf,    acm,
  0,            0,      0,          0,           0,           1,           0,
  0,            0,      0,          0,           0,           0,           1
)  

## Define cost and utility for each state

# food allergy remission 

state_ar<-define_state(
  remission_cost            = remission_cost_all ,
  medical_cost              = 0,
  treatment_cost            = 0,
  ambulance_cost            = 0,
  medical_cost_ED           = 0,
  medical_cost_hospitalized = 0,
  utility                   = utility_far,
  cost_total                = discount(remission_cost, r=dr),
  utility_total             = discount(utility, r=dr)
)

# non-severe reaction

state_ns<-define_state(
  remission_cost            = 0,
  medical_cost              = medical_cost_ns,
  treatment_cost            = 0,
  ambulance_cost            = 0,
  medical_cost_ED           = 0,
  medical_cost_hospitalized = 0,
  utility                   = utility_fa,
  cost_total                = discount(medical_cost, r=dr),
  utility_total             = discount(utility, r=dr)
)

#severe allergic reaction-watch and waiting 

state_sw<-define_state(
  remission_cost            = 0,
  medical_cost              = medical_cost_ns,
  treatment_cost            = treatment_cost_ww,
  ambulance_cost            = 0,
  medical_cost_ED           = 0,
  medical_cost_hospitalized = 0,
  utility                   = utility_sr,
  cost_total                = discount(medical_cost+treatment_cost, r=dr),
  utility_total             = discount(utility, r=dr)
)

#severe allergic reaction - ED transfer 

state_sED<- define_state(
  remission_cost            = 0,
  medical_cost              = medical_cost_ns, 
  treatment_cost            = dispatch_strategy(
                              ED_transfer = treatment_cost_ED,
                              watch_wait  = treatment_cost_ww_ED),
  ambulance_cost            = ambulance_cost_ED_ED,
  medical_cost_ED           = medical_cost_ED_ED,
  medical_cost_hospitalized = 0,
  utility                   = utility_sr,
  cost_total                = discount(medical_cost + treatment_cost + ambulance_cost + medical_cost_ED , r=dr),
  utility_total             = discount(utility, r=dr)
)

#severe allergic reaction -hospitalized 

state_sh<-define_state(
  remission_cost            = 0,
  medical_cost              = 0,
  treatment_cost            = 0,
  ambulance_cost            = 0,
  medical_cost_ED           = 0,
  medical_cost_hospitalized = medical_cost_hospital,
  utility                   = utility_sr,
  cost_total                = discount(medical_cost_hospitalized,r=dr),
  utility_total             = discount(utility, r=dr)
)


#food allergy fatality
state_faf<-define_state(
  remission_cost            = 0,
  medical_cost              = 0, 
  medical_cost_hospitalized = 0,
  treatment_cost            = 0,
  ambulance_cost            = 0,
  medical_cost_ED           = 0,
  utility                   = 0,
  cost_total                = 0,
  utility_total             = 0
)

#all-cause mortality
state_acm<-define_state(
  remission_cost            = 0,
  medical_cost              = 0, 
  medical_cost_hospitalized = 0,
  treatment_cost            = 0,
  ambulance_cost            = 0,
  medical_cost_ED           = 0,
  utility                   = 0,
  cost_total                = 0,
  utility_total             = 0
)

strategy_ED<-define_strategy(
  transition  = Transition_ED,
  state_ar    = state_ar,
  state_ns    = state_ns,
  state_sw    = state_sw,
  state_sED   = state_sED,
  state_sh    = state_sh,
  state_faf   = state_faf,
  state_acm   = state_acm
)

strategy_watch<- define_strategy(
  transition  = Transition_watch,
  state_ar    = state_ar,
  state_ns    = state_ns,
  state_sw    = state_sw,
  state_sED   = state_sED,
  state_sh    = state_sh,
  state_faf   = state_faf,
  state_acm   = state_acm
)


time0 <- define_init(state_ar  = 0,
                     state_ns  = 10000,
                     state_sw  = 0,
                     state_sED = 0,
                     state_sh  = 0,
                     state_faf = 0,
                     state_acm = 0)

# Model for X10 mortality in watch and wait 
par_allerg_10<- modify(
  par_allerg,
  p_ww_faf    = (10* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas),                                  #transition from watch and wait to food allergy fatality 
  dr          = rescale_prob( p=0.015, from = 365)  
)
allergy_mod_10<-run_model(
  parameters  = par_allerg_10,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch,
  init        = time0,
  cycles      = 7300,
  cost        = cost_total,
  effect      = utility_total,
  method = "beginning"
)




## Model X 100 mortality

#parameter for #parameter for X100 mortality in watch and wait 
par_allerg_100<-modify(
  par_allerg,
  p_ww_faf = (100* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas),# Watch and wait to food allergy fatality X100, baseline 0.000002
  dr          = rescale_prob( p=0.015, from = 365)  
)

allergy_mod_100<-run_model(
  parameters  = par_allerg_100,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch,
  init        = time0,
  cycles      = 20*365,
  cost        = cost_total,
  effect      = utility_total
)
summary(allergy_mod_100)

## Model X 500 mortality

#parameter for #parameter for X500 mortality in watch and wait 
par_allerg_500<-modify(
  par_allerg,
  p_ww_faf =  (500* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas), # Watch and wait to food allergy fatality X500, baseline 0.000002
  dr          = rescale_prob( p=0.015, from = 365)  
)

allergy_mod_500<-run_model(
  parameters  = par_allerg_500,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch,
  init        = time0,
  cycles      = 20*365,
  cost        = cost_total,
  effect      = utility_total
)


## Model X 1000 mortality
# parameter for X 1000 mortality in watch and wait
par_allerg_1000<-modify(
  par_allerg,
  p_ww_faf = (1000* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas), # Watch and wait to food allergy fatality X500, baseline 0.000002
  dr          = rescale_prob( p=0.015, from = 365)  
)

allergy_mod_1000<-run_model(
  parameters  = par_allerg_1000,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch,
  init        = time0,
  cycles      = 20*365,
  cost        = cost_total,
  effect      = utility_total
)


##societal perspective -define state, 
#add "indirect_cost for food allergy", "out of pocket cost for food allergy", 
# "indirect cost-hospitalization", "out of pocket cost in ED", and indirect cost in ED 

# food allergy remission 

state_ar_societal<-define_state(
  remission_cost                = remission_cost_all ,
  medical_cost                  = 0,
  treatment_cost                = 0,
  ambulance_cost                = 0,
  medical_cost_ED               = 0,
  medical_cost_hospitalized     = 0,
  indirect_cost_fa              = 0, 
  out_of_pocket_cost_fa         = 0, 
  indirect_cost_hospitalization = 0,
  out_of_pocket_ED              = 0,
  indirect_cost_ED              = 0, 
  utility                       = utility_far,
  cost_total                    = discount(remission_cost, r=dr),
  utility_total                 = discount(utility, r=dr)
)

# non-severe reaction
state_ns_societal<-define_state(
  remission_cost                = 0,
  medical_cost                  = medical_cost_ns,
  treatment_cost                = 0,
  ambulance_cost                = 0,
  medical_cost_ED               = 0,
  medical_cost_hospitalized     = 0,
  indirect_cost_fa              = indirect_cost_allergy, 
  out_of_pocket_cost_fa         = out_of_pocket_cost_allergy, 
  indirect_cost_hospitalization = 0,
  out_of_pocket_ED              = 0, 
  indirect_cost_ED              = 0,
  utility                       = utility_fa,
  cost_total                    = discount(medical_cost+ indirect_cost_fa + out_of_pocket_cost_fa , r=dr),
  utility_total                 = discount(utility, r=dr)
)

#severe allergic reaction-watch and waiting 
state_sw_societal<-define_state(
  remission_cost                = 0,
  medical_cost                  = medical_cost_ns,
  treatment_cost                = treatment_cost_ww,
  ambulance_cost                = 0,
  medical_cost_ED               = 0,
  medical_cost_hospitalized     = 0,
  indirect_cost_fa              = indirect_cost_allergy, 
  out_of_pocket_cost_fa         = out_of_pocket_cost_allergy, 
  indirect_cost_hospitalization = 0,
  out_of_pocket_ED              = 0, 
  indirect_cost_ED              = 0,  
  utility                       = utility_sr,
  cost_total                    = discount(medical_cost+treatment_cost +indirect_cost_fa + out_of_pocket_cost_fa, r=dr),
  utility_total                 = discount(utility, r=dr)
)

#severe allergic reaction - ED transfer 
state_sED_societal<- define_state(
  remission_cost                = 0,
  medical_cost                  = medical_cost_ns, 
  treatment_cost                = dispatch_strategy(
                                    ED_transfer = treatment_cost_ED,
                                    watch_wait = treatment_cost_ww_ED),
  ambulance_cost                = ambulance_cost_ED_ED,
  medical_cost_ED               = medical_cost_ED_ED,
  medical_cost_hospitalized     = 0,
  indirect_cost_fa              = 0, 
  out_of_pocket_cost_fa         = 0, 
  indirect_cost_hospitalization = 0,
  out_of_pocket_ED              = out_of_pocket_emergency, 
  indirect_cost_ED              = indirect_cost_emergency, 
  utility =utility_sr,
  cost_total = discount(medical_cost + treatment_cost + ambulance_cost + medical_cost_ED + out_of_pocket_ED + indirect_cost_ED, r=dr),
  utility_total = discount(utility, r=dr)
)

#severe allergic reaction -hospitalized 
state_sh_societal<-define_state(
  remission_cost                = 0,
  medical_cost                  = 0,
  treatment_cost                = 0,
  ambulance_cost                = 0,
  medical_cost_ED               = 0,
  medical_cost_hospitalized     = medical_cost_hospital,
  indirect_cost_fa              = 0, 
  out_of_pocket_cost_fa         = 0, 
  indirect_cost_hospitalization = indirect_cost_hospital,
  out_of_pocket_ED              = 0, 
  indirect_cost_ED              = 0, 
  utility                       = utility_sr,
  cost_total                    = discount(medical_cost_hospitalized + indirect_cost_hospitalization ,r=dr),
  utility_total                 = discount(utility, r=dr)
)


#food allergy fatality
state_faf_societal<-define_state(
  remission_cost                = 0,
  medical_cost                  = 0, 
  medical_cost_hospitalized     = 0,
  treatment_cost                = 0,
  ambulance_cost                = 0,
  medical_cost_ED               = 0,
  indirect_cost_fa              = 0, 
  out_of_pocket_cost_fa         = 0, 
  indirect_cost_hospitalization = 0,
  out_of_pocket_ED              = 0, 
  indirect_cost_ED              = 0, 
  utility                       = 0,
  cost_total                    = 0,
  utility_total                 = 0
)

#all-cause mortality
state_acm_societal<-define_state(
  remission_cost                = 0,
  medical_cost                  = 0, 
  medical_cost_hospitalized     = 0,
  treatment_cost                = 0,
  ambulance_cost                = 0,
  medical_cost_ED               = 0,
  indirect_cost_fa              = 0, 
  out_of_pocket_cost_fa         = 0, 
  indirect_cost_hospitalization = 0,
  out_of_pocket_ED              = 0, 
  indirect_cost_ED              = 0, 
  utility                       = 0,
  cost_total                    = 0,
  utility_total                 = 0
)

strategy_ED_societal<-define_strategy(
  transition = Transition_ED,
  state_ar   = state_ar_societal,
  state_ns   = state_ns_societal,
  state_sw   = state_sw_societal,
  state_sED  = state_sED_societal,
  state_sh   = state_sh_societal,
  state_faf  = state_faf_societal,
  state_acm  = state_acm_societal
)

strategy_watch_societal<- define_strategy(
  transition = Transition_watch,
  state_ar   = state_ar_societal,
  state_ns   = state_ns_societal,
  state_sw   = state_sw_societal,
  state_sED  = state_sED_societal,
  state_sh   = state_sh_societal,
  state_faf  = state_faf_societal,
  state_acm  = state_acm_societal
)




#run model for X10 mortality in watch and wait in societal perspective 
allergy_mod_10_societal<-run_model(
  parameters = par_allerg_10,
  ED_transfer = strategy_ED_societal,
  watch_wait = strategy_watch_societal,
  init = time0,
  cycles = 20*365,
  cost = cost_total,
  effect = utility_total
)


# run base model for discount = 0% 
par_allerg_dr_0<- modify(
  par_allerg,
  p_ww_faf    = (10* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas),                                  #transition from watch and wait to food allergy fatality 
  dr          = 0  
)

allergy_mod_dr_0<-run_model(
  parameters = par_allerg_dr_0,
  ED_transfer = strategy_ED,
  watch_wait = strategy_watch,
  init = time0,
  cycles = 20*365,
  cost = cost_total,
  effect = utility_total
)



par_allerg_dr_3<- modify(
  par_allerg,
  p_ww_faf    = (10* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas),                                  #transition from watch and wait to food allergy fatality 
  dr          = rescale_prob( p=0.03, from = 365)  
)

allergy_mod_dr_3<-run_model(
  parameters = par_allerg_dr_3,
  ED_transfer = strategy_ED,
  watch_wait = strategy_watch,
  init = time0,
  cycles = 20*365,
  cost = cost_total,
  effect = utility_total
)



## Build output table

value_10<-get_values(allergy_mod_10)


counts_10<-get_counts(allergy_mod_10)




#develop the table to compare the effect of different mortality in watch & wait

#function to build table
valuetable<-function(value,counts, senario_name){ a <-value %>%
  group_by(.strategy_names, value_names) %>% 
  summarize(.groups = "keep", sum=sum(value/10000)) %>% 
  pivot_wider(names_from = value_names, values_from = sum) %>% 
  ungroup() %>%
  mutate(cost_diff = cost_total- lag(cost_total)) %>%
  mutate(Qaly_diff = utility_total- lag(utility_total)) %>%
  mutate(cost_per_QALY_saved= cost_diff/Qaly_diff) %>%
  mutate(scenario =senario_name)

b <-counts %>% filter(state_names == "state_faf" & model_time == 7300) %>%
  mutate(death_diff = (count - lag(count))/10000)

merge(a,b, by = ".strategy_names" ) %>%
  mutate(cost_per_life_saved = abs(cost_diff/death_diff)) %>%
  select(scenario, .strategy_names, cost_total,utility_total,cost_diff,Qaly_diff,cost_per_QALY_saved, cost_per_life_saved,death_diff ) 
}



value_table_10<-valuetable(value_10,counts_10, "X10_mortality_in_ww")


#INMB calculation

INMB<-value_table_10 %>% 
  mutate(INMB_result = Qaly_diff* 50000-cost_diff )

print(INMB)

#Build the final table to compare the cost_per_lifesaved with different mortality
final_Table<-value_table_10 %>% mutate_if(is.numeric, as.character) 
final_Table <-replace(final_Table, is.na(final_Table),"reference") 

final_Table<-final_Table %>% rename(
  "strategy_name"= ".strategy_names",
  "QALYs_total" = "utility_total",
  "increment_cost" = "cost_diff",
  "increment_Qaly" = "Qaly_diff",
  "increment_cost_per_life_saved" = "cost_per_life_saved"
  
)




#final summary table for different fatality scenario

print(as.data.frame(final_Table))



## DSA Analysis 

# sensitivity analysis parameter 
allergy_sa<-define_dsa(
  p_ns_ar,                ar_age(age = age)*0.8, ar_age(age = age) *1.2,
  p_severe,               0.087 *0.8,            0.087 *1.2 ,
  p_sh_faf,               0.0045*0.8,            0.0045*1.2,
  p_sh_ED,                0.001684833*0.8,       0.001684833*1.2,
  treatment_cost_ED,      0.8,                   95,
  treatment_cost_ww,      95*0.8,                95*1.2,
  treatment_cost_ww_ED,   0.8,                   95,
  remission_cost_all,     569/365*0.8,           569/365*1.2,
  medical_cost_ns,        1388/365 *0.8,         1388/365*1.2,
  ambulance_cost_ED_ED,   848*0.8,               848*1.2,
  medical_cost_ED_ED,     331*0.8,               331*1.2,
  utility_far,            0.93/365*0.8,          1/365, #utility max =1 
  utility_fa,             0.92/365*0.8,          1/365 ,
  utility_sr,             0.812/365,             0.848/365,
  medical_cost_hospital,  1866*0.8,             1866*1.2,
  p_biphas,               0.046 *0.8,           0.046*1.2
  
)



# run sensitivity analysis 
allergy_dsa<-run_dsa(
  model = allergy_mod_10,
  dsa = allergy_sa
)

dsa_icer<-allergy_dsa$dsa %>% select(
  .strategy_names, .par_names, .par_value, .cost, .effect,.n_indiv ) %>% group_by(.par_names) %>%
  reframe( cost_diff =(.cost[.strategy_names == "watch_wait"] - .cost[.strategy_names == "ED_transfer"])/.n_indiv,
             effect_diff = (.effect[.strategy_names == "watch_wait"] -.effect[.strategy_names == "ED_transfer"])/.n_indiv) %>% 
  mutate(cost_per_Qaly_gained = cost_diff/effect_diff)


cost_per_year_QALY_saved_ref<-value_table_10$cost_per_QALY_saved[value_table_10$.strategy_names == "watch_wait"]

#Build tornado plot for SA result
tornado_plot <- function(df, refer_value){
  df <-as.data.frame(df)
  df <- df %>% select(
    .strategy_names, .par_names, .par_value, .cost, .effect, .n_indiv )%>% 
    mutate(
      Boundary = rep(c("Lower_Bound","Upper_Bound"), length.out = nrow(df))) 
  df<-df %>%  group_by(.par_names, Boundary) %>%
    reframe( cost_diff =.cost[.strategy_names == "watch_wait"] - .cost[.strategy_names == "ED_transfer"],
               effect_diff = .effect[.strategy_names == "watch_wait"] -.effect[.strategy_names == "ED_transfer"]) %>%
    mutate(cost_per_Qaly_gained = cost_diff/effect_diff) 
  df<-df%>% select(.par_names, Boundary,cost_per_Qaly_gained) %>%
    pivot_wider(names_from = Boundary,values_from = cost_per_Qaly_gained) 

  df$UL_Difference <- abs(df$Upper_Bound - df$Lower_Bound)
  base.value <- refer_value
  
  dsa_names<-c("Ambulance cost (+/-20%)", "Medical cost - ED (+/-20%)","Medical cost - Hospitalization (+/-20%)","Medical cost - Food allergy (+/-20%)", "Probability -Biphasic reaction (+/-20%)",
    "Annual probability - Food allergy remission (+/-20%)","Annual probability - Severe allergy reaction (+/-20%)", "Probability - Hospitalization post-ED visit (+/-20%)",
    "Food allergy fatality in hospitalized patients (+/-20%)","Medical cost - Food allergy remission (+/-20%)",
    "Epinephrine cost in ED state - Immediate ED transfer strategy ($0.8, $95)", "Epinephrine auto injector cost in watchful waiting state (+/-20%)", 
    "Epinephrine cost in ED state - Watchful waiting strategy ($0.8, $95)", 
               "Utility - Food allergy (+/-20%)", "Utility - Food allergy remission (+/-20%)", "Disutility - Severe allergy reaction (+/-20%)"
  )
  
 
  df$parameter <- dsa_names
  # get order of parameters according to size of intervals
  order.parameters <- df %>% arrange(UL_Difference) %>%
    mutate(parameter=factor(x=parameter, levels=parameter)) %>%
    select(parameter) %>% unlist() %>% levels()
  
  # width of columns in plot (value between 0 and 1)
  width <- 0.8
  
  # get data frame in shape for ggplot and geom_rect
  df.2 <- df %>% 
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
    # just reordering columns
    select(parameter, type, output.value, UL_Difference) %>%
    # create the columns for geom_rect
    mutate(parameter=factor(parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(parameter)-width/2,
           xmax=as.numeric(parameter)+width/2)
  
  
  p <- ggplot() + 
    geom_rect(data = df.2, 
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
    theme_bw() +
    theme(axis.title.y=element_blank(), legend.position = 'bottom',
          legend.title = element_blank(),text=element_text(size=11)) + 
    geom_hline(yintercept = base.value) +
    scale_x_continuous(breaks = c(1:length(order.parameters)), 
                       labels = order.parameters) +
    coord_flip() + labs(y="Incremental cost per QALY saved ($/QALY)") + 
    labs(caption="Figure 2.") + 
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
  
  
  list(df,p)
}

#tornado_plot for the model
DSA_tornado<-tornado_plot(allergy_dsa$dsa, cost_per_year_QALY_saved_ref)

print(DSA_tornado)

ggsave("dsa_tornado.png")

#PSA analysis 
psa_base<-define_psa(
  p_severe ~ beta(66.3345,696.1314),
  p_ns_ar_original ~ beta(47.4951,771.3863),
  p_biphas ~ beta(183.5019,3805.6703),
  p_EAI ~ beta(8.4940, 0.8813),
  p_faf ~ beta(99.9999,144927335.2320),
  p_sh_faf ~ beta(45.8194, 10136.2621),
  p_EAI_ED ~ beta(7.6118, 6.4841),
  medical_cost_ns_original ~ gamma(1393,139.3),
  medical_cost_original~ gamma(304, 30.4),
  ambulance_cost_ED_ED ~ gamma(848,84.8),
  waiting_time_ED ~ gamma(3.6,6.8664),
  medical_h ~ gamma(3670,367),
  length_stay ~ lognormal(mean = 2.1,sd= 2.7,meanlog = 0.742,sdlog = 0.993),
  out_of_pocket_original~ gamma(89, 8.9),
  no_visit ~gamma(0.3,0.9),
  indirect_cost_allergy_original ~gamma(4173, 417.3),
  out_of_pocket_allergy_original ~ gamma(2440, 244),
  utilify_far_original ~ beta(8.7708,0.6602),
  utility_fa_original ~ beta(7.0800, 0.6157),
  utility_sa ~ beta(1.6689, 0.1651)
)
psa_result_base_sample<-run_psa(allergy_mod_10,psa_base, N= 10)

psa_result_base<-run_psa(allergy_mod_10,psa_base, N= 1000)

save(psa_result_base, file = "PSA_result_base")

psa_result_societal<-run_psa(allergy_mod_10_societal,psa_base, N =1000)

save(psa_result_societal, file = "PSA_result_societal")

psa_result_100<-run_psa(allergy_mod_100 ,psa_base, N= 1000)

save(psa_result_100, file = "PSA_result_100")

psa_result_500<-run_psa(allergy_mod_500 ,psa_base, N= 1000)

save(psa_result_500, file = "PSA_result_500")

psa_result_1000<-run_psa(allergy_mod_1000 ,psa_base, N= 1000)

save(psa_result_1000, file = "PSA_result_1000")



psa_result_dr_0<-run_psa(allergy_mod_dr_0 ,psa_base, N= 1000)

save(psa_result_dr_0 , file = "PSA_result_dr_0")

psa_result_dr_3<-run_psa(allergy_mod_dr_3 ,psa_base, N= 1000)

save(psa_result_dr_3 , file = "PSA_result_dr_3")


#PSA summary 

Psa_summary<-function(dr,scenario_name) {
  a <-dr$psa
  
  b <-a%>% group_by(.index) %>% 
    mutate(cost_diff = .cost- lag(.cost)) %>%
    mutate(Qaly_diff = .effect- lag(.effect))%>%
    mutate(INMB =(Qaly_diff/10000) *50000-cost_diff/10000 )
  
  mean_cost<-round(mean(b$cost_diff,na.rm = TRUE)/10000,4)
  
  mean_utility<-signif(mean(b$Qaly_diff,na.rm = TRUE)/10000,4)
  
  INMB_result<-round(mean(b$INMB,na.rm = TRUE),4)
  
  cost_ED<-round(mean(a$.cost[a$.strategy_names=="ED_transfer"])/10000,4)
  
  cost_watch<-round(mean(a$.cost[a$.strategy_names=="watch_wait"])/10000,4)
  
  utility_ED<-round(mean(a$.effect[a$.strategy_names=="ED_transfer"])/10000,4)
  
 utility_watch<-round(mean(a$.effect[a$.strategy_names=="watch_wait"])/10000,4)
  
  c<-data.frame(
    Scenrio = scenario_name,
    watch_cost = cost_watch,
    ED_cost = cost_ED,
    QALY_ED = utility_ED,
    QAlY_watch = utility_watch,
    Increment_cost = mean_cost,
    Increment_utility= mean_utility,
    ICER= mean_cost/mean_utility,
    INMB = INMB_result)
    
    print(c)

}

PSA_result_summary<-bind_rows(Psa_summary(psa_result_base,"psa_base"), Psa_summary(psa_result_100,"psa_100"), Psa_summary(psa_result_500, "psa_500"),
    Psa_summary(psa_result_1000,"psa_1000"), Psa_summary(psa_result_dr_0, "discount 0"), Psa_summary(psa_result_dr_3, "discount 3%"),
    Psa_summary(psa_result_societal,"psa_societal"))
                                                                                                

#ceac plot 
plot_ceac<-plot(psa_result_base , type = "ac", max_wtp = 150000, n=1000) + xlim (-1, 150001)+
  labs(y="Proportion", x="Willingness-to-pay")+
  theme_bw() + theme(panel.border = element_blank(),
                     # panel.grid.major = element_blank(),
                     # panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black") 
  )

#CEA plot 
CEA_plot <- function(data, 
                     x, y, 
                     title=NULL,
                     xlab=NULL, ylab=NULL,wtp){
  p <- ggplot(data = data,
              aes(x,y))+
    geom_point()+
    stat_ellipse()+
    labs(x=xlab, y=ylab, title=title)+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0)+
    geom_abline(intercept = 0, slope = 50000,linetype = 2)+
    annotate("text", x = -0.0006, y = -200, label = "WTP = $50,000/QALY", size = 8, color = "black") + 
    
    theme_bw(base_size = 18) + theme(panel.border = element_blank(),
                       # panel.grid.major = element_blank(),
                       # panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black")
                      
    ) +labs(caption="Figure 3.")+ 
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
  
  return(p)
}


cea_analysis<-psa_result_base$psa %>% 
  arrange(.strategy_names, .index) %>% 
  group_by(.index) %>%
mutate(cost_diff = (.cost-lag(.cost))/10000, utility_diff = (.effect-lag(.effect))/10000)

plot_cea_base<-CEA_plot(data = cea_analysis, x=cea_analysis$utility_diff,y=cea_analysis$cost_diff,xlab="Incremental QALY", ylab = "Incremental Cost" )


#scenario analysis start age = 0 
# build parameters
par_allerg_0 <-define_parameters(
  age_initial = 0,
  age = floor(age_initial + model_time/365)
)


par_allerg_0<-modify(
  par_allerg_0,
  p_severe    = 0.087,                                                     # transition from non-severe reaction to severe reaction 
  p_ns_ar_original = 0.058,
  p_ns_ar     = if_else(age <=6, rescale_prob(p_ns_ar_original,from = 365), 0), # transition from non-severe reaction to food allergy remission
  p_biphas    = 0.046,                                                     # have biphasitic reaction
  p_EAI       = 0.906,                                                     # patient with known food allergy having an EAI
  p_ns_sw_ww  = (p_EAI-p_biphas) * rescale_prob(p =p_severe, from = 365) , # non-severe reaction to severe reaction for watch and wait
  p_ns_sED_ww = (1-p_EAI+p_biphas) * rescale_prob(p = p_severe, from = 365),# non-severe reaction to severe reaction transfer to ED for watch and wait
  p_faf = 0.00000069,                                                      # food allergy fatality among patients with food allergy
  p_sh_faf    = 0.0045,                                                # transition from hospitalization to food allergy fatality
  p_sh_ED      = rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365)/p_sh_faf,  # transition from severe state ED to hospitalization 
  p_ns_sED_ED = rescale_prob(p =p_severe, from = 365),                 # transition from non-severe to ED in ED scenario
  acm         = look_up(data = life_table, Age = age,                  #daily all-cause mortality
                        value = "fatality_daily"),
)

par_allerg_0<-modify(
  par_allerg_0,
  p_EAI_ED                   = 0.54,                                 # Probability of using an EAI prior to ED visit 
  treatment_cost_ED          = 0.8 +95 *p_EAI_ED,                    # epi treatment cost in ED 
  treatment_cost_ww          = 95,                                   # EAI treatment cost in watch and wait 
  treatment_cost_ww_ED       = 0.8,                                  # epi cost for ED transfer in watch and wait scenario 
  remission_cost_all         = 569/365,                              # remission patient daily cost for all scenario
  medical_cost_ns_original   = 1393,                                 # yearly direct medical cost of food allergy < 18 yrs 
  medical_cost_original      = 304,                                  # Average cost of all-cause ED visit
  medical_cost_ED_ED         = round(medical_cost_original *1.09,digits = 0), # daily medical cost in ED 
  no_visit                   = 0.3,                                  # no.of ED visit per year
  medical_cost_ns            = round(((medical_cost_ns_original*1.068)-(medical_cost_ED_ED *no_visit )), digits = 0)/365,# daily medical cost for Canadian with food allergy
  ambulance_cost_ED_ED       = 848,                                  # ambulance cost 
  medical_h                  = 3670,                                 # total medical cost hospitalization for allergy
  length_stay                = 2.1,                                  # length of stay per admission 
  medical_cost_hospital      = round(1.068*medical_h/length_stay,digits = 0),         # medical cost hospitalized
  indirect_cost_allergy_original = 4173,                              # indirect cost for food allergy < 18 years
  out_of_pocket_allergy_original = 2440,                             # out of pocket cost of food allergy per year 
  indirect_cost_hospital      = 251,                                 # indirect cost in hospitalization - societal 
  out_of_pocket_original      = 89,                                  # out of pocket cost 
  out_of_pocket_emergency     = round(out_of_pocket_original * 1.068, digits = 0),    # out of pocket in ED-societal 
  waiting_time_ED              = 3.6,                                #  waiting time in ED/ hours 
  indirect_cost_emergency     = round(waiting_time_ED*31.37,digits = 0),              # indirect cost in ED -societal 
  indirect_cost_allergy      = round(1.068*(indirect_cost_allergy_original-no_visit* indirect_cost_emergency), digits = 0) /365,   # indirect cost for food allergy-societal
  out_of_pocket_cost_allergy = round(1.068* (out_of_pocket_allergy_original-no_visit* indirect_cost_emergency), digits = 0) /365,   # out of pocket cost for food allergy-societal 
  utilify_far_original       = 0.93,
  utility_far                = utilify_far_original/365,                             # utility -food allergy remission 
  utility_fa_original        = 0.92,
  utility_sa                 = 0.91,
  utility_fa                 = utility_fa_original/365,                             # utility- food allergy non-severe
  utility_sr                 = utility_fa -(1-utility_sa)/365        # utility-severe reaction 
  
)

par_allerg_0_10<- modify(
  par_allerg_0,
  p_ww_faf    = (10* rescale_prob(p_faf, from = 365)/rescale_prob(p_severe, from = 365) -(1-p_EAI+p_biphas)* p_sh_ED*p_sh_faf)/(p_EAI-p_biphas),                                  #transition from watch and wait to food allergy fatality 
  dr          = rescale_prob( p=0.015, from = 365)  
)

allergy_mod_0_10<-run_model(
  parameters  = par_allerg_10,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch,
  init        = time0,
  cycles      = 7300,
  cost        = cost_total,
  effect      = utility_total,
  method = "beginning"
)

psa_result_age_0<-run_psa(allergy_mod_0_10,psa_base, N= 1000)

PSA_result_summary<-bind_rows(Psa_summary(psa_result_base,"psa_base"), Psa_summary(psa_result_100,"psa_100"), Psa_summary(psa_result_500, "psa_500"),
                              Psa_summary(psa_result_1000,"psa_1000"), Psa_summary(psa_result_dr_0, "discount 0"), Psa_summary(psa_result_dr_3, "discount 3%"),
                              Psa_summary(psa_result_societal,"psa_societal"), Psa_summary(psa_result_age_0,"psa_age_0"))



