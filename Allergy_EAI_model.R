
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
p_severe    = 0.087,
p_ns_ar     = ar_age(age = age),                                     # transition from non-severe reaction to food allergy remission
p_ns_sw_ww  = (1-0.14) * rescale_prob(p =p_severe, from = 365) ,     # non-severe reaction to severe reaction for watch and wait
p_ns_sED_ww = 0.14 * rescale_prob(p = p_severe, from = 365),         # non-severe reaction to severe reaction transfer to ED for watch and wait
p_sh_ED      = 0.001684833,                                          # transition from severe state ED to hospitalization 
p_sh_ww      = 0.0193168,                                            # transition from severe state watch & wait to hospitalization
p_ns_sED_ED = rescale_prob(p =p_severe, from = 365),                 # transition from non-severe to ED
p_sh_faf    = 0.0045,                                                # transition from hospitalization to food allergy fatality
acm         = look_up(data = life_table, Age = age,                  #daily all-cause mortality
                      value = "fatality_daily"),
p_ww_faf    = 8.692562e-05,                                          #transition from watch and wait to food allergy fatality 
dr          = rescale_prob( p=0.015, from = 365),                    #discount rate
)

# parameter for cost and utility
par_allerg<-modify(
par_allerg,
treatment_cost_ED          = 0.8 +51.3,                            # epi treatment cost in ED 
treatment_cost_ww          = 95,                                   # EAI treatment cost in watch and wait 
treatment_cost_ww_ED       = 0.8,                                  # epi cost for ED transfer in watch and wait scenario 
remission_cost_all         = 513/365,                              # remission patient daily cost for all scenario
medical_cost_ns            = 1254/365,                             # daily medical cost for Canadian with food allergy
ambulance_cost_ED_ED       = 848,                                  # ambulance cost 
medical_cost_ED_ED         = 331,                                  # daily medical cost in ED 
medical_cost_hospital      = 1866,                                 # medical cost hospitalized
indirect_cost_allergy      = 8413/365,                             # indirect cost for food allergy-societal
out_of_pocket_cost_allergy = 2251/365,                             # out of pocket cost for food allergy-societal 
indirect_cost_hospital      = 251,                                 # indirect cost in hospitalization - societal 
out_of_pocket_emergency     = 97,                                  # out of pocket in ED-societal 
indirect_cost_emergency     = 113,                                 # indirect cost in ED -societal 
utility_far                = 0.93/365,                             # utility -food allergy remission 
utility_fa                 = 0.92/365,                             # utility- food allergy non-severe
utility_sr                 = 0.83/365,                             # utility-severe reaction 

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

#transition matrix for watch and wait -patient will get hospitalized after watch and wait state
Transition_watch <- define_transition(
  state_names = c("state_ar","state_ns", "state_sw", "state_sED", "state_sh", "state_faf","state_acm"),
  C,            0,      0,          0,           0,           0,           acm,
  p_ns_ar,      C,      p_ns_sw_ww, p_ns_sED_ww, 0,           0,           acm,
  0,            C,      0,          0,           p_sh_ww,     0,           acm,
  0,            C,      0,          0,           p_sh_ED,     0,           acm,
  0,            C,      0,          0,           0,           p_sh_faf,    acm,
  0,            0,      0,          0,           0,           1,           0,
  0,            0,      0,          0,           0,           0,           1
)  

#transition matrix for watch and wait -patient will not get hospitalized after watch and wait state
Transition_watch_no_hs <- define_transition(
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

#strategy for watch and wait not 
strategy_watch_no_hs<- define_strategy(
  transition  = Transition_watch_no_hs,
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

allergy_mod_10<-run_model(
  parameters  = par_allerg,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch,
  init        = time0,
  cycles      = 7300,
  cost        = cost_total,
  effect      = utility_total,
  method = "beginning"
)

allergy_mod_10_no_hs<-run_model(
  parameters  = par_allerg,
  ED_transfer = strategy_ED,
  watch_wait  = strategy_watch_no_hs,
  init        = time0,
  cycles      = 7300,
  cost        = cost_total,
  effect      = utility_total,
  method = "beginning"
)

summary(allergy_mod_10)
tmp<-get_counts(allergy_mod_10)

summary(allergy_mod_10_no_hs)
hs <-get_counts(allergy_mod_10_no_hs)

hs %>% group_by()

hs %>% filter(state_names == "state_faf") %>% print(n=730)
a<-tmp %>%
  filter(state_names == "state_faf") %>% print(n=730)
print(a)
#check the count in each state 
tmp %>%   
  group_by(.strategy_names, state_names) %>% 
  summarize(avg=mean(count), sum=sum(count))

a <-get_values(allergy_mod_10) %>%  group_by(.strategy_names, value_names) %>% 
  summarize(avg=mean(value), sum=sum(value))

get_values(allergy_mod_10_no_hs) %>%  group_by(.strategy_names, value_names) %>% 
  summarize(avg=mean(value), sum=sum(value))

## Model X 100 mortality

#parameter for #parameter for X100 mortality in watch and wait 
par_allerg_100<-modify(
  par_allerg,
  p_sw_faf = rescale_prob(p = 0.0002, from = 365) # Watch and wait to food allergy fatality X100, baseline 0.000002
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
  p_sw_faf = rescale_prob(p = 0.001, from = 365) # Watch and wait to food allergy fatality X500, baseline 0.000002
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

summary(allergy_mod_500)

## Model X 1000 mortality
# parameter for X 1000 mortality in watch and wait
par_allerg_1000<-modify(
  par_allerg,
  p_sw_faf = rescale_prob(p = 0.002, from = 365) # Watch and wait to food allergy fatality X500, baseline 0.000002
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

summary(allergy_mod_1000)

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

summary(allergy_mod_10_societal)

## Build output table

value_10<-get_values(allergy_mod_10)
value_100<-get_values(allergy_mod_100)
value_500<-get_values(allergy_mod_500)
value_1000<-get_values(allergy_mod_1000)
value_10_societal<-get_values(allergy_mod_10_societal)

#calculate the fataility difference after 7300 rounds
faf_watch_wait_last<-tmp$count[tmp$.strategy_names == "watch_wait" & tmp$model_time ==7300 & tmp$state_names == "state_faf"]
faf_ED_transfer_last<-tmp$count[tmp$.strategy_names == "ED_transfer" & tmp$model_time ==7300 & tmp$state_names == "state_faf"]

death_diff_base<-(faf_watch_wait_last- faf_ED_transfer_last)

#develop the table to compare the effect of different mortality in watch & wait

#function to build table
valuetable<-function(value,senario_name){ value %>%
  group_by(.strategy_names, value_names) %>% 
    summarize(sum=sum(value/10000)) %>% 
    pivot_wider(names_from = value_names, values_from = sum) %>% 
    ungroup() %>%
    mutate(cost_diff = cost_total- lag(cost_total)) %>%
    mutate(Qaly_diff = utility_total- lag(utility_total)) %>%
    mutate(cost_per_life_saved= cost_diff/Qaly_diff) %>%
    mutate(scenario =senario_name) %>%
    select(scenario, .strategy_names, cost_total,utility_total,cost_diff,Qaly_diff,cost_per_life_saved) 
    
}
  


value_table_10<-valuetable(value_10, "X10_mortality_in_ww")
value_table_100<-valuetable(value_100,"X100_mortality_in_ww")
value_table_500<-valuetable(value_500,"X500_mortality_in_ww")
value_table_1000<-valuetable(value_1000, "X1000_mortality_in_ww")
value_table_10_societal<-valuetable(value_10_societal,"X10_mortality_in_ww")



#extract the cost per year life saved in base allergy model
cost_per_year_life_saved_ref<-value_table_10$cost_per_life_saved[value_table_10$.strategy_names == "watch_wait"]

#Build the final table to compare the cost_per_lifesaved with different mortality
final_Table<-bind_rows(value_table_10,value_table_100,value_table_500,value_table_1000)  
final_Table<-final_Table %>% mutate_if(is.numeric, as.character) 
final_Table <-replace(final_Table, is.na(final_Table),"reference") 

final_Table<-final_Table %>% rename(
  "strategy_name"= ".strategy_names",
  "QALYs_total" = "utility_total",
  "increment_cost" = "cost_diff",
  "increment_Qaly" = "Qaly_diff",
  "increment_cost_per_life_saved" = "cost_per_life_saved"
  
)

#extract the cost per year life saved in base allergy model-societal
cost_per_year_life_saved_ref_societal<-value_table_10_societal$cost_per_life_saved[value_table_10_societal$.strategy_names == "watch_wait"]

#Build the final table to compare the cost_per_lifesaved between base and societal model 
final_Table_societal<-bind_rows(value_table_10,value_table_10_societal)  
final_Table_societal<-final_Table_societal %>% mutate_if(is.numeric, as.character) 
final_Table_societal <-replace(final_Table_societal, is.na(final_Table_societal),"reference") 


final_Table_societal<-final_Table_societal %>% rename(
  "strategy_name"= ".strategy_names",
  "QALYs_total" = "utility_total",
  "increment_cost" = "cost_diff",
  "increment_Qaly" = "Qaly_diff",
  "increment_cost_per_life_saved" = "cost_per_life_saved"
  
)

#final summary table for different fatality scenario

print(as.data.frame(final_Table))
print(as.data.frame (final_Table_societal))

## DSA Analysis 

# sensitivity analysis parameter 
allergy_sa<-define_dsa(
  p_ns_ar,                ar_age(age = age)*0.8, ar_age(age = age) *1.2,
  p_sh,                   0.121 *0.8,            0.121 *1.2,
  p_severe,               0.087 *0.8,            0.087 *1.2 ,
  p_sh_faf,               0.0045*0.8,            0.0045*1.2,
  treatment_cost_ED,      0.8,                   95,
  treatment_cost_ww,      95*0.8,                95*1.2,
  treatment_cost_ww_ED,   0.8*0.8,               0.8*1.2,
  remission_cost_all,     513/365*0.8,           513/365*1.2,
  medical_cost_ns,        1254/365 *0.8,         1254/365*1.2,
  ambulance_cost_ED_ED,   848*0.8,               848*1.2,
  medical_cost_ED_ED,     331*0.8,               331*1.2,
  utility_far,            0.93/365*0.8,          1/365, #utility max =1 
  utility_fa,             0.92/365*0.8,          1/365 ,
  utility_sr,             0.83*0.8/365,          0.83*1.2/365,
  medical_cost_hospital,  1866*0.8,             1866*1.2
  
)



# run sensitivity analysis 
allergy_dsa<-run_dsa(
  model = allergy_mod_10,
  dsa = allergy_sa
)


#Build tornado plot for SA result
tornado_plot <- function(df, refer_value){
  df <-as.data.frame(df)
  df <- df %>% select(
    .strategy_names, .par_names, .par_value, .cost, .effect, .n_indiv )%>% 
    mutate(
      Boundary = rep(c("Lower_Bound","Upper_Bound"), length.out = nrow(df))) 
  df<-df %>%  group_by(.par_names, Boundary) %>%
    summarise( cost_diff =.cost[.strategy_names == "ED_transfer"] - .cost[.strategy_names == "watch_wait"],
               effect_diff = .effect[.strategy_names == "ED_transfer"] -.effect[.strategy_names == "watch_wait"]) %>%
    mutate(cost_per_Qaly_gained = cost_diff/effect_diff) %>% ungroup()
  df<-df%>% select(.par_names, Boundary,cost_per_Qaly_gained) %>%
    pivot_wider(names_from = Boundary,values_from = cost_per_Qaly_gained) 

  df$UL_Difference <- abs(df$Upper_Bound - df$Lower_Bound)
  base.value <- refer_value
  
  dsa_names<-c("ambulance cost(+/-20%)", "medical cost in ED(+/-20%)","cost for hospitalization","Daily medical cost in non severe reaction (+/-20%)", 
    "Annual remission probability  (+/-20%)","Probability of transition to severe exacerbation (+/-20%)", "Probability of hospitalization (+/-20%)",
    "Food allergy fatality among hospitalized patient (+/-20%)","Daily cost for food allergy remisison (+/-20%)",
    "Epinephrine cost in ED for ED transfer scenario (0.8, 95)", "Epinephrine cost in watch_wate state(+/-20%)", 
    "Epinephrine cost in ED for watch wait scenario (+/-20%)", 
               "QALY for food allergy(+/-20%)", "QALY for food allergy remission(+/-20%)", "QALY for severe allergy reaction(+/-20%)"
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
          legend.title = element_blank()) + 
    geom_hline(yintercept = base.value) +
    scale_x_continuous(breaks = c(1:length(order.parameters)), 
                       labels = order.parameters) +
    coord_flip() + labs(y="incremental cost per life-year saved")
  
  
  list(df,p)
}

#tornado_plot for the model
tornado_plot(allergy_dsa$dsa, cost_per_year_life_saved_ref)

