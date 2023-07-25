library(tidyverse)
library(heemod)
library(diagram)
library(ggplot2)


# import the Canada 2020 life table with mortality value in all ages (both sex)

life_table<-read.csv('./life_table_CAN_2020.csv')

#remove row 111 from life_table (age older than 111 )
life_table <- life_table %>% 
                filter(Age.group != "110 years and over") %>% 
                separate(Age.group, into = c("Age", "Unit"), sep = " ") %>% 
                select(Age, VALUE) %>% 
                mutate(Age=as.numeric(Age), fatality_daily = rescale_prob(p = VALUE, from = 365 ))
 
#build age parameter,
par_allerg <-define_parameters(
                  age_initial = 1,
                  age = floor(age_initial + markov_cycle/365)
)


#_ww: watch and wait scenario
#_ED: transfer to ED scenario 
#ns: non severe
#sw: severe watch and wait,
#sED: severe transfer to ED,
#sh: severe with hospitalized, 
#ar: food allergy remission, 
#faf: food allergy fatality,
#dr: discount rate
#acm: all-cause mortality


par_allerg<-modify(
par_allerg,
p_ns_ar = rescale_prob(p = 0.017, from = 365), # transition from non-severe reaction to food allergy remission
p_ns_sw_ww = (1-0.14) * rescale_prob(p =0.087, from = 365) , # non-severe reaction to severe reaction for watch and wait
p_ns_sED_ww =  0.14 * rescale_prob(p =0.087, from = 365), # non-severe reaction to severe reaction transfer to ED for watch and wait
p_sw_sh = 0.121, #transition from severe reaction for watch and wait to hospitalization 
p_sw_faf = rescale_prob(p = 0.00002, from = 365), # Watch and wait to food allergy fatality
p_sED_sh = 0.121, #transition from ED to hospitalization 
p_sED_faf = rescale_prob(p = 0.000002, from = 365), # transition from ED to food allergy fatality
p_ns_sED_ED= rescale_prob(p =0.087, from = 365), # transition from non-severe to ED
p_sh_faf= 0.0045,# transition from hospitalization to food allergy fatality # old value 0.0045 
acm = look_up(data = life_table, Age = age, value = "fatality_daily"), #daily all-cause mortality
dr=rescale_prob( p=0.015, from = 365)
  )


#transition matrix for watch and wait
Transition_watch <- define_transition(
  state_names = c("state_ar","state_ns", "state_sw", "state_sED", "state_sh", "state_faf","state_acm"),
  C,          0,     0,          0,           0,        0,         acm,
  p_ns_ar,    C,     p_ns_sw_ww, p_ns_sED_ww, 0,        0,         acm,
  0,          C,     0,          0,           p_sw_sh,  p_sw_faf,  acm,
  0,          C,     0,          0,           p_sED_sh, p_sED_faf, acm,
  0,          C,     0,          0,           0,        p_sh_faf,  acm,
  0,          0,     0,          0,           0,        1,           0,
  0,          0,     0,          0,           0,        0,           1
)

Transition_watch
plot(Transition_watch)

#transition matrix for ED transfer 
Transition_ED <- define_transition(
  state_names = c("state_ar","state_ns", "state_sw", "state_sED", "state_sh", "state_faf","state_acm"),
  C,            0,      0,           0,       0,         0,      acm,
  p_ns_ar,      C,      0, p_ns_sED_ED,       0,         0,      acm,
  0,            0,      0,           0,       0,         1,        0,
  0,            C,      0,           0,p_sED_sh, p_sED_faf,      acm,
  0,            C,      0,           0,       0,  p_sh_faf,      acm,
  0,            0,      0,           0,       0,         1,        0,
  0,            0,      0,           0,       0,         0,        1
)

Transition_ED
plot(Transition_ED)


# food allergy remission 
state_ar<-define_state(
  remission_cost= 513/365,
  medical_cost = 0,
  treatment_cost = 0,
  ambulance_cost = 0,
  medical_cost_ED = 0,
  medical_cost_hospitalized =0,
  utility =0.93,
  cost_total = discount(remission_cost, r=dr),
  utility_total = discount(utility, r=dr)
)

# non-severe reaction
state_ns<-define_state(
  remission_cost= 0,
  medical_cost = 1254/365,
  treatment_cost = 0,
  ambulance_cost = 0,
  medical_cost_ED = 0,
  medical_cost_hospitalized =0,
  utility = 0.92,
  cost_total = discount(medical_cost, r=dr),
  utility_total = discount(utility, r=dr)
)

#severe allergic reaction-watch and waiting 
state_sw<-define_state(
  remission_cost= 0,
  medical_cost = 1254/365,
  treatment_cost = 95,
  ambulance_cost = 0,
  medical_cost_ED = 0,
  medical_cost_hospitalized =0,
  utility = 0.83,
  cost_total = discount(medical_cost+treatment_cost, r=dr),
  utility_total = discount(utility, r=dr)
)

#severe allergic reaction - ED transfer 
#medical cost =direct cost of food allergy/year+ED medical cost
state_sED<- define_state(
  remission_cost= 0,
  medical_cost = 1254/365, 
  medical_cost_ED = 331,
  ambulance_cost = 848,
  treatment_cost = 0.8,
  medical_cost_hospitalized =0,
  utility =0.83,
  cost_total = discount(medical_cost + medical_cost_ED + ambulance_cost+ treatment_cost, r=dr),
  utility_total = discount(utility, r=dr)
)

#severe allergic reaction -hospitalized 
state_sh<-define_state(
  remission_cost= 0,
  medical_cost = 0,
  treatment_cost = 0,
  ambulance_cost = 0,
  medical_cost_ED = 0,
  medical_cost_hospitalized =1866,
  utility = 0.83,
  cost_total = discount(medical_cost_hospitalized,r=dr),
  utility_total = discount(utility, r=dr)
)


#food allergy fatality
state_faf<-define_state(
  remission_cost= 0,
  medical_cost =0, 
  medical_cost_hospitalized = 0,
  treatment_cost = 0,
  ambulance_cost = 0,
  medical_cost_ED = 0,
  utility = 0,
  cost_total = 0,
  utility_total = 0
)

#all-cause mortality
state_acm<-define_state(
  remission_cost= 0,
  medical_cost =0, 
  medical_cost_hospitalized = 0,
  treatment_cost = 0,
  ambulance_cost = 0,
  medical_cost_ED = 0,
  utility = 0,
  cost_total = 0,
  utility_total = 0
)


strategy_watch<- define_strategy(
  transition = Transition_watch,
  state_ar = state_ar,
  state_ns = state_ns,
  state_sw = state_sw,
  state_sED = state_sED,
  state_sh = state_sh,
  state_faf = state_faf,
  state_acm = state_acm
)



strategy_ED<-define_strategy(
  transition = Transition_ED,
  state_ar = state_ar,
  state_ns = state_ns,
  state_sw = state_sw,
  state_sED = state_sED,
  state_sh = state_sh,
  state_faf = state_faf,
  state_acm = state_acm
)

time0 <- define_init(state_ar = 0,
                     state_ns = 10000,
                     state_sw = 0,
                     state_sED= 0,
                     state_sh = 0,
                     state_faf= 0,
                     state_acm=0)

allergy_mod<-run_model(
                parameters = par_allerg,
                watch_wait = strategy_watch,
                ED_transfer = strategy_ED,
                init = time0,
                cycles = 20*365,
                cost = cost_total,
                effect = utility
)


plot(allergy_mod)
tmp <- get_counts(allergy_mod)
tmp %>%   
  group_by(.strategy_names, state_names) %>% 
  summarise(avg=mean(count), sum=sum(count))

summary(allergy_mod)

plot(allergy_mod, states = c("state_faf" )) 
plot(allergy_mod, states = c("state_sh")) 
plot(allergy_mod, states = c("state_sED")) 
plot(allergy_mod, states = c("state_sw"))
plot(allergy_mod,states = c("state_ns"))
plot(allergy_mod, states = c("state_ar"))



summary(allergy_mod)

rescale_prob


