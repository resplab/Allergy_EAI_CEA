library(tidyverse)
library(heemod)
library(diagram)

states <- c("no_AR", "AR")

mat_trans <- define_transition(
  .9, .1,
  .2, .8,
state_names=states
)

plot(mat_trans)

state_no_AR <- define_state(
  cost = 1234,
  utility = 0.85
)

state_AR <- define_state(
  cost = 4321,
  utility = 0.50
)


strat <- define_strategy(
            transition = mat_trans,
            no_AR = state_no_AR,
            AR = state_AR
)

res_mod <- run_model(
  strat,
  cycles = 10,
  cost = cost,
  effect = utility
)
