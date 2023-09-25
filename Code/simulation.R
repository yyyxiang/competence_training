library('tidyverse')
source('simulation_helper.R')

efforts <- seq(0, 1, by = 0.01)
training_box_weights <- c(0,2,4,6,8)

## Training
gamma <- 10.5 # inverse temperature
initial_s1s <- c(2, 3, 3, 5)
initial_s2s <- c(8, 8, 9, 8)
final_weights <- c(11, 12, 13, 13.5)
reward <- 1 # reward for successful joint lift
alpha <- 0.04 # training cost

sim_training <- NULL
for (s in 1:length(initial_s1s)) {
  simulation <- simulate_training(initial_s1s[s], initial_s2s[s], training_box_weights, final_weights[s]) %>% mutate(scenario = s, final_weight = final_weights[s])
  sim_training <- rbind(sim_training, simulation)
}

# write.csv(sim_training,'sim_training.csv', row.names = FALSE)


## Team selection
gamma <- 4 # inverse temperature
starting_strengths <- c(2, 8, 20, 40)
unit_cost <- 0.02
costs <- starting_strengths*unit_cost # hiring cost
final_weights <- c(24, 50, 61)
reward <- 2 # reward for successful joint lift
alpha <- 0 # no training cost

sim_team_selection <- NULL
for (final_weight in final_weights) {
  combinations <- expand.grid(starting_strengths, starting_strengths)
  names(combinations) <- c('s1', 's2')
  combinations <- combinations %>% filter(s1 < s2)
  
  for (i in 1:length(combinations$s1)) {
    initial_s1 <- combinations$s1[i]
    initial_s2 <- combinations$s2[i]
    simulation <- simulate_training(initial_s1, initial_s2, training_box_weights, final_weight) %>% 
      filter(round == max(round))
    # calculate action values
    cost <- (initial_s1 + initial_s2)*unit_cost
    combinations[i, 'planning'] <- ((simulation$s1[simulation$model == 'planning'] + simulation$s2[simulation$model == 'planning']) >= final_weight)*reward - cost
    combinations[i, 'learning'] <- (simulation$s1[simulation$model == 'learning'] + simulation$s2[simulation$model == 'learning']) - (initial_s1 + initial_s2)
    combinations[i, 'exploitation'] <- max(simulation$s1[simulation$model == 'exploitation'], simulation$s2[simulation$model == 'exploitation'], starting_strengths[! starting_strengths %in% c(initial_s1, initial_s2)])
    combinations[i, 'equity'] <- -max(simulation$s1[simulation$model == 'equity'], simulation$s2[simulation$model == 'equity'], starting_strengths[! starting_strengths %in% c(initial_s1, initial_s2)]) + min(simulation$s1[simulation$model == 'equity'], simulation$s2[simulation$model == 'equity'], starting_strengths[! starting_strengths %in% c(initial_s1, initial_s2)])
  }
  
  probs <- combinations %>% 
    mutate(planning = exp(planning*gamma)/sum(exp(planning*gamma)),
           learning = exp(learning*gamma)/sum(exp(learning*gamma)),
           exploitation = exp(exploitation*gamma)/sum(exp(exploitation*gamma)),
           equity = exp(equity*gamma)/sum(exp(equity*gamma))) %>% 
    pivot_longer(c('planning', 'learning', 'exploitation', 'equity'), names_to = 'model', values_to = 'prob') %>% 
    mutate(final_weight = final_weight)
  
  sim_team_selection <- rbind(sim_team_selection, probs)
}

sim_team_selection <- sim_team_selection %>% 
  arrange(final_weight, s1, s2) %>% 
  mutate(team = paste0('<', s1, ',', s2, '>'),
         team = as_factor(team)) %>% 
  select(-c(s1, s2))

# write.csv(sim_team_selection,'sim_team_selection.csv', row.names = FALSE)

