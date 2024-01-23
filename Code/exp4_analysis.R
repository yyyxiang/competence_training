library('tidyverse')
library('lmerTest')
library('Rmisc')
library('patchwork')
library('scales')
library('viridis')
source('simulation_helper.R')
source('plot_helper.R')

## Experiment 4
# Note: For ease of analysis, math levels are converted to strengths.

## Part I. fit inverse temperature parameter gamma to individual participants
exp4_dat <- read.csv('./../Data/exp4.csv', header = T, stringsAsFactors = T) %>% 
  filter(round == 0) %>% 
  select(subject, target, team)

## param fitting - grid search
efforts <- seq(0, 1, by = 0.01)
training_box_weights <- c(0,2,4,6,8)
starting_strengths <- c(2, 8, 20, 40)
unit_cost <- 0.02
costs <- starting_strengths*unit_cost # hiring cost
targets <- c(24, 50, 61)
reward <- 2 # reward for successful joint lift
alpha <- 0 # no training cost
search_space <- seq(5, 15, 0.5)

gamma_grid <- NULL
for (gamma in search_space) {
  gamma_planning <- gamma
  gamma_exploitation <- gamma
  gamma_equity <- gamma
  gamma_learning <- gamma
  gamma_equality <- gamma
  
  sim_team_selection <- NULL
  for (target in targets) {
    combinations <- expand.grid(starting_strengths, starting_strengths)
    names(combinations) <- c('s1', 's2')
    combinations <- combinations %>% filter(s1 < s2)
    
    for (i in 1:length(combinations$s1)) {
      initial_s1 <- combinations$s1[i]
      initial_s2 <- combinations$s2[i]
      simulation <- simulate_training_strength(initial_s1, initial_s2, training_box_weights, target) %>% 
        filter(round == max(round))
      # calculate action values
      cost <- (initial_s1 + initial_s2)*unit_cost
      combinations[i, 'planning'] <- ((simulation$s1[simulation$model == 'planning'] + simulation$s2[simulation$model == 'planning']) >= target)*reward - cost
      combinations[i, 'exploitation'] <- max(simulation$s1[simulation$model == 'exploitation'], simulation$s2[simulation$model == 'exploitation'], starting_strengths[! starting_strengths %in% c(initial_s1, initial_s2)]) - cost
      combinations[i, 'equity'] <- -max(simulation$s1[simulation$model == 'equity'], simulation$s2[simulation$model == 'equity'], starting_strengths[! starting_strengths %in% c(initial_s1, initial_s2)]) + min(simulation$s1[simulation$model == 'equity'], simulation$s2[simulation$model == 'equity'], starting_strengths[! starting_strengths %in% c(initial_s1, initial_s2)]) - cost
      combinations[i, 'learning'] <- (simulation$s1[simulation$model == 'learning'] + simulation$s2[simulation$model == 'learning']) - (initial_s1 + initial_s2) - cost
      combinations[i, 'equality'] <- -cost
    }
    
    probs <- combinations %>% 
      mutate(planning = exp(planning*gamma_planning)/sum(exp(planning*gamma_planning)),
             exploitation = exp(exploitation*gamma_exploitation)/sum(exp(exploitation*gamma_exploitation)),
             equity = exp(equity*gamma_equity)/sum(exp(equity*gamma_equity)),
             learning = exp(learning*gamma_learning)/sum(exp(learning*gamma_learning)),
             equality = exp(equality*gamma_equality)/sum(exp(equality*gamma_equality)),) %>% 
      pivot_longer(c('planning', 'exploitation', 'equity', 'learning', 'equality'), names_to = 'model', values_to = 'prob') %>% 
      mutate(target = target)
    
    sim_team_selection <- rbind(sim_team_selection, probs)
  }
  
  sim_team_selection <- sim_team_selection %>% 
    arrange(target, s1, s2) %>% 
    mutate(team = paste0('<', s1, ',', s2, '>'),
           team = as_factor(team)) %>% 
    select(-c(s1, s2))
  
  gamma_grid <- rbind(gamma_grid, sim_team_selection %>% mutate(gamma = gamma))
}
gamma_grid <- gamma_grid %>% 
  pivot_wider(names_from = model, values_from = prob)

planning <- NULL
exploitation <- NULL
equity <- NULL
learning <- NULL
equality <- NULL

for (sub in 1:max(exp4_dat$subject)) {
  df <- NULL
  for (g in search_space) {
    sub_df <- merge(exp4_dat %>% filter(subject == sub), gamma_grid %>% filter(gamma == g))
    df <- rbind(df, sub_df)
  }
  df <- df %>% 
    group_by(gamma) %>% 
    dplyr::summarize(planning = sum(log(planning)),
                     exploitation = sum(log(exploitation)),
                     equity = sum(log(equity)),
                     learning = sum(log(learning)),
                     equality = sum(log(equality)))
  
  planning_subset <- df[which.max(df$planning), c('gamma', 'planning')] %>% mutate(subject = sub)
  exploitation_subset <- df[which.max(df$exploitation), c('gamma', 'exploitation')] %>% mutate(subject = sub)
  equity_subset <- df[which.max(df$equity), c('gamma', 'equity')] %>% mutate(subject = sub)
  learning_subset <- df[which.max(df$learning), c('gamma', 'learning')] %>% mutate(subject = sub)
  equality_subset <- df[which.max(df$equality), c('gamma', 'equality')] %>% mutate(subject = sub)
  
  planning <- rbind(planning, planning_subset)
  exploitation <- rbind(exploitation, exploitation_subset)
  equity <- rbind(equity, equity_subset)
  learning <- rbind(learning, learning_subset)
  equality <- rbind(equality, equality_subset)
}

## Part II. bar plot
sim <- NULL
for (sub in 1:max(exp4_dat$subject)) {
  gamma_planning <- planning$gamma[planning$subject == sub]
  gamma_exploitation <- exploitation$gamma[exploitation$subject == sub]
  gamma_equity <- equity$gamma[equity$subject == sub]
  gamma_learning <- learning$gamma[learning$subject == sub]
  gamma_equality <- equality$gamma[equality$subject == sub]
  
  gamma_grid_longer <- gamma_grid %>% 
    pivot_longer(c('planning', 'exploitation', 'equity', 'learning', 'equality'), 
                 names_to = 'model',
                 values_to = 'prob')
  
  planning_subset <- gamma_grid_longer %>% 
    filter(gamma == gamma_planning & model == 'planning') %>% 
    mutate(subject = sub, gamma = NULL)
  exploitation_subset <- gamma_grid_longer %>% 
    filter(gamma == gamma_exploitation & model == 'exploitation') %>% 
    mutate(subject = sub, gamma = NULL)
  equity_subset <- gamma_grid_longer %>% 
    filter(gamma == gamma_equity & model == 'equity') %>% 
    mutate(subject = sub, gamma = NULL)
  learning_subset <- gamma_grid_longer %>% 
    filter(gamma == gamma_learning & model == 'learning') %>% 
    mutate(subject = sub, gamma = NULL)
  equality_subset <- gamma_grid_longer %>% 
    filter(gamma == gamma_equality & model == 'equality') %>% 
    mutate(subject = sub, gamma = NULL)
  
  sim <- rbind(sim, planning_subset, exploitation_subset, equity_subset, learning_subset, equality_subset)
}
sim <- sim %>% 
  group_by(model, target, team) %>% 
  dplyr::summarize(prob = mean(prob))
sim_wider <- sim %>% 
  pivot_wider(names_from = model, values_from = prob)

exp4_dat <- read.csv('./../Data/exp4.csv', header = T, stringsAsFactors = T) %>% 
  filter(round == 0) %>% 
  select(subject, scenario, target, team)
num_subject <- length(unique(exp4_dat$subject))
counts <- table(exp4_dat$team, exp4_dat$target)
p <- counts/num_subject
m <- data.frame(p) %>% 
  dplyr::rename(team = Var1, target = Var2, prob = Freq) %>% 
  mutate(ci = sqrt(prob*(1-prob)/num_subject)*1.96) %>% 
  mutate(model = 'data')
params <- exp4_dat %>%
  filter(subject == 1) %>%
  select(scenario, target) %>%
  arrange(scenario) %>%
  mutate(title = paste0('[Scenario ', scenario, 
                        ']\nStarting math levels: <2, 8, 20, 40>\nTarget: ', 
                        target))

dat_merge <- merge(m, sim_wider)

sim <- sim %>% 
  mutate(ci = NA)
data_sim <- rbind(m, sim) %>% 
  mutate(scenario = case_when(target == unique(params$target)[1] ~ 1,
                              target == unique(params$target)[2] ~ 2,
                              target == unique(params$target)[3] ~ 3))

p1 <- bar_plot(data_sim, 1)
p2 <- bar_plot(data_sim, 2)
p3 <- bar_plot(data_sim, 3)

pdf('fig8a.pdf', onefile = T, width = 12, height = 7)
p1 | p2 | p3
dev.off()

## Part III. correlation
dat_merge_long <- dat_merge %>% 
  select(-model) %>% 
  pivot_longer(cols = c('planning', 'exploitation', 'equity', 'learning', 'equality'), 
               names_to = 'model',
               values_to = 'predicted_prob')

for (m in unique(dat_merge_long$model)) {
  print(paste0(m, ' model:'))
  print(cor.test(dat_merge_long$prob[dat_merge_long$model == m], dat_merge_long$predicted_prob[dat_merge_long$model == m], method = 'pearson', use = 'complete.obs'))
}

## Part IV. random-effects Bayesian model selection
## calculate BIC and model evidence
loglik <- cbind(planning %>% arrange(subject) %>% select(planning),
                exploitation %>% arrange(subject) %>% select(exploitation),
                equity %>% arrange(subject) %>% select(equity),
                learning %>% arrange(subject) %>% select(learning),
                equality %>% arrange(subject) %>% select(equality))

# BIC = k*ln(n) - 2*ln(L). k: # free parameters, n: # observations (scenarios), L: log likelihood using MLE
bic <- 1*log(4) - 2*loglik

# model evidence = -0.5*BIC
model_evidence <- -0.5*bic
write.table(model_evidence, 'exp4_model_evidence.csv', sep = ",", col.names = F, row.names = F)

p1 <- model_evidence %>%
  pivot_longer(everything(), names_to = 'model', values_to = 'evidence') %>%
  mutate(model = factor(model, levels = c('planning', 'exploitation', 'equity', 'learning', 'equality'),
                        labels = c('Planning', 'Exploitation', 'Equity', 'Learning', 'Equality'),
                        ordered = T)) %>%
  ggplot(aes(model, evidence, fill = model)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(name = NULL, values = c('#e78ac3', '#a6d854', '#8da0cb', '#66c2a5', '#e5c494')) +
  theme_bw() +
  labs(x = NULL, y = 'Model evidence') +
  theme(legend.position = 'none')

pdf('fig3d.pdf', onefile = T, width = 5, height = 3)
p1
dev.off()

## calculate posterior exceedance probabilities and posteriors from model evidence estimates 
# using the bms function in the mfit package: https://github.com/sjgershm/mfit

# pxp: protected exceedance probabilities
pxp <- read.csv('exp4_protected_exceedance_probabilities.csv', header = T)
pxp$planning

## Part V. individual participant 2-D plots for modal teams
exp4_dat <- read.csv('./../Data/exp4.csv', header = T, stringsAsFactors = T) %>% 
  select(-c(expIndex, pset_selected, agent_selected)) %>% 
  dplyr::rename(s1 = weak_math, s2 = strong_math) %>% 
  group_by(scenario) %>% 
  add_count(team) %>% 
  filter(n == max(n)) %>% 
  arrange(scenario, subject, round)
params <- exp4_dat %>%
  group_by(scenario) %>% 
  filter(round == 0 & row_number() == 1) %>% 
  select(scenario, target, s1, s2) %>%
  mutate(title = paste0('[Scenario ', scenario,
                        ']\n Starting math levels: <', s1, ', ', s2,
                        '>\n Target: ', target))

dat_endpoint <- exp4_dat %>% 
  filter(round == 3) %>% 
  group_by(scenario, s1, s2) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(model = 'data')

p1 <- individual(dat_endpoint, 1) + labs(x = 'Math level of agent worse at math', y = 'Math level of agent better at math')
p2 <- individual(dat_endpoint, 2) + labs(x = 'Math level of agent worse at math', y = NULL)
p3 <- individual(dat_endpoint, 3) + labs(x = 'Math level of agent worse at math', y = NULL)

pdf('fig8b.pdf', onefile = T, width = 10, height = 4)
p1 | p2 | p3
dev.off()
