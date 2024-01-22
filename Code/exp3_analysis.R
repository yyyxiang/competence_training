library('tidyverse')
library('lmerTest')
library('Rmisc')
library('patchwork')
library('scales')
library('viridis')
source('simulation_helper.R')
source('plot_helper.R')

## Experiment 3
# Note: For ease of analysis, math levels are converted to strengths.

## Part I. fit inverse temperature parameter gamma to individual participants
## convert choices of agents and boxes to strategies
exp3_dat <- read.csv('./../Data/exp3.csv', header = T, stringsAsFactors = T) %>% 
  select(-c(expIndex, weak_math, strong_math, target)) %>% 
  filter(round != 0) %>% 
  mutate(round_combination = paste0(agent_selected, pset_selected)) %>% 
  mutate(round_combination = case_when(
    # If no agent is selected, no box should be selected; recoding those who chose a box but didn't choose agent
    round_combination == '02' | round_combination == '04' | round_combination == '06' | round_combination == '08' ~ '00',
    # If no box is selected, then no agent should be selected; recoding those who chose an agent but didn't choose a box
    round_combination == '10' | round_combination == '20' ~ '00',
    T ~ round_combination)) %>% 
  group_by(subject, scenario) %>% 
  dplyr::summarize(strategy = paste0(round_combination, collapse = ""))

## param fitting - grid search
efforts <- seq(0, 1, by = 0.01)
training_box_weights <- c(0,2,4,6,8)
initial_s1s <- c(2, 3, 3, 5, 2, 3, 3, 5)
initial_s2s <- c(8, 8, 9, 8, 8, 8, 9, 8)
targets <- c(11, 12, 13, 13.5, 12, 12.5, 13.5, 15)
reward <- 1 # reward for successful joint lift
alpha <- 0.04 # training cost
search_space <- seq(5, 15, 0.5)

gamma_grid <- NULL
for (gamma in search_space) {
  sim_training_probs <- NULL
  for (s in 1:length(initial_s1s)) {
    simulation <- simulate_training_probabilities(initial_s1s[s], initial_s2s[s], training_box_weights, targets[s]) %>% 
      mutate(scenario = s, gamma = gamma)
    sim_training_probs <- rbind(sim_training_probs, simulation)
  }
  gamma_grid <- rbind(gamma_grid, sim_training_probs)
}

planning <- NULL
exploitation <- NULL
equity <- NULL
learning <- NULL
equality <- NULL

for (sub in 1:max(exp3_dat$subject)) {
  df <- NULL
  for (g in search_space) {
    sub_df <- merge(exp3_dat %>% filter(subject == sub), gamma_grid %>% filter(gamma == g))
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

## Part II. 2-D plot
sim <- NULL
for (sub in 1:max(exp3_dat$subject)) {
  gamma_planning <- planning$gamma[planning$subject == sub]
  gamma_exploitation <- exploitation$gamma[exploitation$subject == sub]
  gamma_equity <- equity$gamma[equity$subject == sub]
  gamma_learning <- learning$gamma[learning$subject == sub]
  gamma_equality <- equality$gamma[equality$subject == sub]
  
  sim_strength <- NULL
  for (s in 1:length(initial_s1s)) {
    simulation <- simulate_training_strength(initial_s1s[s], initial_s2s[s], training_box_weights, targets[s]) %>% 
      mutate(scenario = s, target = targets[s], subject = sub)
    sim_strength <- rbind(sim_strength, simulation)
  }
  sim <- rbind(sim, sim_strength)
}
sim$round <- as.integer(sim$round)
sim <- sim %>% 
  group_by(round, model, scenario, target) %>% 
  dplyr::summarize(s1 = mean(s1), s2 = mean(s2))

exp3_dat <- read.csv('./../Data/exp3.csv', header = T, stringsAsFactors = T) %>% 
  select(-c(expIndex, pset_selected, agent_selected)) %>% 
  dplyr::rename(s1 = weak_math, s2 = strong_math)

data_summary <- exp3_dat %>% 
  group_by(scenario, target, round) %>% 
  dplyr::summarize(uci_s1 = CI(s1)[1],
                   lci_s1 = CI(s1)[3],
                   uci_s2 = CI(s2)[1],
                   lci_s2 = CI(s2)[3],
                   s1 = mean(s1),
                   s2 = mean(s2)) %>% 
  mutate(model = 'data')
params <- exp3_dat %>%
  filter(round == 0 & subject == 1) %>% 
  select(scenario, target, s1, s2) %>% 
  arrange(scenario) %>% 
  mutate(title = paste0('[Scenario ', scenario,
                        ']\n Starting math levels: <', s1, ', ', s2, 
                        '>\n Target: ', target))

data_sim <- rbind(data_summary, sim %>% mutate(uci_s1 = NA, lci_s1 = NA,
                                               uci_s2 = NA, lci_s2 = NA))

p1 <- plot_2d(data_sim, 1) + labs(x = NULL, y = 'Math level of agent better at math') + theme(legend.position = 'none')
p2 <- plot_2d(data_sim, 5) + labs(x = NULL, y = NULL)
p3 <- plot_2d(data_sim, 2) + labs(x = NULL, y = 'Math level of agent better at math') + theme(legend.position = 'none')
p4 <- plot_2d(data_sim, 6) + labs(x = NULL, y = NULL) + theme(legend.position = 'none')
p5 <- plot_2d(data_sim, 3) + labs(x = NULL, y = 'Math level of agent better at math') + theme(legend.position = 'none')
p6 <- plot_2d(data_sim, 7) + labs(x = NULL, y = NULL) + theme(legend.position = 'none')
p7 <- plot_2d(data_sim, 4) + labs(x = 'Math level of agent worse at math', y = 'Math level of agent better at math') + theme(legend.position = 'none')
p8 <- plot_2d(data_sim, 8) + labs(x = 'Math level of agent worse at math', y = NULL) + theme(legend.position = 'none')

pdf('./../figures/raw/fig6.pdf', onefile = T, width = 10, height = 16)
(p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8)
dev.off()

## Part III. correlation
sim_longer <- sim %>% 
  pivot_longer(c(s1, s2), names_to = 'agent', values_to = 'strength') %>% 
  mutate(agent = recode(agent, 's1' = '1', 's2' = '2')) %>% 
  pivot_wider(names_from = model, values_from = strength)

exp3_longer <- exp3_dat %>% 
  pivot_longer(c(s1, s2), names_to = 'agent', values_to = 'strength') %>% 
  mutate(agent = substr(agent, 2, 2))

dat_summary <- exp3_longer %>%
  group_by(scenario, target, round, agent) %>%
  dplyr::summarize(avg_strength = mean(strength),
                   uci_strength = CI(strength)[1],
                   lci_strength = CI(strength)[3])
dat_merge <- merge(dat_summary, sim_longer)
dat_merge_long <- dat_merge %>% 
  pivot_longer(cols = c('planning', 'exploitation', 'equity', 'learning', 'equality'), 
               names_to = 'model',
               values_to = 'strength') %>% 
  filter(round != 0)

for (m in unique(dat_merge_long$model)) {
  print(paste0(m, ' model:'))
  print(cor.test(dat_merge_long$avg_strength[dat_merge_long$model == m], dat_merge_long$strength[dat_merge_long$model == m], method = 'pearson', use = 'complete.obs'))
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
write.table(model_evidence, 'exp3_model_evidence.csv', sep = ",", col.names = F, row.names = F)

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

pdf('./../figures/raw/fig3c.pdf', onefile = T, width = 5, height = 3)
p1
dev.off()

## calculate posterior exceedance probabilities and posteriors from model evidence estimates 
# using the bms function in the mfit package: https://github.com/sjgershm/mfit

# pxp: protected exceedance probabilities
pxp <- read.csv('exp3_protected_exceedance_probabilities.csv', header = T)
pxp$planning

## Part V. individual participant 2-D plots
dat_endpoint <- exp3_dat %>% 
  filter(round == 3) %>% 
  group_by(scenario, s1, s2) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(model = 'data')

p1 <- individual(dat_endpoint, 1) + labs(x = NULL, y = 'Math level of agent better at math')
p2 <- individual(dat_endpoint, 5) + labs(x = NULL, y = NULL)
p3 <- individual(dat_endpoint, 2) + labs(x = NULL, y = 'Math level of agent better at math')
p4 <- individual(dat_endpoint, 6) + labs(x = NULL, y = NULL)
p5 <- individual(dat_endpoint, 3) + labs(x = NULL, y = 'Math level of agent better at math')
p6 <- individual(dat_endpoint, 7) + labs(x = NULL, y = NULL)
p7 <- individual(dat_endpoint, 4) + labs(x = 'Math level of agent worse at math', y = 'Math level of agent better at math')
p8 <- individual(dat_endpoint, 8) + labs(x = 'Math level of agent worse at math', y = NULL)

pdf('./../figures/raw/fig7.pdf', onefile = T, width = 10, height = 16)
(p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8)
dev.off()
