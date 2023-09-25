library('tidyverse')
library('lmerTest')
library('Rmisc')
library('patchwork')
library('scales')
source('plot_helper.R')

## Experiment 1: Training
# 2-D plots
exp1_dat <- read.csv('./../Data/exp1.csv', header = T, stringsAsFactors = T) %>% 
  select(-c(expIndex, weight_selected, agent_selected, total)) %>% 
  dplyr::rename(s1 = weak_strength, s2 = strong_strength)
data_summary <- exp1_dat %>% 
  group_by(scenario, final_weight, round) %>% 
  dplyr::summarize(uci_s1 = CI(s1)[1],
                   lci_s1 = CI(s1)[3],
                   uci_s2 = CI(s2)[1],
                   lci_s2 = CI(s2)[3],
                   s1 = mean(s1),
                   s2 = mean(s2)) %>% 
  mutate(model = 'data')
params <- exp1_dat %>%
  filter(round == 0 & subject == 1) %>% 
  select(scenario, final_weight, s1, s2) %>% 
  arrange(scenario) %>% 
  mutate(title = paste0('[Scenario ', scenario,
                        ']\n Starting strengths: <', s1, ', ', s2, 
                        '>\n Target: ', final_weight))

sim <- read.csv('sim_training.csv', header = T, stringsAsFactors = T)
data_sim <- rbind(data_summary, sim %>% mutate(uci_s1 = NA, lci_s1 = NA,
                                               uci_s2 = NA, lci_s2 = NA))

p1 <- plot_2d(data_sim, 1) + labs(x = NULL) + theme(legend.position = 'none')
p2 <- plot_2d(data_sim, 2) + labs(x = NULL, y = NULL)
p3 <- plot_2d(data_sim, 3) + theme(legend.position = 'none')
p4 <- plot_2d(data_sim, 4) + labs(y = NULL) + theme(legend.position = 'none')

pdf('fig1a.pdf', onefile = T, width = 10, height = 8)
(p1 | p2) / (p3 | p4)
dev.off()

# Model comparison
exp1_dat <- exp1_dat %>% 
  pivot_longer(c(s1, s2), names_to = 'agent', values_to = 'strength') %>% 
  mutate(agent = substr(agent, 2, 2))
sim <- sim %>% 
  pivot_longer(c(s1, s2), names_to = 'agent', values_to = 'strength') %>% 
  mutate(agent = recode(agent, 's1' = '1', 's2' = '2')) %>% 
  pivot_wider(names_from = model, values_from = strength)
dat_merge <- merge(exp1_dat, sim) %>% 
  filter(round != 0) %>% 
  mutate(subject = as_factor(subject))

fit_lmm1 <- lmer(strength ~ planning + (planning|subject), dat_merge)
fit_lmm2 <- lmer(strength ~ exploitation + (exploitation|subject), dat_merge)
fit_lmm3 <- lmer(strength ~ equity + (equity|subject), dat_merge)
fit_lmm4 <- lmer(strength ~ learning + (learning|subject), dat_merge)

bic_table <- BIC(fit_lmm1, fit_lmm2, fit_lmm3, fit_lmm4) %>% 
  mutate(model = factor(c('planning', 'exploitation', 'equity', 'learning'),
                        levels = c('planning', 'exploitation', 'equity', 'learning'),
                        labels = c('Planning', 'Exploitation', 'Equity', 'Learning'),
                        ordered = T))

pdf('fig1b.pdf', onefile = T, width = 8, height = 3)
bic_plot(bic_table)
dev.off()

# Correlation
dat_summary <- exp1_dat %>%
  group_by(scenario, final_weight, round, agent) %>%
  dplyr::summarize(avg_strength = mean(strength),
                   uci_strength = CI(strength)[1],
                   lci_strength = CI(strength)[3])
dat_merge <- merge(dat_summary, sim)
dat_merge_long <- dat_merge %>% 
  pivot_longer(cols = c('planning', 'exploitation', 'equity', 'learning'), 
               names_to = 'model',
               values_to = 'strength') %>% 
  filter(round != 0)

text_labels <- data.frame(model = unique(dat_merge_long$model), x = 10, y = 1)
for (m in unique(dat_merge_long$model)) {
  correlation <- cor(dat_merge_long$avg_strength[dat_merge_long$model == m], dat_merge_long$strength[dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
  text_labels$text[text_labels$model == m] <- paste0('r = ', format(round(correlation, 2), nsmall = 2))
  # use cor.test to calculate p-values
}

pdf('fig1c.pdf', onefile = T, width = 10, height = 3)
cor_plt(dat_merge_long, 'strength', 'avg_strength', 'lci_strength', 'uci_strength', text_labels) +
  scale_x_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) +
  scale_y_continuous(limits = c(0,12), breaks = c(0,3,6,9,12)) +
  coord_fixed()
dev.off()


## Experiment 2: Team selection
# Bar plot
exp2_dat <- read.csv('./../Data/exp2.csv', header = T, stringsAsFactors = T)
num_subject <- length(unique(exp2_dat$subject))
counts <- table(exp2_dat$team, exp2_dat$final_weight)
p <- counts/num_subject
m <- data.frame(p) %>% 
  dplyr::rename(team = Var1, final_weight = Var2, prob = Freq) %>% 
  mutate(ci = sqrt(prob*(1-prob)/num_subject)*1.96) %>% 
  mutate(model = 'data')
params <- exp2_dat %>%
  filter(subject == 1) %>%
  select(scenario, final_weight) %>%
  arrange(scenario) %>%
  mutate(title = paste0('[Scenario ', scenario, 
                        ']\nStarting strengths: <2, 8, 20, 40>\nTarget: ', 
                        final_weight))

sim <- read.csv('sim_team_selection.csv', header = T, stringsAsFactors = T) %>% 
  mutate(ci = NA)
data_sim <- rbind(m, sim) %>% 
  mutate(scenario = case_when(final_weight == unique(params$final_weight)[1] ~ 1,
                              final_weight == unique(params$final_weight)[2] ~ 2,
                              final_weight == unique(params$final_weight)[3] ~ 3))

pdf('fig2a.pdf', onefile = T, width = 12, height = 6)
p1 <- bar_plot(data_sim, 1)
p2 <- bar_plot(data_sim, 2)
p3 <- bar_plot(data_sim, 3)
p1 + p2 + p3
dev.off()

# Model comparison
sim <- sim %>% 
  select(-ci) %>% 
  pivot_wider(names_from = model, values_from = prob)
dat_merge <- merge(m, sim)
mdl1 <- lm(prob ~ planning, data = dat_merge)
mdl2 <- lm(prob ~ exploitation, data = dat_merge)
mdl3 <- lm(prob ~ equity, data = dat_merge)
mdl4 <- lm(prob ~ learning, data = dat_merge)

bic_table <- BIC(mdl1, mdl2, mdl3, mdl4) %>% 
  mutate(model = factor(c('planning', 'exploitation', 'equity', 'learning'),
                        levels = c('planning', 'exploitation', 'equity', 'learning'),
                        labels = c('Planning', 'Exploitation', 'Equity', 'Learning'),
                        ordered = T))

pdf('fig2b.pdf', onefile = T, width = 8, height = 3)
bic_plot(bic_table)
dev.off()

# Correlation
dat_merge_long <- dat_merge %>% 
  select(-model) %>% 
  pivot_longer(cols = c('planning', 'exploitation', 'equity', 'learning'), 
               names_to = 'model',
               values_to = 'predicted_prob')

text_labels <- data.frame(model = unique(dat_merge_long$model), x = 0.8, y = 0.2)
for (m in unique(dat_merge_long$model)) {
  correlation <- cor(dat_merge_long$prob[dat_merge_long$model == m], dat_merge_long$predicted_prob[dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
  text_labels$text[text_labels$model == m] <- paste0('r = ', format(round(correlation, 2), nsmall = 2))
  # use cor.test to calculate p-values
}

pdf('fig2c.pdf', onefile = T, width = 10, height = 3)
cor_plt2(dat_merge_long, 'predicted_prob', 'prob', 'prob + ci', 'prob - ci', text_labels) +
  scale_x_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1)) +
  coord_fixed()
dev.off()

