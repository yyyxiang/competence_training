library('tidyverse')
library('patchwork')
library('viridis')
source('plot_helper.R')

# Appendix A: Individual participant 2-D plots in Experiment 1
exp1_dat <- read.csv('./../Data/exp1.csv', header = T, stringsAsFactors = T) %>% 
  select(-c(expIndex, weight_selected, agent_selected, total)) %>% 
  dplyr::rename(s1 = weak_strength, s2 = strong_strength)
params <- exp1_dat %>%
  filter(round == 0 & subject == 1) %>% 
  select(scenario, final_weight, s1, s2) %>% 
  mutate(title = paste0('[Scenario ', scenario,
                        ']\n Starting strengths: <', s1, ', ', s2, 
                        '>\n Target: ', final_weight))

dat_endpoint <- exp1_dat %>% 
  filter(round == 3) %>% 
  group_by(scenario, s1, s2) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(model = 'data')

p1 <- individual(dat_endpoint, 1) + theme(legend.position = 'none')
p2 <- individual(dat_endpoint, 2)
p3 <- individual(dat_endpoint, 3) + theme(legend.position = 'none')
p4 <- individual(dat_endpoint, 4) + theme(legend.position = 'none')

pdf('appendixA.pdf', onefile = T, width = 10, height = 8)
(p1 | p2) / (p3 | p4)
dev.off()

