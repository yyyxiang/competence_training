plot_2d <- function(data_frame, Scenario) {
  
  return (
    data_frame %>% 
      filter(scenario == Scenario) %>% 
      mutate(model = factor(model, levels = c('data', 'planning', 'exploitation', 'equity', 'learning', 'equality'), ordered = T)) %>%
      ggplot(aes(s1, s2, color = model, group = model, shape = model)) +
      geom_errorbar(aes(ymin = lci_s2, ymax = uci_s2)) +
      geom_errorbarh(aes(xmin = lci_s1, xmax = uci_s1)) +
      geom_line() +
      geom_point() +
      geom_abline(slope = -1, intercept = params$target[Scenario], linetype = 'dashed') +
      scale_color_manual(name = NULL,
                         labels = c('Data', 'Planning model', 'Exploitation model',
                                    'Equity model','Learning model', 'Equality model'),
                         values = c('#F8766D', '#e78ac3', '#a6d854', '#8da0cb', '#66c2a5', '#e5c494')) +
      scale_shape_discrete(name = NULL,
                           labels = c('Data', 'Planning model', 'Exploitation model',
                                      'Equity model','Learning model', 'Equality model')) +
      theme_bw() +
      theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) + 
      labs(title = params$title[Scenario])
  )
}

cor_plt <- function(data_frame, x, y, ymin, ymax, text_df) {
  
  return(
    data_frame %>% 
      mutate(scenario = as_factor(scenario)) %>% 
      ggplot(aes_string(x, y, color = 'scenario')) +
      geom_errorbar(aes_string(ymin = ymin, ymax = ymax, width = 0)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      scale_color_manual(values = c('#76b7b2', '#edc948', '#ff9da7', '#b07aa1')) +
      geom_text(data = text_df,
                mapping = aes(label = text, x = x , y = y),
                inherit.aes = F,
                fontface='italic',
                size = 5) +
      facet_grid(~ factor(model, levels = c('planning', 'exploitation', 'equity', 'learning', 'equality'), ordered = T), 
                 labeller = as_labeller(c(`planning`='Planning', `exploitation`='Exploitation', 
                                          `equity`='Equity', `learning`='Learning', `equality`='Equality'))) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(color = 'Scenario', x = 'Model', y = 'Data')
  )
}

bic_plot <- function(data_frame) {
  
  return (
    data_frame %>% 
      ggplot(aes(model, BIC, color = model)) +
      geom_point(size = 3) +
      scale_color_manual(values = c('#e78ac3', '#a6d854', '#8da0cb', '#66c2a5', '#e5c494')) +
      theme_bw() +
      theme(legend.position = 'none') +
      labs(x = 'Model')
  )
}

bar_plot <- function(data_frame, Scenario) {
  
  return (
    data_frame %>% 
      filter(scenario == Scenario) %>% 
      mutate(model = factor(model, ordered = T,
                            levels = c('data', 'planning', 'exploitation', 'equity', 'learning', 'equality')),
             team = factor(team, levels = c('<2,8>', '<2,20>', '<2,40>', '<8,20>', '<8,40>', '<20,40>'), ordered = T)) %>% 
      ggplot(aes(team, prob, fill = model)) +
      geom_col(position = 'dodge2') +
      geom_errorbar(aes(ymin = prob-ci, ymax = prob+ci), width = .1, position = position_dodge(.9)) +
      coord_cartesian(ylim = c(0,1)) +
      scale_fill_manual(name = NULL,
                        values = c('#F8766D', '#e78ac3', '#a6d854', '#8da0cb', '#66c2a5', '#e5c494')) +
      facet_wrap(~ model, nrow = 6,
                 labeller = as_labeller(c(`data` = 'Data', `planning`='Planning model', `exploitation`='Exploitation model',
                                          `equity`='Equity model', `learning`='Learning model', `equality`='Equality model'))) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), legend.position = 'none') +
      labs(title = params$title[Scenario], x = 'Team', y = 'Probability')
      
  )
}

cor_plt2 <- function(data_frame, x, y, ymin, ymax, text_df) {
  
  return(
    data_frame %>% 
      mutate(target = as_factor(target)) %>% 
      ggplot(aes_string(x, y, color = 'target')) +
      geom_errorbar(aes_string(ymin = ymin, ymax = ymax, width = 0)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      scale_color_manual(values = c('#EA8331', '#00BAE0', '#9590FF')) +
      geom_text(data = text_df,
                mapping = aes(label = text, x = x , y = y),
                inherit.aes = F,
                fontface='italic',
                size = 5) +
      facet_grid(~ factor(model, levels = c('planning', 'exploitation', 'equity', 'learning', 'equality'), ordered = T), 
                 labeller = as_labeller(c(`planning`='Planning', `exploitation`='Exploitation', 
                                          `equity`='Equity', `learning`='Learning', `equality`='Equality'))) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(color = 'Target', x = 'Model', y = 'Data')
  )
}

individual <- function(data_frame, Scenario) {
  
  return(
    data_frame %>%
      filter(scenario == Scenario) %>%
      arrange(n) %>% 
      ggplot(aes(s1, s2,  color = n)) +
      geom_segment(aes(x = params$s1[Scenario], y = params$s2[Scenario], xend = s1, yend = s2),
                   linewidth = 1.5,
                   arrow = arrow(length = unit(0.05, 'npc'))) +
      geom_abline(slope = -1, intercept = params$target[Scenario], linetype = 'dashed') +
      scale_color_viridis(option = 'inferno', discrete = F, begin = 0.85, end = 0.2) +
      scale_size_continuous(range=c(0.5,3), guide = NULL) +
      theme_bw() +
      theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) +
      labs(title = params$title[Scenario])
  )
}

