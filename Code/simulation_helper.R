lift_outcome <- function(strength, effort, weight){
  return(strength*effort >= weight)
}

# set agent_reward to an extremely large number because we assume agents are highly motivated
lift_utility <- function(strength, effort, weight, agent_reward=1000, rho=1){
  outcome <-  lift_outcome(strength, effort, weight)
  utility <- agent_reward*outcome - rho*effort
  return(utility)
}

lift_effort <- function(strength, weight, agent_reward=1000, rho=1){
  utilities <- sapply(efforts, function(effort){
    lift_utility(strength, effort, weight, agent_reward, rho=1)
  })
  effort <- efforts[which.max(utilities)]
  return(effort)
}

update_strength <- function(strength, effort, chosen, rho=1){
  new_strength = strength + chosen*rho*effort
  return(new_strength)
}

simulate_training_probabilities <- function(initial_s1, initial_s2, weights, target) {
  df <- expand.grid(r1_agent = c(1,2), r1_weight = weights, 
                    r2_agent = c(1,2), r2_weight = weights, 
                    r3_agent = c(1,2), r3_weight = weights) %>% 
    mutate(r1_agent = if_else(r1_weight == 0, 0, r1_agent),
           r2_agent = if_else(r2_weight == 0, 0, r2_agent),
           r3_agent = if_else(r3_weight == 0, 0, r3_agent),
           r0_agent = NA, r0_weight = NA, r0_s1 = initial_s1, r0_s2 = initial_s2) %>% 
    distinct()
  for (i in 1:length(df$r1_agent)) {
    df$r1_s1[i] = update_strength(df$r0_s1[i], lift_effort(df$r0_s1[i], df$r1_weight[i]), df$r1_agent[i] == 1)
    df$r1_s2[i] = update_strength(df$r0_s2[i], lift_effort(df$r0_s2[i], df$r1_weight[i]), df$r1_agent[i] == 2)
    df$r2_s1[i] = update_strength(df$r1_s1[i], lift_effort(df$r1_s1[i], df$r2_weight[i]), df$r2_agent[i] == 1)
    df$r2_s2[i] = update_strength(df$r1_s2[i], lift_effort(df$r1_s2[i], df$r2_weight[i]), df$r2_agent[i] == 2)
    df$r3_s1[i] = update_strength(df$r2_s1[i], lift_effort(df$r2_s1[i], df$r3_weight[i]), df$r3_agent[i] == 1)
    df$r3_s2[i] = update_strength(df$r2_s2[i], lift_effort(df$r2_s2[i], df$r3_weight[i]), df$r3_agent[i] == 2)
  }
  
  # planning model: cost-benefit
  df_planning <- df %>% 
    mutate(R = case_when((r3_s1 + r3_s2) >= target ~ reward, T ~ 0)) %>% 
    mutate(C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = R - C) %>% 
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value)) %>% 
    mutate(strategy = paste0(r1_agent, r1_weight, r2_agent, r2_weight, r3_agent, r3_weight)) %>% 
    select(strategy, prob) %>% 
    mutate(model = 'planning')
  
  # exploitation model: maximize stronger agent's strength after training
  df_exploitation <- df %>% 
    mutate(stronger_strength = if_else(r3_s1 >= r3_s2, r3_s1, r3_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = stronger_strength - C) %>%
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value)) %>% 
    mutate(strategy = paste0(r1_agent, r1_weight, r2_agent, r2_weight, r3_agent, r3_weight)) %>% 
    select(strategy, prob) %>% 
    mutate(model = 'exploitation')
  
  # equity model: minimize strength difference after training
  df_equity <- df %>% 
    mutate(strength_diff = -abs(r3_s1-r3_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = strength_diff - C) %>%
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value)) %>% 
    mutate(strategy = paste0(r1_agent, r1_weight, r2_agent, r2_weight, r3_agent, r3_weight)) %>% 
    select(strategy, prob) %>% 
    mutate(model = 'equity')
  
  # learning model: maximize total strength increase
  df_learning <- df %>% 
    mutate(total_increase = (r3_s1-r0_s1) + (r3_s2-r0_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = total_increase - C) %>% 
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value)) %>% 
    mutate(strategy = paste0(r1_agent, r1_weight, r2_agent, r2_weight, r3_agent, r3_weight)) %>% 
    select(strategy, prob) %>% 
    mutate(model = 'learning')
  
  # equality model: invest equally in agents
  df_equality <- df %>% 
    mutate(C = (r1_weight + r2_weight + r3_weight) * alpha,
           action_value = -C) %>% 
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value)) %>% 
    mutate(strategy = paste0(r1_agent, r1_weight, r2_agent, r2_weight, r3_agent, r3_weight)) %>% 
    select(strategy, prob) %>% 
    mutate(model = 'equality')
  
  df <- rbind(df_planning, df_exploitation, df_equity, df_learning, df_equality)
  df <- df %>% 
    pivot_wider(names_from = model, values_from = prob)
  
  return(df)
}

# different gamma for each model
simulate_training_strength <- function(initial_s1, initial_s2, weights, target) {
  df <- expand.grid(r1_agent = c(1,2), r1_weight = weights, 
                    r2_agent = c(1,2), r2_weight = weights, 
                    r3_agent = c(1,2), r3_weight = weights) %>% 
    mutate(r1_agent = if_else(r1_weight == 0, 0, r1_agent),
           r2_agent = if_else(r2_weight == 0, 0, r2_agent),
           r3_agent = if_else(r3_weight == 0, 0, r3_agent),
           r0_agent = NA, r0_weight = NA, r0_s1 = initial_s1, r0_s2 = initial_s2) %>% 
    distinct()
  for (i in 1:length(df$r1_agent)) {
    df$r1_s1[i] = update_strength(df$r0_s1[i], lift_effort(df$r0_s1[i], df$r1_weight[i]), df$r1_agent[i] == 1)
    df$r1_s2[i] = update_strength(df$r0_s2[i], lift_effort(df$r0_s2[i], df$r1_weight[i]), df$r1_agent[i] == 2)
    df$r2_s1[i] = update_strength(df$r1_s1[i], lift_effort(df$r1_s1[i], df$r2_weight[i]), df$r2_agent[i] == 1)
    df$r2_s2[i] = update_strength(df$r1_s2[i], lift_effort(df$r1_s2[i], df$r2_weight[i]), df$r2_agent[i] == 2)
    df$r3_s1[i] = update_strength(df$r2_s1[i], lift_effort(df$r2_s1[i], df$r3_weight[i]), df$r3_agent[i] == 1)
    df$r3_s2[i] = update_strength(df$r2_s2[i], lift_effort(df$r2_s2[i], df$r3_weight[i]), df$r3_agent[i] == 2)
  }
  
  # planning model: cost-benefit
  df_planning <- df %>% 
    mutate(R = case_when((r3_s1 + r3_s2) >= target ~ reward, T ~ 0)) %>% 
    mutate(C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(net_reward = R - C) %>% 
    mutate(exp_net_reward = exp(net_reward*gamma_planning),
           prob = exp_net_reward/sum(exp_net_reward),
           index = rownames(df)) %>% 
    select(-c(R, C, net_reward)) %>% 
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
    separate(name, c('round', 'key')) %>% 
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>% 
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
    select(round, s1_prob, s2_prob) %>% 
    group_by(round) %>% 
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
    mutate(model = 'planning')
  
  # exploitation model: maximize stronger agent's strength after training
  df_exploitation <- df %>% 
    mutate(stronger_strength = if_else(r3_s1 >= r3_s2, r3_s1, r3_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = stronger_strength - C) %>%
    mutate(exp_action_value = exp(action_value*gamma_exploitation),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>% 
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
    separate(name, c('round', 'key')) %>% 
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>% 
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
    select(round, s1_prob, s2_prob) %>% 
    group_by(round) %>% 
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
    mutate(model = 'exploitation')
  
  # equity model: minimize strength difference after training
  df_equity <- df %>% 
    mutate(strength_diff = -abs(r3_s1-r3_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = strength_diff - C) %>%
    mutate(exp_action_value = exp(action_value*gamma_equity),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>% 
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
    separate(name, c('round', 'key')) %>% 
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>% 
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
    select(round, s1_prob, s2_prob) %>% 
    group_by(round) %>% 
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
    mutate(model = 'equity')
  
  # learning model: maximize total strength increase
  df_learning <- df %>% 
    mutate(total_increase = (r3_s1-r0_s1) + (r3_s2-r0_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
    mutate(action_value = total_increase - C) %>% 
    mutate(exp_action_value = exp(action_value*gamma_learning),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>% 
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
    separate(name, c('round', 'key')) %>% 
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>% 
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
    select(round, s1_prob, s2_prob) %>% 
    group_by(round) %>% 
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
    mutate(model = 'learning')
  
  # equality model: invest equally in agents
  df_equality <- df %>% 
    mutate(C = (r1_weight + r2_weight + r3_weight) * alpha,
           action_value = -C) %>% 
    mutate(exp_action_value = exp(action_value*gamma_equality),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>% 
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
    separate(name, c('round', 'key')) %>% 
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>% 
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
    select(round, s1_prob, s2_prob) %>% 
    group_by(round) %>% 
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
    mutate(model = 'equality')
  
  df <- rbind(df_planning, df_exploitation, df_equity, df_learning, df_equality)
  return(df)
}


########## dump ##########

# same gamma for all models
simulate_training_same_gamma <- function(initial_s1, initial_s2, weights, target) {
  df <- expand.grid(r1_agent = c(1,2), r1_weight = weights,
                    r2_agent = c(1,2), r2_weight = weights,
                    r3_agent = c(1,2), r3_weight = weights) %>%
    mutate(r1_agent = if_else(r1_weight == 0, 0, r1_agent),
           r2_agent = if_else(r2_weight == 0, 0, r2_agent),
           r3_agent = if_else(r3_weight == 0, 0, r3_agent),
           r0_agent = NA, r0_weight = NA, r0_s1 = initial_s1, r0_s2 = initial_s2) %>%
    distinct()
  for (i in 1:length(df$r1_agent)) {
    df$r1_s1[i] = update_strength(df$r0_s1[i], lift_effort(df$r0_s1[i], df$r1_weight[i]), df$r1_agent[i] == 1)
    df$r1_s2[i] = update_strength(df$r0_s2[i], lift_effort(df$r0_s2[i], df$r1_weight[i]), df$r1_agent[i] == 2)
    df$r2_s1[i] = update_strength(df$r1_s1[i], lift_effort(df$r1_s1[i], df$r2_weight[i]), df$r2_agent[i] == 1)
    df$r2_s2[i] = update_strength(df$r1_s2[i], lift_effort(df$r1_s2[i], df$r2_weight[i]), df$r2_agent[i] == 2)
    df$r3_s1[i] = update_strength(df$r2_s1[i], lift_effort(df$r2_s1[i], df$r3_weight[i]), df$r3_agent[i] == 1)
    df$r3_s2[i] = update_strength(df$r2_s2[i], lift_effort(df$r2_s2[i], df$r3_weight[i]), df$r3_agent[i] == 2)
  }

  # planning model: cost-benefit
  df_planning <- df %>%
    mutate(R = case_when((r3_s1 + r3_s2) >= target ~ reward, T ~ 0)) %>%
    mutate(C = (r1_weight + r2_weight + r3_weight) * alpha) %>%
    mutate(net_reward = R - C) %>%
    mutate(exp_net_reward = exp(net_reward*gamma),
           prob = exp_net_reward/sum(exp_net_reward),
           index = rownames(df)) %>%
    select(-c(R, C, net_reward)) %>%
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>%
    separate(name, c('round', 'key')) %>%
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>%
    select(round, s1_prob, s2_prob) %>%
    group_by(round) %>%
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>%
    mutate(model = 'planning')

  # exploitation model: maximize stronger agent's strength after training
  df_exploitation <- df %>%
    mutate(stronger_strength = if_else(r3_s1 >= r3_s2, r3_s1, r3_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>%
    mutate(action_value = stronger_strength - C) %>%
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>%
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>%
    separate(name, c('round', 'key')) %>%
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>%
    select(round, s1_prob, s2_prob) %>%
    group_by(round) %>%
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>%
    mutate(model = 'exploitation')

  # equity model: minimize strength difference after training
  df_equity <- df %>%
    mutate(strength_diff = -abs(r3_s1-r3_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>%
    mutate(action_value = strength_diff - C) %>%
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>%
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>%
    separate(name, c('round', 'key')) %>%
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>%
    select(round, s1_prob, s2_prob) %>%
    group_by(round) %>%
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>%
    mutate(model = 'equity')

  # learning model: maximize total strength increase
  df_learning <- df %>%
    mutate(total_increase = (r3_s1-r0_s1) + (r3_s2-r0_s2),
           C = (r1_weight + r2_weight + r3_weight) * alpha) %>%
    mutate(action_value = total_increase - C) %>%
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>%
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>%
    separate(name, c('round', 'key')) %>%
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>%
    select(round, s1_prob, s2_prob) %>%
    group_by(round) %>%
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>%
    mutate(model = 'learning')

  # equality model: invest equally in agents
  df_equality <- df %>%
    # mutate(a1_box = (r1_agent == 1)*r1_weight + (r2_agent == 1)*r2_weight + (r3_agent == 1)*r3_weight,
    #        a2_box = (r1_agent == 2)*r1_weight + (r2_agent == 2)*r2_weight + (r3_agent == 2)*r3_weight,
    #        box_diff = -abs(a1_box - a2_box),
    #        C = (r1_weight + r2_weight + r3_weight) * alpha) %>%
    # mutate(action_value = box_diff - C) %>%
    mutate(C = (r1_weight + r2_weight + r3_weight) * alpha,
           action_value = -C) %>%
    mutate(exp_action_value = exp(action_value*gamma),
           prob = exp_action_value/sum(exp_action_value),
           index = rownames(df)) %>%
    pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>%
    separate(name, c('round', 'key')) %>%
    mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>%
    select(round, s1_prob, s2_prob) %>%
    group_by(round) %>%
    dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>%
    mutate(model = 'equality')

  # # rawlsian model: maximize weaker agent's strength after training
  # df_rawlsian <- df %>% 
  #   mutate(weaker_strength = if_else(r3_s1 <= r3_s2, r3_s1, r3_s2),
  #          C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
  #   mutate(action_value = weaker_strength - C) %>%
  #   mutate(exp_action_value = exp(action_value*gamma),
  #          prob = exp_action_value/sum(exp_action_value),
  #          index = rownames(df)) %>% 
  #   pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
  #   separate(name, c('round', 'key')) %>% 
  #   mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
  #   pivot_wider(names_from = key, values_from = value) %>% 
  #   mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
  #   select(round, s1_prob, s2_prob) %>% 
  #   group_by(round) %>% 
  #   dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
  #   mutate(model = 'rawlsian')
  # 
  # # utilitarian model: maximize sum of agents' strength after training
  # df_utilitarian <- df %>% 
  #   mutate(sum_strength = r3_s1 + r3_s2,
  #          C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
  #   mutate(action_value = sum_strength - C) %>%
  #   mutate(exp_action_value = exp(action_value*gamma),
  #          prob = exp_action_value/sum(exp_action_value),
  #          index = rownames(df)) %>% 
  #   pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
  #   separate(name, c('round', 'key')) %>% 
  #   mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
  #   pivot_wider(names_from = key, values_from = value) %>% 
  #   mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
  #   select(round, s1_prob, s2_prob) %>% 
  #   group_by(round) %>% 
  #   dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
  #   mutate(model = 'utilitarian')
  
  df <- rbind(df_planning, df_exploitation, df_equity, df_learning, df_equality)
  return(df)
}

# # transformed strategies to combinations
# simulate_training_probabilities_combination <- function(initial_s1, initial_s2, weights, target) {
#   df <- expand.grid(r1_agent = c(1,2), r1_weight = weights, 
#                     r2_agent = c(1,2), r2_weight = weights, 
#                     r3_agent = c(1,2), r3_weight = weights) %>% 
#     mutate(r1_agent = if_else(r1_weight == 0, 0, r1_agent),
#            r2_agent = if_else(r2_weight == 0, 0, r2_agent),
#            r3_agent = if_else(r3_weight == 0, 0, r3_agent),
#            r0_agent = NA, r0_weight = NA, r0_s1 = initial_s1, r0_s2 = initial_s2) %>% 
#     distinct()
#   for (i in 1:length(df$r1_agent)) {
#     df$r1_s1[i] = update_strength(df$r0_s1[i], lift_effort(df$r0_s1[i], df$r1_weight[i]), df$r1_agent[i] == 1)
#     df$r1_s2[i] = update_strength(df$r0_s2[i], lift_effort(df$r0_s2[i], df$r1_weight[i]), df$r1_agent[i] == 2)
#     df$r2_s1[i] = update_strength(df$r1_s1[i], lift_effort(df$r1_s1[i], df$r2_weight[i]), df$r2_agent[i] == 1)
#     df$r2_s2[i] = update_strength(df$r1_s2[i], lift_effort(df$r1_s2[i], df$r2_weight[i]), df$r2_agent[i] == 2)
#     df$r3_s1[i] = update_strength(df$r2_s1[i], lift_effort(df$r2_s1[i], df$r3_weight[i]), df$r3_agent[i] == 1)
#     df$r3_s2[i] = update_strength(df$r2_s2[i], lift_effort(df$r2_s2[i], df$r3_weight[i]), df$r3_agent[i] == 2)
#   }
#   
#   # planning model: cost-benefit
#   df_planning <- df %>% 
#     mutate(R = case_when((r3_s1 + r3_s2) >= target ~ reward, T ~ 0)) %>% 
#     mutate(C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
#     mutate(action_value = R - C) %>% 
#     mutate(exp_action_value = exp(action_value*gamma),
#            prob = exp_action_value/sum(exp_action_value),
#            index = rownames(df)) %>% 
#     mutate(r1 = paste0(r1_agent, r1_weight),
#            r2 = paste0(r2_agent, r2_weight),
#            r3 = paste0(r3_agent, r3_weight)) %>% 
#     rowwise() %>% 
#     mutate(combination = paste(sort(c(r1, r2, r3)), collapse = " ")) %>% 
#     ungroup() %>% 
#     group_by(combination) %>% 
#     dplyr::summarize(prob = sum(prob)) %>% 
#     mutate(model = 'planning')
#   
#   # exploitation model: maximize stronger agent's strength after training
#   df_exploitation <- df %>% 
#     mutate(stronger_strength = if_else(r3_s1 >= r3_s2, r3_s1, r3_s2),
#            C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
#     mutate(action_value = stronger_strength - C) %>%
#     mutate(exp_action_value = exp(action_value*gamma),
#            prob = exp_action_value/sum(exp_action_value),
#            index = rownames(df)) %>% 
#     mutate(r1 = paste0(r1_agent, r1_weight),
#            r2 = paste0(r2_agent, r2_weight),
#            r3 = paste0(r3_agent, r3_weight)) %>% 
#     rowwise() %>% 
#     mutate(combination = paste(sort(c(r1, r2, r3)), collapse = " ")) %>% 
#     ungroup() %>% 
#     group_by(combination) %>% 
#     dplyr::summarize(prob = sum(prob)) %>%
#     mutate(model = 'exploitation')
#   
#   # equity model: minimize strength difference after training
#   df_equity <- df %>% 
#     mutate(strength_diff = -abs(r3_s1-r3_s2),
#            C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
#     mutate(action_value = strength_diff - C) %>%
#     mutate(exp_action_value = exp(action_value*gamma),
#            prob = exp_action_value/sum(exp_action_value),
#            index = rownames(df)) %>% 
#     mutate(r1 = paste0(r1_agent, r1_weight),
#            r2 = paste0(r2_agent, r2_weight),
#            r3 = paste0(r3_agent, r3_weight)) %>% 
#     rowwise() %>% 
#     mutate(combination = paste(sort(c(r1, r2, r3)), collapse = " ")) %>% 
#     ungroup() %>% 
#     group_by(combination) %>% 
#     dplyr::summarize(prob = sum(prob)) %>% 
#     mutate(model = 'equity')
#   
#   # learning model: maximize total strength increase
#   df_learning <- df %>% 
#     mutate(total_increase = (r3_s1-r0_s1) + (r3_s2-r0_s2),
#            C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
#     mutate(action_value = total_increase - C) %>% 
#     mutate(exp_action_value = exp(action_value*gamma),
#            prob = exp_action_value/sum(exp_action_value),
#            index = rownames(df)) %>% 
#     mutate(r1 = paste0(r1_agent, r1_weight),
#            r2 = paste0(r2_agent, r2_weight),
#            r3 = paste0(r3_agent, r3_weight)) %>% 
#     rowwise() %>% 
#     mutate(combination = paste(sort(c(r1, r2, r3)), collapse = " ")) %>% 
#     ungroup() %>% 
#     group_by(combination) %>% 
#     dplyr::summarize(prob = sum(prob)) %>% 
#     mutate(model = 'learning')
#   
#   # equality model: invest equally in agents
#   df_equality <- df %>% 
#     mutate(C = (r1_weight + r2_weight + r3_weight) * alpha,
#            action_value = -C) %>% 
#     mutate(exp_action_value = exp(action_value*gamma),
#            prob = exp_action_value/sum(exp_action_value),
#            index = rownames(df)) %>% 
#     mutate(r1 = paste0(r1_agent, r1_weight),
#            r2 = paste0(r2_agent, r2_weight),
#            r3 = paste0(r3_agent, r3_weight)) %>% 
#     rowwise() %>% 
#     mutate(combination = paste(sort(c(r1, r2, r3)), collapse = " ")) %>% 
#     ungroup() %>% 
#     group_by(combination) %>% 
#     dplyr::summarize(prob = sum(prob)) %>% 
#     mutate(model = 'equality')
#   
#   df <- rbind(df_planning, df_exploitation, df_equity, df_learning, df_equality)
#   df <- df %>% 
#     pivot_wider(names_from = model, values_from = prob)
#   
#   return(df)
# }

# simulate_training_wo_cost <- function(initial_s1, initial_s2, weights, target) {
#   df <- expand.grid(r1_agent = c(1,2), r1_weight = weights, 
#                     r2_agent = c(1,2), r2_weight = weights, 
#                     r3_agent = c(1,2), r3_weight = weights) %>% 
#     mutate(r1_agent = if_else(r1_weight == 0, 0, r1_agent),
#            r2_agent = if_else(r2_weight == 0, 0, r2_agent),
#            r3_agent = if_else(r3_weight == 0, 0, r3_agent),
#            r0_agent = NA, r0_weight = NA, r0_s1 = initial_s1, r0_s2 = initial_s2) %>% 
#     distinct()
#   for (i in 1:length(df$r1_agent)) {
#     df$r1_s1[i] = update_strength(df$r0_s1[i], lift_effort(df$r0_s1[i], df$r1_weight[i]), df$r1_agent[i] == 1)
#     df$r1_s2[i] = update_strength(df$r0_s2[i], lift_effort(df$r0_s2[i], df$r1_weight[i]), df$r1_agent[i] == 2)
#     df$r2_s1[i] = update_strength(df$r1_s1[i], lift_effort(df$r1_s1[i], df$r2_weight[i]), df$r2_agent[i] == 1)
#     df$r2_s2[i] = update_strength(df$r1_s2[i], lift_effort(df$r1_s2[i], df$r2_weight[i]), df$r2_agent[i] == 2)
#     df$r3_s1[i] = update_strength(df$r2_s1[i], lift_effort(df$r2_s1[i], df$r3_weight[i]), df$r3_agent[i] == 1)
#     df$r3_s2[i] = update_strength(df$r2_s2[i], lift_effort(df$r2_s2[i], df$r3_weight[i]), df$r3_agent[i] == 2)
#   }
#   
#   # planning model: cost-benefit
#   df_planning <- df %>% 
#     mutate(R = case_when((r3_s1 + r3_s2) >= target ~ reward, T ~ 0)) %>% 
#     mutate(C = (r1_weight + r2_weight + r3_weight) * alpha) %>% 
#     mutate(net_reward = R - C) %>% 
#     mutate(exp_net_reward = exp(net_reward*gamma),
#            prob = exp_net_reward/sum(exp_net_reward),
#            index = rownames(df)) %>% 
#     select(-c(R, C, net_reward)) %>% 
#     pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
#     separate(name, c('round', 'key')) %>% 
#     mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
#     pivot_wider(names_from = key, values_from = value) %>% 
#     mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
#     select(round, s1_prob, s2_prob) %>% 
#     group_by(round) %>% 
#     dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
#     mutate(model = 'planning')
#   
#   # learning model: maximize total strength increase
#   df_learning <- df %>% 
#     mutate(total_increase = (r3_s1-r0_s1) + (r3_s2-r0_s2),
#            exp_increase = exp(total_increase*gamma),
#            prob = exp_increase/sum(exp_increase),
#            index = rownames(df)) %>% 
#     pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
#     separate(name, c('round', 'key')) %>% 
#     mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
#     pivot_wider(names_from = key, values_from = value) %>% 
#     mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
#     select(round, s1_prob, s2_prob) %>% 
#     group_by(round) %>% 
#     dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
#     mutate(model = 'learning')
#   
#   # exploitation model: maximize stronger agent's strength after training
#   df_exploitation <- df %>% 
#     mutate(exp_stronger_strength = if_else(r3_s1 >= r3_s2, exp(r3_s1*gamma), exp(r3_s2*gamma)),
#            prob = exp_stronger_strength/sum(exp_stronger_strength),
#            index = rownames(df)) %>% 
#     pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
#     separate(name, c('round', 'key')) %>% 
#     mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
#     pivot_wider(names_from = key, values_from = value) %>% 
#     mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
#     select(round, s1_prob, s2_prob) %>% 
#     group_by(round) %>% 
#     dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
#     mutate(model = 'exploitation')
#   
#   # equity model: minimize strength difference after training
#   df_equity <- df %>% 
#     mutate(strength_diff = -abs(r3_s1-r3_s2),
#            exp_strength_diff = exp(strength_diff*gamma),
#            prob = exp_strength_diff/sum(exp_strength_diff),
#            index = rownames(df)) %>% 
#     pivot_longer(starts_with('r'), names_to = 'name', values_to = 'value') %>% 
#     separate(name, c('round', 'key')) %>% 
#     mutate(round = recode(round, 'r0' = '0', 'r1' = '1', 'r2' = '2', 'r3' = '3')) %>%
#     pivot_wider(names_from = key, values_from = value) %>% 
#     mutate(s1_prob = s1*prob, s2_prob = s2*prob) %>% 
#     select(round, s1_prob, s2_prob) %>% 
#     group_by(round) %>% 
#     dplyr::summarize(s1 = sum(s1_prob), s2 = sum(s2_prob)) %>% 
#     mutate(model = 'equity')
#   
#   df <- rbind(df_planning, df_learning, df_exploitation, df_equity)
#   return(df)
# }
