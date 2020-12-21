promethee123<- function(alternatives, criteria, decision_matrix, min_max, normalization_function,
                              q_indifference, p_preference, s_curve_change, criteria_weights){

  n_alt <- length(alternatives)
  n_crit <- length(criteria)



  differences <- c()
  for (j in 1:n_crit) {
    for (i in 1:n_alt) {
      for (h in 1:n_alt) {
        if (min_max[j] == "max") {
          value <- (decision_matrix[j,i] - decision_matrix[j,h])
        }
        if (min_max[j] == "min") {
          value <- (decision_matrix[j,h] - decision_matrix[j,i])
        }
        differences <- append(differences, value)
      }
    }
  }



  normalized <- c()
  x <- 1
  y <- (n_alt^2)
  for (j in 1:n_crit) {
    for (i in x:y) {

      if (normalization_function[j] == 1){
        value <- differences[i]
        if (value <= 0){
          degree <- 0
        }else{
          degree <- 1
        }
      }
      if (normalization_function[j] == 2){
        value <- differences[i]
        if (value <= q_indifference[j]){
          degree <- 0
        }else{
          degree <- 1
        }
      }
      if (normalization_function[j] == 3){
        value <- differences[i]
        if (value <= 0){
          degree <- 0
        }
        if (value > 0 && value < p_preference[j]){
          degree <- value/p_preference[j]
        }
        if (value > p_preference[j]){
          degree <- 1
        }
      }
      if (normalization_function[j] == 4){
        value <- differences[i]
        if (value <= q_indifference[j]){
          degree <- 0
        }
        if (value > q_indifference[j] && value < p_preference[j]){
          degree <- 0.5
        }
        if (value > p_preference[j]){
          degree <- 1
        }
      }
      if (normalization_function[j] == 5){
        value <- differences[i]
        if (value <= q_indifference[j]){
          degree <- 0
        }
        if (value > q_indifference[j] && value < p_preference[j]){
          degree <- ((value - q_indifference[j]) / (p_preference[j] - q_indifference[j]))
        }
        if (value >= p_preference[j]){
          degree <- 1
        }
      }
      if (normalization_function[j] == 6){
        value <- differences[i]
        if (value <= 0){
          degree <- 0
        }else{
          degree <- round((1 - (exp(1) ** ((-((x)**2))/(2*(s_curve_change[j] ** 2))))), 3)
        }
      }
      normalized <- append(normalized, degree)
    }

    x <- (y+1)
    y <- (y+(n_alt^2))

  }



  weighted <- c()
  x <- 1
  y <- (n_alt^2)
  for (j in 1:n_crit) {
    for (i in x:y) {
      value <- normalized[i]*criteria_weights[j]
      weighted <- append(weighted, value)
    }
    x <- (y+1)
    y <- (y+(n_alt^2))
  }
  print('')
  print("========== Alternative Performances in Each Criterion ==========")
  print('')
  x <- 1
  y <- (n_alt^2)

  for (j in 1:n_crit) {

    weighted_crit <- weighted[x:y]

    matrix_weighted <- matrix(weighted_crit, nrow = n_alt, ncol = n_alt, byrow = TRUE)
    rownames(matrix_weighted) <- alternatives
    colnames(matrix_weighted) <- alternatives
    print('')
    print(paste( 'Weighted Matrix relative to criterion', criteria[j] ))
    print(matrix_weighted)

    x <- (y+1)
    y <- (y+(n_alt^2))

  }


  global_index <- c()
  for (i in 1:(n_alt^2)) {
    value_sum <- 0
    for (h in 1:n_crit) {
      value <- weighted[((n_alt^2)*(h-1))+i]
      value_sum <- value_sum + value
    }
    value_index <- round(value_sum/n_crit, 4)
    global_index <- append(global_index, value_index)
  }
  matrix_global_index <- matrix(global_index, nrow = n_alt, ncol = n_alt, byrow = TRUE)
  colnames(matrix_global_index) <- alternatives
  rownames(matrix_global_index) <- alternatives
  print("==================== Global Index of Preference ====================")
  print('')
  print(matrix_global_index)
  print('')



  positive_flows <- c()
  for (i in 1:n_alt) {
    pos_flow <- 0
    for (h in 1:n_alt) {
      value <- global_index[((n_alt*(i-1))+h)]
      pos_flow <- pos_flow + value
    }
    positive_flows <- append(positive_flows, pos_flow)
  }

  negative_flows <- c()
  for (i in 1:n_alt) {
    neg_flow <- 0
    for (h in 1:n_alt) {
      value <- global_index[((n_alt*(h-1))+i)]
      neg_flow <- neg_flow + value
    }
    negative_flows <- append(negative_flows, neg_flow)
  }

  net_flows <- c()
  for (i in 1:n_alt) {
    value <- positive_flows[i] - negative_flows[i]
    net_flows <- append(net_flows, value)
  }

  Flows <- data.frame(alternatives, positive_flows, negative_flows, net_flows)
  print('')
  print("==================== Outranking Flows ====================")
  print('')
  print(Flows)
  print('')

  print('')
  print("==================== PROMETHEE I ====================")
  print('')
  for (i in 1:n_alt) {
    print(paste('Partial Prefernce Relation of ', alternatives[i], ":"))
    print("")
    for (h in 1:n_alt) {
      if (i != h){
        if ((positive_flows[i] > positive_flows[h]) && (negative_flows[i] < negative_flows[h])){
          print(paste(alternatives[i], " is preferable     to ", alternatives[h]))
        }
        else if ((positive_flows[i] == positive_flows[h]) && (negative_flows[i] < negative_flows[h])){
          print(paste(alternatives[i], " is preferable     to ", alternatives[h]))
        }
        else if ((positive_flows[i] > positive_flows[h]) && (negative_flows[i] == negative_flows[h])){
          print(paste(alternatives[i], " is preferable     to ", alternatives[h]))
        }
        else if ((positive_flows[i] == positive_flows[h]) && (negative_flows[i] == negative_flows[h])){
          print(paste(alternatives[i], " is indifferent to ", alternatives[h]))
        }
        else if ((positive_flows[i] < positive_flows[h]) && (negative_flows[i] < negative_flows[h])){
          print(paste(alternatives[i], " is incompatible   to ", alternatives[h]))
        }
        else if ((positive_flows[i] > positive_flows[h]) && (negative_flows[i] > negative_flows[h])){
          print(paste(alternatives[i], " is incompatible   to ", alternatives[h]))
        }
        else {
          print(paste(alternatives[i], " is not preferable to ", alternatives[h]))
        }
      }
    }
    print("")
  }


  print('')
  print("==================== PROMETHEE II ====================")
  ordering <- sort(net_flows, decreasing = TRUE)
  for(i in 1:n_alt){
    print(paste(alternatives[match(ordering[i],net_flows)],'=',ordering[i]))
  }


  print('')
  print("==================== PROMETHEE III ====================")
  print('')
  stand_error <- round((sd(net_flows)/sqrt(n_alt)), 3)
  stand_error
  x_limit <- c()
  y_limit <- c()
  for (i in 1:n_alt) {
    x <- round((net_flows[i] - stand_error), 3)
    y <- round((net_flows[i] + stand_error), 3)
    x_limit <- append(x_limit, x)
    y_limit <- append(y_limit, y)
  }
  for (i in 1:n_alt) {
    print(paste('Prefernce Relations of ', alternatives[i], ":"))
    print("")
    for (h in 1:n_alt) {
      if (i != h){
        if (x_limit[i] > y_limit[h]){
          print(paste(alternatives[i], " is preferable     to ", alternatives[h]))
        }
        else if ((x_limit[i] <= y_limit[h]) && (x_limit[h] <= y_limit[i])){
          print(paste(alternatives[i], " is indifferent    to ", alternatives[h]))
        }
        else {
          print(paste(alternatives[i], " is not preferable to ", alternatives[h]))
        }
      }
    }
    print("")
  }


  requireNamespace("ggplot2")
  requireNamespace("cowplot")
  coresAll <- c('blue', 'green', 'goldenrod', 'red', 'purple', 'chocolate', 'sienna',
                'gold', 'olivedrab', 'royalblue', 'mediumpurple', 'grey', 'maroon',
                'coral', 'yellowgreen', 'slategrey', 'darkviolet', 'pink',
                'springgreen', 'aqua', 'salmon', 'darkseagreen', 'steelblue', 'linen',
                'indigo', 'tomato', 'khaki', 'magenta', 'lightcoral', 'yellow','black')
  cores <- coresAll[1:n_alt]
  scale <- c()
  scale <- append(scale, negative_flows)
  scale <- append(scale, positive_flows)
  min <- (min(scale) - 0.1)
  max <- (max(scale) + 0.1)
  f_neg <- negative_flows
  f_pos <- positive_flows
  lista_fluxo_liquido <- net_flows
  flux_inf <- x_limit
  flux_sup <- y_limit
  alt <- alternatives
  df <- data.frame("y" = f_neg,"y_end"=f_pos, "x"=flux_inf, "x_end"=flux_sup, "liq"=lista_fluxo_liquido, "colors"=cores, "alt"=alt)
  partial = ggplot(df,aes(colour = alt))
  partial <- partial +  geom_segment(aes(x=1, y = min, xend=1, yend=max), color="black") +
    geom_segment(aes(x=2, y = min, xend=2, yend=max), color="black")
  for(i in 1:n_alt){
    partial <- partial + geom_segment(aes(x=1, y=f_neg, xend=2, yend=f_pos), size = 1) +
      geom_point(aes(x=1, y = f_neg),  size=1.8 ) + geom_point(aes (x=2, y = f_pos), size=1.8) +
      scale_colour_manual(values = cores)
  }
  partial <- partial +
    labs(color = "Alternatives") +
    ggtitle("PROMETHEE I") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  bot <- (min(ordering)-0.1)
  top <- (max(ordering)+0.1)
  total = ggplot(df,aes(colour = alt))
  total <- total +  geom_segment(aes(x=1, y = bot, xend=1, yend=top), color="black")
  for(i in 1:n_alt){
    total <- total +  geom_point(aes(x=1, y = lista_fluxo_liquido), size=2.2) +
      scale_colour_manual(values = cores)
  }
  total <- total +
    labs(color = "Alternatives") +
    ggtitle("PROMETHEE II") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  inf <- (min(flux_inf)-0.1)
  sup <- (max(flux_sup)+0.1)
  intervals = ggplot(df,aes(fill = alt))
  intervals <- intervals +  geom_segment(aes(x=inf, y = 1, xend=sup, yend=1), color="black")
  for(i in 1:n_alt){
    intervals <- intervals +  geom_point(aes(x=flux_inf, y = 1),shape=25,colour = "transparent", size =2.5) +
      geom_point(aes(x=flux_sup, y = 1),shape=24, colour = "transparent", size =2.5) +
      scale_fill_manual(values = cores)
  }
  intervals <- intervals +
    labs(fill = "Alternatives") +
    ggtitle("PROMETHEE III") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

  ggdraw() +
    draw_plot(partial, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(total, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(intervals, x = 0, y = 0, width = 1, height = 0.5)



}
