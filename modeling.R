
source("functions.R")

# read clean data 
wave2 <- read.csv("./Data/wave2_clean.csv", header = TRUE, sep = ",", fill = TRUE,
                  fileEncoding = "UTF-8")

wave3 <- read.csv("./Data/wave3_clean.csv", header = TRUE, sep = ",", fill = TRUE, 
                  fileEncoding = "UTF-8")


# set theme for all of the plots
theme_set(theme_light() +
            theme(panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 14), 
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 10), 
                  text = element_text(family = "serif")))

# 1. Random Forest =======================================================

# A) Stratified CV --------------------------------------------------------

# Long loading time

# i. Wave 2
# Accuracy for Cross validation and best mtry
accuracy_wave2 <- getting_accuracy(dataset = wave2,
                                   seed = 11561523,
                                   weight_type = "civey")

# Accuracy comparison - no weights, civey weights and simulation weights
accuracy_no_weights_wave2 <-
  getting_accuracy(dataset = wave2,
                   seed = 11561523,
                   weight_type = "none")[["accuracy"]]
accuracy_civey_wave2 <-
  getting_accuracy(dataset = wave2,
                   seed = 11561523,
                   weight_type = "civey")[["accuracy"]]
accuracy_simulation_wave2 <-
  getting_accuracy(dataset = wave2,
                   seed = 11561523,
                   weight_type = "simulation")[["mean_accuracy_sim"]]

comparison_accuracy_wave2 <-
  data.frame(
    no_weights = round(accuracy_no_weights_wave2, 3),
    civey_weights = round(accuracy_civey_wave2, 3),
    simulated_weights = round(accuracy_simulation_wave2, 3)
  )

png("./Plots/comparison_accuracy_wave2.png", height = 200, width = 500)
accuracy_table_wave2 <- tableGrob(comparison_accuracy_wave2)
grid.arrange(accuracy_table_wave2)
dev.off()



# ii. Wave 3

# Accuracy for Cross validation and best mtry
accuracy_wave3 <- getting_accuracy(dataset = wave3,
                                   seed = 11561523,
                                   weight_type = "civey")

# Accuracy comparison - no weights, civey weights and simulation weights
accuracy_no_weights_wave3 <-
  getting_accuracy(dataset = wave3,
                   seed = 11561523,
                   weight_type = "none")[["accuracy"]]
accuracy_civey_wave3 <-
  getting_accuracy(dataset = wave3,
                   seed = 11561523,
                   weight_type = "civey")[["accuracy"]]
accuracy_simulation_wave3 <-
  getting_accuracy(dataset = wave3,
                   seed = 11561523,
                   weight_type = "simulation")[["mean_accuracy_sim"]]

comparison_accuracy_wave3 <-
  data.frame(
    no_weights = round(accuracy_no_weights_wave3, 3),
    civey_weights = round(accuracy_civey_wave3, 3),
    simulated_weights = round(accuracy_simulation_wave3, 3)
  )

png("./Plots/comparison_accuracy_wave3.png", height = 200, width = 500)
accuracy_table_wave3 <- tableGrob(comparison_accuracy_wave3)
grid.arrange(accuracy_table_wave3)
dev.off()


# Plotting one example of a weight distribution
sim_weight_wave2 <- plotting_simulation_example(wave2, 
                    "Verteilung einer simulierten Gewichtungsvariable")

ggsave("./Plots/simulated_weights_wave2.png",
       width = 11,
       height = 6,
       dpi = 1000)


# B) Prediction -----------------------------------------------------------

# i. Wave 2
# ggplot with predicted frequencies for each party with best mtry
rf_predictions_wave2 <- predicting_undecided(dataset = wave2, 
                                             weight_type = "civey",
                                             title = "Ein Monat vor der Wahl", 
                                             best_mtry = 3)


# ii. Wave 3
# ggplot with predicted frequencies for each party with best mtry
rf_predictions_wave3 <- predicting_undecided(dataset = wave3, 
                                             weight_type = "civey",
                                             title = "Zwei Wochen vor der Wahl", 
                                             best_mtry = 2)

comparison_rf <- rf_predictions_wave2 + 
  rf_predictions_wave3 + plot_annotation(
    title = 'Homogenitätsannahme', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/comparison_rf.png",
       width = 11,
       height = 6,
       dpi = 1000)

# 2. Dempster Bounds ======================================================

# A) without restrictions -------------------------------------------------

# i. Wave 2

# get data frame of weighted dempster bounds for all parties
dempster_bounds_wave2 <- combining_bounds(wave2, demp_type = "weighted_demp")

# plotting the results
dempster_plot_wave2 <- ggplot(dempster_bounds_wave2, 
                              aes(x = reorder(party, lower_bound), y = middle)) + 
  geom_errorbar(aes(ymin = lower_bound, 
                    ymax = upper_bound), width = 0.3,
                size = 1.3,
                col = c("black", "gold", "firebrick3",
                        "mediumvioletred", "green4", "blue3")) +  
  scale_x_discrete(labels = c("CDU.CSU" = "CDU/CSU"))  + 
  ylim(c(0, 35)) +
  coord_flip() + 
  labs(title = "Ein Monat vor der Wahl",
       y = "Anteil",
       x = "Partei") 


# ii. wave 3
# get data frame of weighted dempster bounds for all parties
dempster_bounds_wave3 <- combining_bounds(wave3, demp_type = "weighted_demp")

# plotting the results
dempster_plot_wave3 <- ggplot(dempster_bounds_wave3, 
                              aes(x = reorder(party, lower_bound), y = middle)) + 
  geom_errorbar(aes(ymin = lower_bound, 
                    ymax = upper_bound), width = 0.3,
                size = 1.3,
                col = c("black", "gold", "firebrick3",
                        "mediumvioletred", "green4", "blue3")) +  
  scale_x_discrete(labels = c("CDU.CSU" = "CDU/CSU"))  + 
  ylim(c(0, 35)) +
  coord_flip() + 
  labs(title = "Zwei Wochen vor der Wahl",
       y = "Anteil",
       x = "Partei") 

comparison_dempster <- dempster_plot_wave2 + 
  dempster_plot_wave3 + plot_annotation(
    title = 'Dempster Bounds', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/comparison_dempster.png",
       width = 11,
       height = 6,
       dpi = 1000)




# B) 80/20 restriction ----------------------------------------------------


# i. Wave 2
# get data frame of restricted dempster bounds for all parties
res_dempster_bounds_wave2 <- combining_bounds(wave2, demp_type = "res_demp")

# plotting the result for wave 2 with restrictions
res_dempster_plot_wave2 <- ggplot(res_dempster_bounds_wave2, 
                                  aes(x = reorder(party, lower_bound),
                                      y = middle)) + 
  geom_errorbar(aes(ymin = lower_bound, 
                    ymax = upper_bound), width = 0.3,
                size = 1.1,
                col = c("black", "gold", "firebrick3",
                        "mediumvioletred", "green4", "blue3")) + 
  geom_point(position = position_dodge(width = 0.8), size = 2) + 
  geom_errorbar(aes(ymin = dempster_bounds_wave2$lower_bound, 
                    ymax = dempster_bounds_wave2$upper_bound), width = 0.3, 
                alpha = 0.4,
                size = 0.8,
                col = c("black", "gold", "firebrick3",
                        "mediumvioletred", "green4", "blue3"))  + 
  scale_x_discrete(labels = c("CDU.CSU" = "CDU/CSU"))  + 
  ylim(c(0, 35)) +
  coord_flip() + 
  labs(title = "Ein Monat vor der Wahl",
       y = "Anteil",
       x = "Partei") 


# ii. Wave 2
# get data frame of restricted dempster bounds for all parties
res_dempster_bounds_wave3 <- combining_bounds(wave3, demp_type = "res_demp")

# plotting the result for wave 3 with restrictions
res_dempster_plot_wave3 <- ggplot(res_dempster_bounds_wave3, 
                                  aes(x = reorder(party, lower_bound),
                                      y = middle)) + 
  geom_errorbar(aes(ymin = lower_bound, 
                    ymax = upper_bound), width = 0.3,
                size = 1.1,
                col = c("black", "gold", "firebrick3",
                        "mediumvioletred", "green4", "blue3")) + 
  geom_point(position = position_dodge(width = 0.8), size = 2) + 
  geom_errorbar(aes(ymin = dempster_bounds_wave3$lower_bound, 
                    ymax = dempster_bounds_wave3$upper_bound), width = 0.3, 
                alpha = 0.4,
                size = 0.8,
                col = c("black", "gold", "firebrick3",
                        "mediumvioletred", "green4", "blue3")) +
  scale_x_discrete(labels = c("CDU.CSU" = "CDU/CSU"))  + 
  ylim(c(0, 35)) +
  coord_flip() + 
  labs(title = "Zwei Wochen vor der Wahl",
       y = "Anteil",
       x = "Partei") 


comparison_res_dempster <- res_dempster_plot_wave2 + 
  res_dempster_plot_wave2 + plot_annotation(
    title = 'Eingeschränkte Dempster Bounds', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/comparison_res_dempster.png",
       width = 11,
       height = 6,
       dpi = 1000)


# c) Coalitions ---------------------------------------------------------

# i. Wave 2
# 6 possible coalitions 
# choices of undecided people that are present in both waves
possible_coalitions <- list(c("CDU.CSU", "FDP"), c("SPD", "Grüne"), 
                            c("SPD", "Grüne", "Linke"),
                            c("CDU.CSU", "FDP", "AfD"), 
                            c("CDU.CSU", "SPD"),  c("FDP", "AfD"))


# combine all coalitions for wave 2
dempster_coalitions_wave2 <- rbind(
  get_dempster_bounds(wave2, type = "coalitions", coalition = possible_coalitions[[1]]),
  get_dempster_bounds(wave2, type = "coalitions", coalition = possible_coalitions[[2]]),
  get_dempster_bounds(wave2, type = "coalitions", coalition = possible_coalitions[[3]]),
  get_dempster_bounds(wave2, type = "coalitions", coalition = possible_coalitions[[4]]),
  get_dempster_bounds(wave2, type = "coalitions", coalition = possible_coalitions[[5]]),
  get_dempster_bounds(wave2, type = "coalitions", coalition = possible_coalitions[[6]])
)

# get middle point plotting
for(i in 1:nrow(dempster_coalitions_wave2)){
  dempster_coalitions_wave2[["middle"]][i] <- 
    mean(c(dempster_coalitions_wave2$lower_bound[i],
           dempster_coalitions_wave2$upper_bound[i]))
}

# plotting the result for wave 2
dempster_coalitions_plot_wave2 <- ggplot(dempster_coalitions_wave2, 
                                         aes(x = reorder(coalition, lower_bound),
                                             y = middle)) + 
  geom_errorbar(aes(ymin = lower_bound, 
                    ymax = upper_bound), width = 0.3,
                size = 1.1,
                col = brewer.pal(n = 6, name = 'Paired')) + 
  geom_hline(yintercept = 50, linetype = 'dashed', col = 'darkgrey') +
  scale_x_discrete(labels = c("CDU.CSU, SPD" = "CDU/CSU, SPD",
                              "CDU.CSU, FDP, AfD" = "CDU/CSU, FDP, AfD",
                              "CDU.CSU, FDP" = "CDU/CSU, FDP")) +
  ylim(c(0, 55)) +
  coord_flip() + 
  labs(title = "Ein Monat vor der Wahl",
       y = "Anteil",
       x = "Partitionierung")


# ii. Wave 3
# combine all coalitions for wave 3
dempster_coalitions_wave3 <- rbind(
  get_dempster_bounds(wave3, type = "coalitions", coalition = possible_coalitions[[1]]),
  get_dempster_bounds(wave3, type = "coalitions", coalition = possible_coalitions[[2]]),
  get_dempster_bounds(wave3, type = "coalitions", coalition = possible_coalitions[[3]]),
  get_dempster_bounds(wave3, type = "coalitions", coalition = possible_coalitions[[4]]),
  get_dempster_bounds(wave3, type = "coalitions", coalition = possible_coalitions[[5]]),
  get_dempster_bounds(wave3, type = "coalitions", coalition = possible_coalitions[[6]])
)

# get middle point for plotting
for(i in 1:nrow(dempster_coalitions_wave3)){
  dempster_coalitions_wave3[["middle"]][i] <- 
    mean(c(dempster_coalitions_wave3$lower_bound[i],
           dempster_coalitions_wave3$upper_bound[i]))
}

# plotting the result for wave 3
dempster_coalitions_plot_wave3 <- ggplot(dempster_coalitions_wave3, 
                                         aes(x = reorder(coalition, lower_bound),
                                             y = middle)) + 
  geom_errorbar(aes(ymin = lower_bound, 
                    ymax = upper_bound), width = 0.3,
                size = 1.1,
                col = brewer.pal(n = 6, name = 'Paired')) + 
  geom_hline(yintercept = 50, linetype = 'dashed', col = 'darkgrey') +
  scale_x_discrete(labels = c("CDU.CSU, SPD" = "CDU/CSU, SPD",
                              "CDU.CSU, FDP, AfD" = "CDU/CSU, FDP, AfD",
                              "CDU.CSU, FDP" = "CDU/CSU, FDP")) +
  ylim(c(0, 55)) +
  coord_flip() + 
  labs(title = "Zwei Wochen vor der Wahl",
       y = "Anteil",
       x = "Partitionierung")


comparison_coalitions <- dempster_coalitions_plot_wave2 + 
  dempster_coalitions_plot_wave3 + plot_annotation(
    title = 'Dempster Bounds mit möglichen Partitionierungen', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_coalitions.png",
       width = 11,
       height = 6,
       dpi = 1000)


# 4. Comparison ===========================================================


# i. Wave 2
decided_weights_wave2 <- plotting_decided_weights(wave2)

# ii. Wave 3
decided_weights_wave3 <- plotting_decided_weights(wave3)


# i. Wave 2
forced_decision_wave2 <- plotting_forced_decision(wave2)
# ii. Wave 3
forced_decision_wave3 <- plotting_forced_decision(wave3)


# Put all together: random forest, only decided and Sonntagsfrage by everyone
# i. Wave 2
rf_predictions_wave2 <- predicting_undecided(dataset = wave2, 
                                             weight_type = "civey",
                                             title = "Homogenitätsannahme", 
                                             best_mtry = 3)
# ii. Wave 3
rf_predictions_wave3 <- predicting_undecided(dataset = wave3, 
                                             weight_type = "civey",
                                             title = "Homogenitätsannahme", 
                                             best_mtry = 2)


# Get all plots next to each other
comparison_total <- ggarrange(decided_weights_wave2, decided_weights_wave3, 
                              rf_predictions_wave2, rf_predictions_wave3,
                              forced_decision_wave2, forced_decision_wave3
                              + rremove("x.text"), common.legend = TRUE,
                              ncol = 2, nrow = 3)

annotate_figure(comparison_total, 
                top = text_grob("Vergleich der punktwertigen Prognosen", size = 16,
                                family = "serif"))


ggsave("./Plots/comparison_total.png",
       width = 11,
       height = 6,
       dpi = 1000)


