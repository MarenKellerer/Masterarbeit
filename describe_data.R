
source("functions.R")

# read data 
wave2 <- read.csv("./Data/data_w2.csv", header = TRUE, sep = ",", fill = TRUE,
                  fileEncoding = "UTF-8")

wave2_missings <- read.csv("./Data/wave2_missings.csv", header = TRUE, sep = ",",
                           fill = TRUE, fileEncoding = "UTF-8")

wave3 <- read.csv("./Data/data_w3.csv", header = TRUE, sep = ",", fill = TRUE, 
                  fileEncoding = "UTF-8")

data_all <- read.csv("./Data/data_all_clean.csv", header = TRUE, sep = ",",
                     fill = TRUE, fileEncoding = "UTF-8")

data_all_missings <- read.csv("./Data/data_all_missings.csv", header = TRUE, 
                              sep = ",", fill = TRUE, fileEncoding = "UTF-8")

# set theme for all of the plots
theme_set(theme_light() +
            theme(panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 14), 
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 10), 
                  text = element_text(family = "serif")))



# how many people are decided before cleaning data?
round(prop.table(table(wave2$ontic_bin)), 3) # 86.9% decided and 13.1% undecided
round(prop.table(table(wave3$ontic_bin)), 3) # 86.3% decided and 13.7% undecided


# 1. Describing missing values --------------------------------------------

missings_plot_wave2 <- missings_plot(wave2_missings, "Stichprobe")
missings_plot_all <- missings_plot(data_all_missings, "Gesamter Datensatz")

# combining the plots
comparison_missings <- missings_plot_all + 
  missings_plot_wave2 + plot_annotation(
  title =  'Anteil fehlender Werte (bis 50%)', 
  theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_missings.png",
       width = 11,
       height = 6,
       dpi = 1000)


# 2. Sample Generation ----------------------------------------------------

# remove na's resulting from ontic_bin from data_all
data_all <- na.omit(data_all) 

# A) Comparing Covariables "Geschlecht" and "Alter" in both data frames:
#    data_all and wave2

# i. Wave 2
plot_sex_all <- plotting_covariables(data_all, variable = "Geschlecht",
                                     title = "Gesamter Datensatz")
plot_sex_wave2 <- plotting_covariables(wave2, variable ="Geschlecht",
                                       title = "Stichprobe")

comparison_sex <- plot_sex_all + 
  plot_sex_wave2 + plot_annotation(
    title = 'Verteilung der Variable Geschlecht', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_sex.png",
       width = 11,
       height = 6,
       dpi = 1000)

# ii. whole data
plot_age_all <- plotting_covariables(data_all, variable = "Alter",
                                     title = "Gesamter Datensatz")
plot_age_wave2 <- plotting_covariables(wave2, variable ="Alter",
                                       title = "Stichprobe")

comparison_age <- plot_age_all + 
  plot_age_wave2 + plot_annotation(
    title = 'Verteilung der Variable Alter', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/comparison_age.png",
       width = 11,
       height = 6,
       dpi = 1000)


# B) Considering the variable weight (usind wave2)

# Distribution of the variable weight
distribution_weight_wave2 <- ggplot(wave2, aes(x = Gewicht)) +
  geom_histogram(bins = 150, position = "dodge", fill = "#0072B2") +
  scale_fill_manual(values = c("#0072B2")) +
  xlim(0, 4) +
  geom_vline(xintercept = 1, linetype = 'dashed', col = 'darkgrey') +
  labs(title = "Verteilung der Variable Gewicht",
       y = "Anzahl",
       x = "Gewicht") + 
  theme(plot.title = element_text(size = 16))

ggsave("./Plots/distribution_weight_wave2.png",
       width = 11,
       height = 6,
       dpi = 1000)


# Plotting variable age concluding the weights
distribution_age <- wave2 %>% 
  mutate(Gewicht = fct_rev(cut(Gewicht, c(0, 1, 3, 10)))) %>%
  ggplot(aes(x = factor(Alter), fill = Gewicht)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), colour = "white", size = 0.2) +
  scale_fill_manual(values = c("darkred", "#FE7773", "#FFC2C3")) +
  labs(title = "Verteilung Alter",
       y = "Anteil",
       x = "Alter",
       fill = "Gewicht")


# Getting the frequencies of the weight for each age group
wave2_weights <- wave2 %>% 
  group_by(Alter) %>% 
  summarise(Freq = round(sum(Gewicht)/nrow(wave2), 2))

# Plotting the weights instead of the count
distribution_age_weights <- 
  ggplot(wave2_weights, aes(x = Alter, y = Freq)) +
  geom_bar(stat = "identity", fill = "grey60") + #, alpha = 0.7
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.9), 
            size = 4, col = "black", family = "serif") +
  labs(title = "Anteil Gewicht pro Altersgruppe",
       y = "Anteil",
       x = "Alter",
       fill = "Gewicht")


age_including_weights <- distribution_age + 
  distribution_age_weights + plot_annotation(
    title = 'Berücksichtigung der Variable Gewicht', 
    theme = theme(plot.title = element_text(size = 16)))


ggsave("./Plots/age_including_weights.png",
       width = 11,
       height = 6,
       dpi = 1000)


# 3. Descriptive Analysis -------------------------------------------------

# read clean data 
wave2 <- read.csv("./Data/wave2_clean.csv", header = TRUE, sep = ",", fill = TRUE,
                  fileEncoding = "UTF-8")
wave3 <- read.csv("./Data/wave3_clean.csv", header = TRUE, sep = ",", fill = TRUE, 
                  fileEncoding = "UTF-8")

# how many people are decided after cleaning data?
round(prop.table(table(wave2$ontic_bin)), 3) # 93.1% decided and 6.9% undecided
round(prop.table(table(wave3$ontic_bin)), 3) # 93.7% decided and 6.3% undecided


# A) Plotting decided and undecided people without weights

# i. Wave 2
decided_wave2 <- plotting_decided(wave2)
undecided_wave2 <- plotting_undecided(wave2)

comparison_decided_undecided_wave2 <- decided_wave2 + 
  undecided_wave2 + plot_annotation(
    title = 'Verteilung der Parteien - 2. Welle', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_decided_undecided_wave2.png",
       width = 11,
       height = 6,
       dpi = 1000)


# ii. Wave 3
decided_wave3 <- plotting_decided(wave3)
undecided_wave3 <- plotting_undecided(wave3)

comparison_decided_undecided_wave3 <- decided_wave3 + 
  undecided_wave3 + plot_annotation(
    title = 'Verteilung der Parteien - 3. Welle', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_decided_undecided_wave3.png",
       width = 11,
       height = 6,
       dpi = 1000)



# B) Plotting decided and undecided people including weights

# i. Wave 2
decided_wave2_weights <- plotting_decided_weights(wave2)
undecided_wave2_weights <- plotting_undecided_weights(wave2)

comparison_decided_undecided_wave2_weights <- decided_wave2_weights + 
  undecided_wave2_weights + plot_annotation(
    title = 'Verteilung der Parteien ein Monat vor der Wahl', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_decided_undecided_wave2_weights.png",
       width = 11,
       height = 6,
       dpi = 1000)


# ii. Wave 3
decided_wave3_weights <- plotting_decided_weights(wave3)
undecided_wave3_weights <- plotting_undecided_weights(wave3)

comparison_decided_undecided_wave3_weights <- decided_wave3_weights + 
  undecided_wave3_weights + plot_annotation(
    title = 'Verteilung der Parteien zwei Wochen vor der Wahl', 
    theme = theme(plot.title = element_text(size = 16)))

ggsave("./Plots/comparison_decided_undecided_wave3_weights.png",
       width = 11,
       height = 6,
       dpi = 1000)




