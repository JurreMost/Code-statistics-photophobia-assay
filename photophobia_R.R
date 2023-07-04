##Loading in data and library
library("readxl")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/jurre/OneDrive/Documents/school/jaar 3/thesis")
data <- read_excel("photophobia.xlsx",sheet = 2)
data_control <- read_excel("photophobia.xlsx",sheet = 1)
data_redecap <- read_excel("photophobia.xlsx",sheet = 3)
data_transplantation <- read_excel("photophobia.xlsx",sheet = 4)

##Summary statistics of transplantation
results_transplantation <- data_transplantation %>%
  summarise(
    `Total Survival Rate` = sum(survived) / sum(total_transplantations),
    `Survival Rate SD` = sqrt(`Total Survival Rate` * (1 - `Total Survival Rate`) / sum(total_transplantations)),
    `Total Transplant Rate` = sum(tissue_transplant) / sum(survived),
    `Transplant Rate SD` = sqrt(`Total Transplant Rate` * (1 - `Total Transplant Rate`) / sum(survived)),
    `Total Transplant (Same Orientation) Rate` = sum(transplant_same_orientation) / sum(survived),
    `Transplant (Same Orientation) Rate SD` = sqrt(`Total Transplant (Same Orientation) Rate` * (1 - `Total Transplant (Same Orientation) Rate`) / sum(survived))
  ) %>%
  t() %>%
  as.data.frame()
# Print the dataframe
print(results_transplantation)


##Calculate variables
dark_region <- 25 #length of dark region (mm)
total_slide <- 60 #length of total slide(mm)
proportion_dark_region <- dark_region/total_slide

treatment_colors <- c("#7f5436", "#a3845d") 

# Calculate the propotion of contol
proportion_control <- data_control %>%
  summarize(
    proportion = sum(position == "dark") / n(),
    SD = sqrt(proportion * (1 - proportion) / n()),
    p_value = prop.test(x = sum(position == "dark"), n = n(), p = proportion_dark_region, alternative = "two.sided")$p.value,
    n = n())

#plot 
F_control <- ggplot(proportion_control, aes(x = "", y = proportion, fill = "Control")) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = proportion - SD, ymax = proportion + SD), width = 0.2, color = "black") +
  geom_hline(yintercept = proportion_dark_region, linetype = "dashed", color = "red") +
  geom_text(aes(x = 1, y =  proportion + SD,
                label = ifelse(p_value  < 0.05, "*", "")),
            size = 8, vjust = -0.05, color = "black", fontface = "bold") +
  labs(y = "Photophobia response (proportion planaria in dark region)", fill = "Treatment", x ="", title = "A") +
  scale_fill_manual(values = c("Control" = "grey")) +
  theme_minimal() +
  theme(legend.title = element_blank())

F_control

# Calculate statistics and perform one-sample proportion tests
proportion_treatment <- data %>%
  group_by(treatment, dt_decapitation) %>%
  summarize(
    proportion = sum(position == "dark") / n(),
    SD = sqrt(proportion * (1 - proportion) / n()),
    p_value = prop.test(x = sum(position == "dark"), n = n(), p = proportion_dark_region, alternative = "two.sided")$p.value,
    n = n())

## calculateion of the proportion in dark for the treatment groups
F_treatment <- ggplot(proportion_treatment, aes(x = dt_decapitation, y = proportion, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_errorbar(aes(ymin = proportion - SD, ymax = proportion + SD), position = position_dodge(0.5), width = 0.2, color = "black") +
  geom_hline(yintercept = proportion_dark_region, linetype = "dashed", color = "red") +
  labs(x = "Days after decapitation", fill = "Treatment", title = "B") +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +  # Set custom fill colors
  geom_text(aes(label = ifelse(p_value < 0.05, "*", ""), y = proportion + SD), 
            position = position_dodge(0.5),  # Adjust position for proper alignment
            size = 8, vjust = -0.05, color = "black", fontface = "bold") +
  geom_text(aes(label = n), position = position_dodge(0.5),
            size = 4, vjust = 1.5, color = "white") +
  scale_x_continuous(breaks = seq(min(proportion_treatment$dt_decapitation), max(proportion_treatment$dt_decapitation), by = 1)) +
  theme(axis.title.y = element_blank(), legend.title = element_blank())

F_treatment

# Calculate statistics for the redecap group
proportion_redecap <- data_redecap %>%
  summarize(
    proportion = sum(position == "dark") / n(),
    SD = sqrt(proportion * (1 - proportion) / n()),
    p_value = prop.test(x = sum(position == "dark"), n = n(), p = proportion_dark_region, alternative = "two.sided")$p.value,
    n = n())

# Create plot
F_redecap <- ggplot(proportion_redecap, aes(x = "", y = proportion, fill = "Redecapitation")) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = proportion - SD, ymax = proportion + SD), width = 0.2, color = "black") +
  geom_hline(yintercept = proportion_dark_region, linetype = "dashed", color = "red") +
  geom_text(aes(x = 1, y =  proportion + SD,
                label = ifelse(p_value  < 0.05, "*", "")),
            size = 8, vjust = -0.05, color = "black", fontface = "bold") +
  theme_minimal() +
  ylim(0, 1) +
  labs(fill = "Treatment", x ="", title="C") +
  scale_fill_manual(values = c("Redecapitation" = "darkred")) +
  theme(axis.title.y = element_blank(), legend.title = element_blank())

F_redecap

# Combine the plots and legends
figure <- ggarrange(F_control, F_treatment, F_redecap,
                    ncol = 3, legend = "bottom",
                    widths = c(1, 3, 1))
figure
