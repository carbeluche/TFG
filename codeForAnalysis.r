install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("patchwork")
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(cowplot)
library(patchwork)

# Initial Step. Convert Excel Table into a Data Frame
ruta_excel <- "/Users/carbeluche/Desktop/TFG/DataForAnalysis.xlsx"
df <- read_excel(ruta_excel)
head(df) # si funciona
data <- df
data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
palette_sus <- c("Motor" = "#7a1fa2",     # Motor is always the strongest color
                 "Visual" = "#b566d2",    # Visual is the medium tone
                 "Auditory" = "#e3b5f5")  # Auditory corresponds to the lighter
palette_time <- c("Motor" = "#ed588e",    
                  "Visual" = "#f0adc5",   
                  "Auditory" = "#fce6ee") 
palette_likert <- c("Motor" = "#0d47a1",  
                    "Visual" = "#5e92f3",
                    "Auditory" = "#b3d1ff")
palette_gender <- c("MALE" = "#f5945f",    
                    "FEMALE" = "#f5cfba")  
palette_assist <- c("NO" = "#1b5e20",      
                    "YES" = "#a5d6a7")   
custom_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

# Step 1. Visual Exploration 
# 1.1. Boxplots for SUS_Score, Task_Completion_Time, and Likert_Scale
box_sus <- ggplot(data, aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette_sus) +
  labs(title = "SUS by Disability", x = "Disability Type", y = "SUS Score") +
  custom_theme
box_time <- ggplot(data, aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette_time) +
  labs(title = "Time by Disability", x = "Disability Type", y = "Seconds") +
  custom_theme
box_likert <- ggplot(data, aes(x = DisabilityType, y = Likert_Scale, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette_likert) +
  labs(title = "Likert by Disability", x = "Disability Type", y = "Likert Score") +
  custom_theme
summary_data <- data %>%
  group_by(DisabilityType) %>%
  summarise(
    Mean_SUS = mean(SUS_Score, na.rm = TRUE),
    SD_SUS = sd(SUS_Score, na.rm = TRUE),
    Mean_Likert = mean(Likert_Scale, na.rm = TRUE),
    SD_Likert = sd(Likert_Scale, na.rm = TRUE),
    Mean_Time = mean(Task_Completion_Time, na.rm = TRUE),
    SD_Time = sd(Task_Completion_Time, na.rm = TRUE)
  )
# 1.2. Bar Chats for Mean_SUS, Mean_Time and Mean_Likert
bar_sus <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_SUS, fill = DisabilityType)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_SUS - SD_SUS, ymax = Mean_SUS + SD_SUS), width = 0.2) +
  scale_fill_manual(values = palette_sus) +
  labs(title = "Mean SUS ± SD", y = "Mean SUS", x = "Disability Type") +
  custom_theme
bar_time <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_Time, fill = DisabilityType)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Time - SD_Time, ymax = Mean_Time + SD_Time), width = 0.2) +
  scale_fill_manual(values = palette_time) +
  labs(title = "Mean Time ± SD", y = "Mean Time (s)", x = "Disability Type") +
  custom_theme
bar_likert <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_Likert, fill = DisabilityType)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Likert - SD_Likert, ymax = Mean_Likert + SD_Likert), width = 0.2) +
  scale_fill_manual(values = palette_likert) +
  labs(title = "Mean Likert ± SD", y = "Mean Likert", x = "Disability Type") +
  custom_theme
combined_figure <- (
  (box_sus | box_time | box_likert) /
  (bar_sus | bar_time | bar_likert)
) + 
  plot_layout(nrow = 2) 
quartz(width = 16, height = 10)  
print(combined_figure)
# 1.3. Additional plots for Gender | Assistance_Needed by DisabilityType and Gender vs Assistance
bar_gender <- ggplot(data, aes(x = DisabilityType, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = palette_gender) +
  labs(title = "Gender by Disability", x = "Disability Type", y = "Count", fill = "Gender") +
  custom_theme
bar_assist <- ggplot(data, aes(x = DisabilityType, fill = AssistanceNeeded)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = palette_assist) +
  labs(title = "Assistance by Disability", x = "Disability Type", y = "Count", fill = "Assistance") +
  custom_theme
bar_gender_assist <- ggplot(data, aes(x = AssistanceNeeded, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = palette_gender) +
  labs(title = "Gender by Assistance", x = "Assistance Needed", y = "Count", fill = "Gender") +
  custom_theme
participant_distribution_plot <- (bar_gender | bar_assist | bar_gender_assist) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 13),
    legend.spacing.x = unit(1.2, "cm")
  )
quartz(width = 8, height = 4)
print(participant_distribution_plot)

# Step 2. Inferential Analysis
# 2.1. Model Assumptions
install.packages("performance")
library(performance)
check_model(lm(SUS_Score ~ DisabilityType * Gender, data = data))
check_model(lm(Task_Completion_Time ~ DisabilityType * Gender, data = data))
check_model(lm(Likert_Scale ~ DisabilityType * Gender, data = data))
theme_large <- theme_minimal(base_size = 18) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.margin = margin(10, 10, 10, 10)
  )
plot_qq_sus <- ggplot(data, aes(sample = SUS_Score)) +
  stat_qq() + stat_qq_line() +
  labs(
    title = "SUS_Score",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) + theme_large
plot_qq_likert <- ggplot(data, aes(sample = Likert_Scale)) +
  stat_qq() + stat_qq_line() +
  labs(
    title = "Likert_Scale",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) + theme_large

plot_qq_time <- ggplot(data, aes(sample = Task_Completion_Time)) +
  stat_qq() + stat_qq_line() +
  labs(
    title = "Task_Completion_Time",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) + theme_large
# Consideration: since the density plots are not grouped by Disability, I just picked the Motor colors
plot_density_sus <- ggplot(data, aes(x = SUS_Score)) +
  geom_density(fill = "#7a1fa2", alpha = 0.6) +  
  labs(title = "SUS_Score") + theme_large
plot_density_likert <- ggplot(data, aes(x = Likert_Scale)) +
  geom_density(fill = "#0d47a1", alpha = 0.6) +  
  labs(title = "Likert_Scale") + theme_large
plot_density_time <- ggplot(data, aes(x = Task_Completion_Time)) +
  geom_density(fill = "#ed588e", alpha = 0.6) +  
  labs(title = "Task_Completion_Time") + theme_large
figure_normality <- (plot_qq_sus | plot_density_sus) /
                    (plot_qq_likert | plot_density_likert) /
                    (plot_qq_time | plot_density_time) +
  plot_annotation(title = "Q-Q plots  |  Density histograms", theme = theme_large)
print(figure_normality)
# 2.2. ANOVA Results for SUS_Score
install.packages("afex")
library(afex)
anova_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", between = "Gender", data = data)
anova_likert <- aov_ez(id = "Student", dv = "Likert_Scale", within = "DisabilityType", between = "Gender", data = data)
anova_time <- aov_ez(id = "Student", dv = "Task_Completion_Time", within = "DisabilityType", between = "Gender", data = data)
print(summary(anova_sus))
print("------------")
print(summary(anova_likert))
print("------------")
print(summary(anova_time))
print("------------")
print("------------")
# 2.2.1. Effect Size: Partial eta squared for SUS_Score
install.packages("effectsize")
library(effectsize)
eta_sus <- eta_squared(anova_sus, partial = TRUE)
print("Partial eta squared for SUS:")
print(eta_sus)
print("------------")
print("------------")
# 2.2.2. Quantitative Comparisons and Interaction Analysis
data_female <- subset(data, Gender == "FEMALE")
data_male <- subset(data, Gender == "MALE")
aov_female_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", data = data_female)
aov_male_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", data = data_male)
print(summary(aov_female_sus))
print("------------")
print(summary(aov_male_sus))
print("------------")
print("------------")
# 2.2.2.1. Post-hoc pairwise comparisons for male participants only
install.packages("emmeans")
library(emmeans)
emmeans_male <- emmeans(aov_male_sus, pairwise ~ DisabilityType, adjust = "tukey") 
print(emmeans_male$contrasts)
print("------------")
print("------------")
# 2.2.2.2. Effsize::cohen.d() with paired=TRUE
install.packages("effsize")
install.packages("tidyr")
library(effsize)
library(tidyr)
data_male_cohen <- subset(data, Gender == "MALE") %>%
  select(Student, DisabilityType, SUS_Score)
data_male_wide <- pivot_wider(data_male_cohen, names_from = DisabilityType, values_from = SUS_Score)
d_mv <- cohen.d(data_male_wide$Motor, data_male_wide$Visual, paired = TRUE)
print(d_mv)
d_ma <- cohen.d(data_male_wide$Motor, data_male_wide$Auditory, paired = TRUE)
print(d_ma)
d_va <- cohen.d(data_male_wide$Visual, data_male_wide$Auditory, paired = TRUE)
print(d_va)
print("------------")
print("------------")
# 2.2.2.3. Interaction Plot: SUS Score by Disability Type and Gender
interaction_plot_sus <- ggplot(data, aes(x = DisabilityType, y = SUS_Score, group = Gender, color = Gender)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_color_manual(values = palette_gender) +
  labs(
    x = "DisabilityType",
    y = "Mean SUS_Score",
    color = "Gender"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
print(interaction_plot_sus)
# 2.3. ANOVA Results for Task_Completion_Time just for DisabilityType
emmeans_time <- emmeans(anova_time, pairwise ~ DisabilityType, adjust = "tukey")
print(emmeans_time$contrasts)
print("------------")
install.packages("performance")
library(performance)
eta_time <- eta_squared(anova_time, partial = TRUE)
print("Partial eta squared for Task Completion Time:")
print(eta_time)
print("------------")
print("------------")
# 2.4. Pearson Correlations: SUS_Score & Task_Time | Count, Likert_Scale & Task_Time | Count
cor_sus_time <- cor.test(data$SUS_Score, data$Task_Completion_Time, method = "pearson")
cor_sus_errors <- cor.test(data$SUS_Score, data$Errors_Count, method = "pearson")
cor_likert_time <- cor.test(data$Likert_Scale, data$Task_Completion_Time, method = "pearson")
cor_likert_errors <- cor.test(data$Likert_Scale, data$Errors_Count, method = "pearson")
print("Correlation between SUS Score and Task Completion Time:")
print(cor_sus_time)
print("------------")
print("Correlation between SUS Score and Errors Count:")
print(cor_sus_errors)
print("------------")
print("Correlation between Likert Scale and Task Completion Time:")
print(cor_likert_time)
print("------------")
print("Correlation between Likert Scale and Errors Count:")
print(cor_likert_errors)
print("------------")
# 2.4.1. Additional Scatterplots
theme_scatter <- custom_theme
plot1 <- ggplot(data, aes(x = SUS_Score, y = Task_Completion_Time, color = DisabilityType)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = palette_sus) +
  labs(
    title = "SUS_Score vs Time",
    x = "SUS_Score",
    y = "Seconds",
    color = "Disability Type"
  ) +
  theme_scatter
plot2 <- ggplot(data, aes(x = SUS_Score, y = Errors_Count, color = DisabilityType)) +
  geom_jitter(width = 0.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = palette_sus) +
  labs(
    title = "SUS_Score vs Count",
    x = "SUS_Score",
    y = "Errors",
    color = "Disability Type"
  ) +
  theme_scatter
plot3 <- ggplot(data, aes(x = Likert_Scale, y = Task_Completion_Time, color = DisabilityType)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = palette_likert) +
  labs(
    title = "Likert_Scale vs Time",
    x = "Satisfaction",
    y = "Seconds",
    color = "Disability Type"
  ) +
  theme_scatter
plot4 <- ggplot(data, aes(x = Likert_Scale, y = Errors_Count, color = DisabilityType)) +
  geom_jitter(width = 0.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = palette_likert) +
  labs(
    title = "Likert_Scale vs Count",
    x = "Satisfaction",
    y = "Errors",
    color = "Disability Type"
  ) +
  theme_scatter
figure_scatter <- (plot1 | plot2) / (plot3 | plot4)
print(figure_scatter)
# 2.5. Contingency Table of Assistance_Needed by DisabilityType
table_con <- table(data$DisabilityType, data$AssistanceNeeded)
print("Contingency Table: Assistance Needed per Disability Type")
print(table_con)
expected_counts <- chisq.test(table_con)$expected
print("Expected Counts:")
print(expected_counts)
if (any(expected_counts < 5)) {
  result <- fisher.test(table_con)
  print("Fisher's Exact Test Result:")
  print(result)
} else {
  result <- chisq.test(table_con)
  print("Chi-Squared Test Result:")
  print(result)
}
