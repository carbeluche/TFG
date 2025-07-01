library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(patchwork)
library(performance)
library(afex)
library(effectsize)
library(emmeans)
library(effsize)
library(tidyr)

ui <- fluidPage(
  titlePanel("Dashboard for Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File", accept = ".xlsx"),
      actionButton("analyze", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Boxplots & Means", plotOutput("combinedFigure")),
        tabPanel("Participant Distribution", plotOutput("distributionPlot")),
        tabPanel("Normality Check", plotOutput("qqDensity")),
        tabPanel("Interaction Plot", plotOutput("interactionPlot")),
        tabPanel("Scatterplots", plotOutput("scatterPlot")),
        tabPanel("Contingency Table", verbatimTextOutput("contingencyResults"))
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$analyze, {
    req(input$file)
    data <- read_excel(input$file$datapath)
    data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
    palette_sus <- c("Motor" = "#7a1fa2", "Visual" = "#b566d2", "Auditory" = "#e3b5f5")
    palette_time <- c("Motor" = "#ed588e", "Visual" = "#f0adc5", "Auditory" = "#fce6ee")
    palette_likert <- c("Motor" = "#0d47a1", "Visual" = "#5e92f3", "Auditory" = "#b3d1ff")
    palette_gender <- c("MALE" = "#f5945f", "FEMALE" = "#f5cfba")
    palette_assist <- c("NO" = "#1b5e20", "YES" = "#a5d6a7")
    custom_theme <- theme_minimal(base_size = 16) + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"), plot.title = element_text(size = 18, face = "bold", hjust = 0.5), legend.position = "bottom")
    summary_data <- data %>% group_by(DisabilityType) %>% summarise(Mean_SUS = mean(SUS_Score, na.rm = TRUE), SD_SUS = sd(SUS_Score, na.rm = TRUE), Mean_Likert = mean(Likert_Scale, na.rm = TRUE), SD_Likert = sd(Likert_Scale, na.rm = TRUE), Mean_Time = mean(Task_Completion_Time, na.rm = TRUE), SD_Time = sd(Task_Completion_Time, na.rm = TRUE))
    box_sus <- ggplot(data, aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) + geom_boxplot() + scale_fill_manual(values = palette_sus) + labs(title = "SUS by Disability") + custom_theme
    box_time <- ggplot(data, aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) + geom_boxplot() + scale_fill_manual(values = palette_time) + labs(title = "Time by Disability") + custom_theme
    box_likert <- ggplot(data, aes(x = DisabilityType, y = Likert_Scale, fill = DisabilityType)) + geom_boxplot() + scale_fill_manual(values = palette_likert) + labs(title = "Likert by Disability") + custom_theme
    bar_sus <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_SUS, fill = DisabilityType)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = Mean_SUS - SD_SUS, ymax = Mean_SUS + SD_SUS), width = 0.2) + scale_fill_manual(values = palette_sus) + labs(title = "Mean SUS ± SD") + custom_theme
    bar_time <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_Time, fill = DisabilityType)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = Mean_Time - SD_Time, ymax = Mean_Time + SD_Time), width = 0.2) + scale_fill_manual(values = palette_time) + labs(title = "Mean Time ± SD") + custom_theme
    bar_likert <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_Likert, fill = DisabilityType)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = Mean_Likert - SD_Likert, ymax = Mean_Likert + SD_Likert), width = 0.2) + scale_fill_manual(values = palette_likert) + labs(title = "Mean Likert ± SD") + custom_theme
    combined_figure <- (box_sus | box_time | box_likert) / (bar_sus | bar_time | bar_likert)
    output$combinedFigure <- renderPlot(combined_figure)
    bar_gender <- ggplot(data, aes(x = DisabilityType, fill = Gender)) + geom_bar(position = "dodge") + scale_fill_manual(values = palette_gender) + labs(title = "Gender by Disability") + custom_theme
    bar_assist <- ggplot(data, aes(x = DisabilityType, fill = AssistanceNeeded)) + geom_bar(position = "dodge") + scale_fill_manual(values = palette_assist) + labs(title = "Assistance by Disability") + custom_theme
    bar_gender_assist <- ggplot(data, aes(x = AssistanceNeeded, fill = Gender)) + geom_bar(position = "dodge") + scale_fill_manual(values = palette_gender) + labs(title = "Gender by Assistance") + custom_theme
    dist_plot <- (bar_gender | bar_assist | bar_gender_assist)
    output$distributionPlot <- renderPlot(dist_plot)
    output$qqDensity <- renderPlot({
      p1 <- ggplot(data, aes(sample = SUS_Score)) + stat_qq() + stat_qq_line() + labs(title = "Q-Q SUS") + custom_theme
      p2 <- ggplot(data, aes(x = SUS_Score)) + geom_density(fill = "#7a1fa2", alpha = 0.6) + labs(title = "Density SUS") + custom_theme
      p3 <- ggplot(data, aes(sample = Likert_Scale)) + stat_qq() + stat_qq_line() + labs(title = "Q-Q Likert") + custom_theme
      p4 <- ggplot(data, aes(x = Likert_Scale)) + geom_density(fill = "#0d47a1", alpha = 0.6) + labs(title = "Density Likert") + custom_theme
      (p1 | p2) / (p3 | p4)
    })
    lm_sus <- lm(SUS_Score ~ DisabilityType * Gender, data = data)
    resid_plot <- function(model, title, color = "#2c3e50") {
      df <- data.frame(Fitted = fitted(model), Residuals = resid(model))
      ggplot(df, aes(x = Fitted, y = Residuals)) + geom_point(color = color, alpha = 0.6) + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + labs(title = title) + custom_theme
    }
        output$interactionPlot <- renderPlot({
      ggplot(data, aes(x = DisabilityType, y = SUS_Score, group = Gender, color = Gender)) +
        stat_summary(fun = mean, geom = "line", size = 1.2) +
        stat_summary(fun = mean, geom = "point", size = 3) +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
        scale_color_manual(values = palette_gender) + labs(title = "Interaction Plot") + custom_theme
    })
    output$scatterPlot <- renderPlot({
      ggplot(data, aes(x = SUS_Score, y = Task_Completion_Time, color = DisabilityType)) +
        geom_point() + geom_smooth(method = "lm", se = TRUE, color = "black") + scale_color_manual(values = palette_sus) + labs(title = "SUS vs Time") + custom_theme
    })
    output$contingencyResults <- renderPrint({
      table_con <- table(data$DisabilityType, data$AssistanceNeeded)
      print("Contingency Table:")
      print(table_con)
      expected_counts <- chisq.test(table_con)$expected
      if (any(expected_counts < 5)) {
        fisher.test(table_con)
      } else {
        chisq.test(table_con)
      }
    })
  })
}
shinyApp(ui = ui, server = server)
