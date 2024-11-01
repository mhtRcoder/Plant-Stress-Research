#yield plotting for my thesis
# Load necessary libraries
library(tidyverse)
library(ggpubr)
library(agricolae)
library(rstatix)
getwd()
df <- read.csv(file.choose(),header = TRUE)
df
Test <- df
Test
str(Test)
# Assuming you have the data loaded as `Test`
# Convert ID and time to factors if necessary
Test <- Test %>%
  convert_as_factor(ID, Gen, GC)

# Perform two-way ANOVA for Grain Weight
anova_res <- anova_test(
  data = Test, dv ="Grain Wt", wid = ID,
  within = "time", between = c("Gen", "GC")
)

# Get ANOVA table for inspection
anova_table <- get_anova_table(anova_res)
print(anova_table)

# Pairwise comparisons with p-value adjustment (Bonferroni)
pwc <- Test %>%
  group_by(Gen, time) %>%
  pairwise_t_test(`Grain Wt` ~ GC, paired = TRUE, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic)

# Adding the positions of p-values on the graph
pwc <- pwc %>% add_xy_position(x = "time")

# Bar graph for Grain Weight with mean and SD
bar_plot <- ggbarplot(
  Test, x = "time", y = "Grain Wt", 
  color = "black", fill = "GC", add = "mean_sd", 
  position = position_dodge(0.8),
  facet.by = "Gen", error.plot = "errorbar"
) +
  labs(x = "Time", y = "Grain Weight (Mean ± SD)") +
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(anova_res, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Show the plot
print(bar_plot)

df <- read.csv(file.choose(),header = TRUE)
df
Test <- df
Test
str(Test)


# Load necessary libraries
library(tidyverse)
library(ggpubr)
library(rstatix)

# Assuming 'Test' is your dataset
Test <- Test %>%
  rename(Gwt = Gwt, Agb = Agb, Swt = Swt, Lwt = Lwt, Pwt = Pwt, PH = PH) %>%
  convert_as_factor(Gen, GC)

# Perform pairwise t-tests for each variable
pairwise_test <- function(variable) {
  pwc <- Test %>%
    pairwise_t_test(as.formula(paste(variable, "~ GC")), paired = FALSE, p.adjust.method = "bonferroni")
  pwc <- pwc %>% add_xy_position(x = "Gen")
  return(pwc)
}

# List of parameters
params <- c("Gwt", "Agb", "Swt", "Lwt", "Pwt", "PH")

# Create the plot
create_plot <- function(param_list) {
  plots <- list()
  for(param in param_list) {
    pwc <- pairwise_test(param)
    plot <- ggboxplot(Test, x = "Gen", y = param, color = "GC", 
                      palette = c("#00AFBB", "#E7B800"), add = "jitter") +
      labs(x = "Genotype", y = param) +
      stat_pvalue_manual(pwc, hide.ns = TRUE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[[param]] <- plot
  }
  
  # Arrange the plots in a grid
  combined_plot <- ggarrange(plots[[param_list[1]]], plots[[param_list[2]]], plots[[param_list[3]]],
                             ncol = 1, nrow = 3, labels = param_list)
  return(combined_plot)
}

# Create two sets of plots
plot1 <- create_plot(c("Gwt", "Agb", "Swt"))
plot2 <- create_plot(c("Lwt", "Pwt", "PH"))

# Display the plots
print(plot1)
print(plot2)



#Bar plot style
# Load necessary libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
df <- read.csv(file.choose(),header = TRUE)
df
Test <- df
Test
str(Test)

# Assuming 'Test' is your dataset
Test <- Test %>%
  rename(Gwt = Gwt, Agb = Agb, Swt = Swt, Lwt = Lwt, Pwt = Pwt, PH = PH) %>%
  convert_as_factor(Gen, GC)

# Define the full parameter names
param_names <- c(
  Gwt = "Grain Weight (gm)",
  Agb = "Above Ground Biomass (gm)",
  Swt = "Stem Weight (gm)",
  Lwt = "Leaf Weight (gm)",
  Pwt = "Pod Weight (gm)",
  PH = "Plant Height (cm)"
)

# Perform pairwise t-tests for each variable
pairwise_test <- function(variable) {
  pwc <- Test %>%
    pairwise_t_test(as.formula(paste(variable, "~ GC")), paired = FALSE, p.adjust.method = "bonferroni")
  pwc <- pwc %>% add_xy_position(x = "Gen")
  return(pwc)
}

# Create the bar plot
create_bar_plot <- function(param_list) {
  plots <- list()
  for(param in param_list) {
    pwc <- pairwise_test(param)
    plot <- ggbarplot(Test, x = "Gen", y = param, fill = "GC", 
                      palette = c("#00AFBB", "#E7B800"), add = "mean_sd", 
                      position = position_dodge(0.8), error.plot = "errorbar") +
      labs(x = "Genotype", y = param_names[[param]], title = param_names[[param]]) +
      stat_pvalue_manual(pwc, hide.ns = TRUE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[[param]] <- plot
  }
  
  # Arrange the plots in a grid
  combined_plot <- ggarrange(plots[[param_list[1]]], plots[[param_list[2]]], plots[[param_list[3]]],
                             ncol = 1, nrow = 3, labels = param_list)
  return(combined_plot)
}

# Create two sets of bar plots
bar_plot1 <- create_bar_plot(c("Gwt", "Agb", "Swt"))
bar_plot2 <- create_bar_plot(c("Lwt", "Pwt", "PH"))

# Display the bar plots
print(bar_plot1)
print(bar_plot2)


