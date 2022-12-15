#1
library(tidyverse)
library(RColorBrewer)
library(Hmisc)
library(patchwork)

#2
messy_assignment_dataset <- read_csv("/Users/dhiyaakhtar/R_Assignment/assignment1_dataset1.csv")

#3
long_format_data <- messy_assignment_dataset %>%
  pivot_longer(cols = c(Condition1, Condition2, Condition3, Condition4), 
               names_to = "Condition", 
               values_to = "RT")

#4
tidied_long_format <- long_format_data %>%
  mutate(Condition = recode(Condition,
                            "Condition1" = "ExpectationNumber_TargetNumber",
                            "Condition2" = "ExpectationNumber_TargetLetter", 
                            "Condition3" = "ExpectationLetter_TargetNumber", 
                            "Condition4" = "ExpectationLetter_TargetLetter")) %>%
  separate(col = "Condition", into = c("Expectation", "Target"), sep = "_") %>%
  mutate(Expectation = factor(Expectation), Target = factor(Target))

#5
C1_summary <- tidied_long_format %>%
  filter(Expectation == "ExpectationNumber" & Target == "TargetNumber") %>%
  summarise(N = n(), Mean = mean(RT), Median = median(RT), SD = sd(RT), 
            Min = min(RT), Max = max(RT))

C2_summary <- tidied_long_format %>%
  filter(Expectation == "ExpectationNumber" & Target == "TargetLetter") %>%
  summarise(N = n(), Mean = mean(RT), Median = median(RT), SD = sd(RT),
            Min = min(RT), Max = max(RT))

C3_summary <- tidied_long_format %>%
  filter(Expectation == "ExpectationLetter" & Target == "TargetNumber") %>%
  summarise(N = n(), Mean = mean(RT), Median = median(RT), SD = sd(RT),
            Min = min(RT), Max = max(RT))

C4_summary <- tidied_long_format %>%
  filter(Expectation == "ExpectationLetter" & Target == "TargetLetter") %>%
  summarise(N = n(), Mean = mean(RT), Median = median(RT), SD = sd(RT),
            Min = min(RT), Max = max(RT))

Summary_stat_table <- rbind(C1_summary, C2_summary, C3_summary, C4_summary) %>%
  round(digits = 2)
  
Summary_stat_table$Condition <- c(1, 2, 3, 4)

Summary_stat_table <- Summary_stat_table %>%
  relocate(Condition, .before = N)
 
#visualisations 
histogram <- tidied_long_format %>%
  ggplot(aes(x = RT, fill = Expectation:Target)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black", 
                 fill = "white") + 
  geom_density(size = 1, alpha=.5, fill="darkslategray4") +
  labs(title = "Histogram showing the Reaction Time in Expectation and Target
       Variables with Density Curve",
       x = "Reaction Time in Milliseconds",
       y = "Density") +
  facet_wrap(~ Expectation:Target, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),#plot.title is a parameter of theme whichw e use to change the title, element_text() to center the title
        axis.title = element_text(face = "bold"),
        strip.background = element_rect(colour = "black", fill = "mistyrose2"))  # strip.background parameter of theme and function is used to change color of box, color and fill property of element_rect() and function is used for color change
 #: to take the two variables from the dataset


jitter_plot <- ggplot(tidied_long_format) +
  geom_jitter(aes(x = Expectation:Target, y = RT, colour = Expectation:Target), 
              width = 0.2, size = 2) + # mapping colour to our conditions (expectation and target)
  stat_summary(aes(x = Expectation:Target, y = RT), fun = mean, geom = 'point', 
               colour = 'black', size = 3) +  #using stat sumary to calculate mean, fun function to calculate the mean and geom function to change the default geom of stat summar, size = 2)
  scale_color_brewer(palette = 'Pastel1') + # using scale_colour_brewer() to add a scale to the colour and choose pastel2
  labs(title = "Jitter Plot showing the Mean Reaction Time for each Condition",
       x = "Condition 1 - 4",
       y = "Reaction Time in Milliseconds") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) + # by default, the title is left-alligned, so we used these functions to center the title
  scale_x_discrete(limits=c("ExpectationNumber:TargetNumber", 
                            "ExpectationNumber:TargetLetter", 
                            "ExpectationLetter:TargetNumber",
                            "ExpectationLetter:TargetLetter"))

boxplot <- tidied_long_format %>%
  ggplot(aes(x = Expectation:Target, y = RT, colour = Expectation:Target)) + 
  geom_violin() +
  geom_jitter(alpha = .5, width = .2, colour = "black", shape = 22) +
  geom_boxplot(alpha = .5) +
  scale_color_brewer(palette = 'Pastel1') + 
  labs(title = "Boxplot showing the distribution of Reaction Time in each Condition", 
       x = "Condition 1 - 4", 
       y = "Raction Time in Milliseconds") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold")) +
  scale_x_discrete(limits=c("ExpectationNumber:TargetNumber", 
                            "ExpectationNumber:TargetLetter", 
                            "ExpectationLetter:TargetNumber",
                            "ExpectationLetter:TargetLetter"))

my_plots <- (histogram + jitter_plot / boxplot)


