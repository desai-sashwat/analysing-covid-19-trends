
library(ggplot2)
library(dplyr)
library(corrplot)
data <- read.csv("C:/Users/zhang/Desktop/COVID-19 Survey Student Responses.csv", header = TRUE)
str(data)
colSums(is.na(data))

ggplot(data, aes(x = Medium.for.online.class, y = Time.spent.on.Online.Class)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Relationship between online course medium and study hours", x = "Course Medium", y = "Hours") +
  theme_minimal()


data %>%
  group_by(Health.issue.during.lockdown) %>%
  summarise(
    sleep_mean = mean(Time.spent.on.sleep, na.rm = TRUE),
    fitness_mean = mean(Time.spent.on.fitness, na.rm = TRUE)
  )


p1 <- ggplot(data, aes(x = Prefered.social.media.platform)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Favourite Plateform", x = "Plateform", y = "Num") +
  theme_minimal()
print(p1)


weight_behavior <- data %>%
  group_by(Change.in.your.weight) %>%
  summarise(
    study_time = mean(Time.spent.on.self.study, na.rm = TRUE),
    fitness_time = mean(Time.spent.on.fitness, na.rm = TRUE),
    social_time = mean(Time.spent.on.social.media, na.rm = TRUE)
  )
print(weight_behavior)
#hypothesis testing
str(data)
data$Change.in.your.weight <- as.factor(data$Change.in.your.weight)
# 1. self-study hours ~ weight change
anova_self_study <- aov(Time.spent.on.self.study ~ Change.in.your.weight, data = data)
summary(anova_self_study)
# 2.Length of Fitness ~ Weight Change
anova_fitness <- aov(Time.spent.on.fitness ~ Change.in.your.weight, data = data)
summary(anova_fitness)
TukeyHSD(anova_fitness)
# 3. Social Media Hours ~ Weight Change
anova_social <- aov(Time.spent.on.social.media ~ Change.in.your.weight, data = data)
summary(anova_social)
TukeyHSD(anova_social)

# Correlation analysis of numerical variables
num_data <- data[sapply(data, is.numeric)]
cor_matrix <- cor(num_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# t 
fitness_yes <- data$Time.spent.on.fitness[data$Health.issue.during.lockdown == "YES"]
fitness_no <- data$Time.spent.on.fitness[data$Health.issue.during.lockdown == "NO"]

t.test(fitness_yes, fitness_no)

#ANOVA
anova_model_study <- aov(Time.spent.on.self.study ~ Change.in.your.weight, data = data)
summary(anova_model_study)

#4. correlation test: is time spent on self-study correlated with time spent on social media?
cor.test(data$Time.spent.on.self.study, data$Time.spent.on.social.media)

data$Health.issue.during.lockdown <- as.factor(data$Health.issue.during.lockdown)
data$Time.utilized <- as.factor(data$Time.utilized)

# 4. Check Factor Levels
table(data$Health.issue.during.lockdown)
table(data$Time.utilized)

tab_health_time <- table(data$Health.issue.during.lockdown, data$Time.utilized)

# 2. View Crosstab
tab_health_time

# 3. chi-square 
chi_res <- chisq.test(tab_health_time)
chi_res

ggplot(data, aes(x = Health.issue.during.lockdown, fill = Time.utilized)) +
  geom_bar(position = "fill") +
  labs(title = "Time Utilization by Health Issue Status",
       x = "Health Issue During Lockdown",
       y = "Proportion") +
  theme_minimal()





