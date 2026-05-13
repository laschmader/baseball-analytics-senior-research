graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console
rm(list = ls()) # clears variables


library(tidyverse)
library(ggplot2)
library(mosaic)
library(xtable)
library(broom)
library(car)
library(modelsummary)


#my_file <- file.choose()
my_file <- "/Users/landonschmader/Downloads/fangraphs-minor-league-leaders (10).csv"
compData <- read.table(my_file, header = TRUE, sep = ",")
View(compData)
#my_Data <- filter(compData_test, Season < 2025)

beforeABS <- filter(compData, compData$Season<2023)
afterABS <- filter(compData, compData$Season>=2023)


# Find summary stats/distribution
summary(beforeABS)
summary(afterABS)

# graphing different variables of interest for normality before ABS by density plots

ggplot(beforeABS, aes(x= K.)) + 
  geom_density(fill = "lightblue", alpha = 0.5) +
  ggtitle("Distribution of Strike Out Percentage Without  ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(beforeABS, aes(x=BB.)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Walk Rate Without ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(beforeABS, aes(x=SwStr.)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Swinging Strike Percentage Without ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(beforeABS, aes(x=Balls)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Number of Balls Without ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(beforeABS, aes(x=Strikes)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Number of Strikes Without ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(beforeABS, aes(x=Pitches)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Number of Pitches Without ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

# graphing variables of interest for normality after ABS
ggplot(afterABS, aes(x= K.)) + 
  geom_density(fill = "lightblue", alpha = 0.5) +
  ggtitle("Distribution of Strike Out Percentage With ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(afterABS, aes(x=BB.)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Walk Rate With ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(afterABS, aes(x=SwStr.)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Swinging Strike Percentage With ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(afterABS, aes(x=Balls)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Number of Balls With ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(afterABS, aes(x=Strikes)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Number of Strikes With ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

ggplot(afterABS, aes(x=Pitches)) +
  geom_density(fill = "lightblue", alpha= 0.5) +
  ggtitle("Distribution of Number of Pitches With ABS")+
  theme(plot.title = element_text(size = 25))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size =20))

## Testing data for differences with appropriate hypothesis test

# K-percent: independent t-test to account for difference in most hitters over the years and account for variance amongst subjects and approx.normal

 t.test(x=beforeABS$K., y = afterABS$K., paired = FALSE)
 
 # BB percent : Welch's t-test

t.test(x=beforeABS$BB., y = afterABS$BB., paired = FALSE)

# Swinging Strike Percent: Welch's t-test

t.test(x=beforeABS$SwStr., y = afterABS$SwStr., paired = FALSE)


# Using function for z-test

ztest <- function(x, y, alternative = "two.sided") {
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x <- sd(x)
  sd_y <- sd(y)
  n_x <- length(x)
  n_y <- length(y)
  
  # Compute z statistic
  z <- (mean_x - mean_y) / sqrt((sd_x^2 / n_x) + (sd_y^2 / n_y))
  
  # Two-tailed p-value (by default)
  p <- switch(alternative,
              "two.sided" = 2 * (1 - pnorm(abs(z))),
              "less" = pnorm(z),
              "greater" = 1 - pnorm(z))
  
  result <- list(
    z = z,
    p_value = p,
    mean_x = mean_x,
    mean_y = mean_y,
    diff = mean_x - mean_y,
    alternative = alternative
  )
  
  return(result)
}

ztest(x= beforeABS$Balls, y = afterABS$Balls)
ztest(x= beforeABS$Strikes, y= afterABS$Strikes)
ztest(x= beforeABS$Pitches, y= afterABS$Pitches)



# Balls : Mann-Whitney U test (works well with large samples, non-parametric (not normal), independent samples, compares distributions of data)

# t.test(x= beforeABS$Balls, y = afterABS$Balls, alternative = "two.sided", paired = FALSE)

# Strikes : Mann-Whitney U Test

# t.test(x= beforeABS$Strikes, y = afterABS$Strikes, alternative = "two.sided", paired = FALSE)

# Pitches

# t.test(x= beforeABS$Pitches, y = afterABS$Pitches, alternative = "two.sided", paired = FALSE)

# Adding some fixed effects regression

# install.packages('plm')

library(plm)

# creating new column for fixed effects values
fixed_effects_data <- compData %>%
  mutate(abs_used = ifelse(Season >= 2023,1,0))

panel_data_new <- pdata.frame(fixed_effects_data, index = c("PlayerId", "Season"))


# start running fixed effects for all variables; adding Age and PA as control variables

# install.packages("fixest")

library(fixest)

panel_data_new$Season <- as.factor(panel_data_new$Season)

k_percent_model <- feols(K. ~ abs2023 + abs2024 + abs2025 + Age + PA | PlayerId,  data = panel_data_new, cluster = ~PlayerId)
summary(k_percent_model)

bb_percent_model <- feols(BB.~ abs2023 + abs2024 + abs2025 + Age + PA | PlayerId , data = panel_data_new, cluster = ~PlayerId)
summary(bb_percent_model)


SwStr_percent_model <- feols(SwStr.~ abs2023 + abs2024 + abs2025 + Age +PA | PlayerId, data = panel_data_new, cluster = ~PlayerId)
summary(SwStr_percent_model)


# logged PA to make up for effect that more PA = more balls; these show amount of balls increase/decrease per PA

Balls_model <- fepois(Balls~abs2023 + abs2024+ abs2025 + Age| PlayerId, offset = ~log(PA), data = panel_data_new, cluster = ~PlayerId)
summary(Balls_model)


Strikes_model <- fepois(Strikes~ abs2023 + abs2024 + abs2025 + Age| PlayerId, offset = ~log(PA), data = panel_data_new, cluster = ~PlayerId)
summary(Strikes_model)


Pitches_model <- fepois(Pitches~abs2023 + abs2024 + abs2025 + Age| PlayerId, offset = ~log(PA), data = panel_data_new, cluster = ~PlayerId)
summary(Pitches_model)


# exporting models


etable(k_percent_model, bb_percent_model, SwStr_percent_model, tex = TRUE, file = "ols_new.tex")
etable(Balls_model, Strikes_model, Pitches_model, tex = TRUE, file = "poisson_new.tex")







# Shows number of players who played with what level of ABS; helps with robustness/generalization of results
player_exposure <- fixed_effects_data %>%
  group_by(PlayerId) %>%
  summarize(has_abs = any(abs_used == 1),
            has_nonabs = any(abs_used == 0)) %>%
  mutate(group = case_when(
    has_abs & has_nonabs ~ "Both ABS + Non-ABS",
    has_abs & !has_nonabs ~ "ABS only",
    !has_abs & has_nonabs ~ "Non-ABS only"
  ))

table(player_exposure$group)

library(car)
vif(lm(K. ~ abs2023 + abs2024 + abs2025 + Age + PA, data = panel_data_new))

# possibly graph these trends

avg_K_percent_seasonal <- compData %>%
  group_by(Season) %>%
  summarise(mean_K = mean(K., na.rm = TRUE))

K_percent_avg_over_years <- compData %>%
  summarise(total_avg_K.= mean(K., na.rm = TRUE))

K_percent_avg_over_years$total_avg_K. <- unlist(K_percent_avg_over_years$total_avg_K.)
  

yearly_K._graph <- ggplot(avg_K_percent_seasonal, aes(x = Season, y = mean_K))+
  geom_line()+
  geom_point(color = "orange", size = 4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red")

yearly_K._graph + geom_hline(yintercept = K_percent_avg_over_years$total_avg_K., linetype = "dashed" , color = "blue") + 
  labs(title = "Average Strikeout Percentage (Annual vs. Aggregate)")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 16))
  
avg_bb_percent_seasonal <- compData %>%
  group_by(Season) %>%
  summarise(mean_bb = mean(BB., na.rm = TRUE))

BB_percent_avg_over_years <- compData %>%
  summarise(total_avg_BB.= mean(BB., na.rm = TRUE))

BB_percent_avg_over_years$total_avg_BB. <- unlist(BB_percent_avg_over_years$total_avg_BB.)


yearly_BB._graph <- ggplot(avg_bb_percent_seasonal, aes(x = Season, y = mean_bb))+
  geom_line()+
  geom_point(color = "orange", size = 4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red")

yearly_BB._graph + geom_hline(yintercept = BB_percent_avg_over_years$total_avg_BB., linetype = "dashed" , color = "blue") +
  labs(title = "Average Walk Rate (Annual vs Aggregate)")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))



avg_SwStr_percent_seasonal <- compData %>%
  group_by(Season) %>%
  summarise(mean_SwStr = mean(SwStr., na.rm = TRUE))

SwStr_percent_avg_over_years <- compData %>%
  summarise(total_avg_SwStr.= mean(SwStr., na.rm = TRUE))

SwStr_percent_avg_over_years$total_avg_SwStr. <- unlist(SwStr_percent_avg_over_years$total_avg_SwStr.)


yearly_SwStr._graph <- ggplot(avg_SwStr_percent_seasonal, aes(x = Season, y = mean_SwStr))+
  geom_line()+
  geom_point(color = "orange", size = 4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red")

yearly_SwStr._graph + geom_hline(yintercept = SwStr_percent_avg_over_years$total_avg_SwStr., linetype = "dashed" , color = "blue") +
  labs(title = "Average Swinging Strike Rate (Annual vs Aggregate)")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))

avg_Balls_seasonal <- compData %>%
  group_by(Season) %>%
  summarise(mean_Balls = mean(Balls, na.rm = TRUE))

Balls_avg_over_years <- compData %>%
  summarise(total_avg_Balls= mean(Balls, na.rm = TRUE))

Balls_avg_over_years$total_avg_Balls <- unlist(Balls_avg_over_years$total_avg_Balls)


yearly_Balls_graph <- ggplot(avg_Balls_seasonal, aes(x = Season, y = mean_Balls))+
  geom_line()+
  geom_point(color = "orange", size = 4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red")

yearly_Balls_graph + geom_hline(yintercept = Balls_avg_over_years$total_avg_Balls, linetype = "dashed" , color = "blue") +
  labs(title = "Average Number of Balls (Annual vs. Aggregate)")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))

avg_Strikes_seasonal <- compData %>%
  group_by(Season) %>%
  summarise(mean_Strikes = mean(Strikes, na.rm = TRUE))

Strikes_avg_over_years <- compData %>%
  summarise(total_avg_Strikes= mean(Strikes, na.rm = TRUE))

Strikes_avg_over_years$total_avg_Strikes <- unlist(Strikes_avg_over_years$total_avg_Strikes)


yearly_Strikes_graph <- ggplot(avg_Strikes_seasonal, aes(x = Season, y = mean_Strikes))+
  geom_line()+
  geom_point(color = "orange", size = 4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red")

yearly_Strikes_graph + geom_hline(yintercept = Strikes_avg_over_years$total_avg_Strikes, linetype = "dashed" , color = "blue") +
  labs(title = "Average Number of Strikes (Annual vs. Aggregate)")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))


avg_Pitches_seasonal <- compData %>%
  group_by(Season) %>%
  summarise(mean_Pitches = mean(Pitches, na.rm = TRUE))

Pitches_avg_over_years <- compData %>%
  summarise(total_avg_Pitches= mean(Pitches, na.rm = TRUE))

Pitches_avg_over_years$total_avg_Pitches <- unlist(Pitches_avg_over_years$total_avg_Pitches)


yearly_Pitches_graph <- ggplot(avg_Pitches_seasonal, aes(x = Season, y = mean_Pitches))+
  geom_line()+
  geom_point(color = "orange", size = 4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "red")

yearly_Pitches_graph + geom_hline(yintercept = Pitches_avg_over_years$total_avg_Pitches, linetype = "dashed" , color = "blue") +
  labs(title = "Average Number of Pitches (Annual vs. Aggregate)")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))


pre_abs_players <- unique(compData$PlayerId[compData$Season <= 2022])
abs2025_players <- unique(compData$PlayerId[compData$Season == 2025])

# Find the intersection
players_both <- intersect(pre_abs_players, abs2025_players)

# How many
length(players_both)


abs2024_players <- unique(compData$PlayerId[compData$Season == 2024])

# Find the intersection
players_both_24 <- intersect(pre_abs_players, abs2024_players)

# How many
length(players_both_24)

abs2023_players <- unique(compData$PlayerId[compData$Season == 2023])

# Find the intersection
players_both_23 <- intersect(pre_abs_players, abs2023_players)

# How many
length(players_both_23)



# ABS dummies for each year
panel_data_new$abs2023 <- ifelse(panel_data_new$Season == 2023, 1, 0)
panel_data_new$abs2024 <- ifelse(panel_data_new$Season == 2024, 1, 0)
panel_data_new$abs2025 <- ifelse(panel_data_new$Season == 2025, 1, 0)

cor(panel_data_new[, c("abs2023", "abs2024", "abs2025")])

