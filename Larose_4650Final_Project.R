# Project - Sleep Health and Lifestyle Thesis Disseration
# Esther Larose - U1327652
# 
# Sleep is a crucial outcome of maintaining a healthy lifestyle, constituting 
# approximately one-third of our lives. However, numerous individuals fail to 
# achieve this requisite balance. According to the National Institutes of Health
# insomnia, the most prevalent sleep disorder in the United States, impacts 
# about one-third of the general population. Understanding the significance of 
# sleep is paramount.
#
# Data Features:
# Person ID: An identifier for each individual
# Gender: The gender of the person (Male/Female).
# Age: The age of the person in years.
# Occupation: The occupation or profession of the person.
# Sleep Duration (hours): The number of hours the person sleeps per day.
# Quality of Sleep (scale: 1-10): A subjective rating of the quality of sleep, 
# ranging from 1 to 10.
# Physical Activity Level (minutes/day): The number of minutes the person 
# engages in physical activity daily.
# Stress Level (scale: 1-10): A subjective rating of the stress level 
# experienced by the person, ranging from 1 to 10.
# BMI Category: The BMI category of the person (e.g., Underweight, Normal, 
# Overweight).
# Blood Pressure (systolic/diastolic): The blood pressure measurement of the 
# person, indicated as systolic pressure over diastolic pressure.
# Heart Rate (bpm): The resting heart rate of the person in beats per minute.
# Daily Steps: The number of steps the person takes per day.
# Sleep Disorder: The presence or absence of a sleep disorder in the person 
# (None, Insomnia, Sleep Apnea).
#
#
> library(readr)
> data <- read_csv("Desktop/sleeplife.csv")
> install.packages("plotly")
> install.packages(c("tidyverse", "caret"))
> library(plotly)
> library(tidyverse)
> library(caret)
> library(ggplot2)

#
# Finding the percentage of sleep disorder vs those who don't
> percentage_sleep_disorder <- mean(data$Sleep.Disorder) * 100
# 41.4438 % of people have sleep disorders
> percentage_no_sleep_disorder <- 100 - percentage_sleep_disorder
# 58.5561 % of people do not have sleep disorders
#
# Finding the percentage of males with sleep disorder
> percentage_male_sleep_disorder <- mean(data$Sleep.Disorder[data$Gender == 
                                                               "Male"]) * 100
# 27.5132 % of males with sleep disorder
# Finding the percentage of female with sleep disorder
> percentage_female_sleep_disorder <- mean(data$Sleep.Disorder[data$Gender == 
                                                              "Female"]) * 100
# 55.6756 # of female with sleep disorder
# Logistic Regression Model 
> data$Sleep.Disorder <- ifelse(data$Sleep.Disorder == "None", 0, 1)
> sleepmodel <- glm(Sleep.Disorder ~ Sleep.Duration + Quality.of.Sleep + Stress.
                    Level, data = data, family = "binomial")
> summary(sleepmodel)

Call:
  glm(formula = Sleep.Disorder ~ Sleep.Duration + Quality.of.Sleep + 
        Stress.Level, family = "binomial", data = data)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)       17.2817     2.8393   6.087 1.15e-09 ***
  Sleep.Duration    -0.9331     0.3200  -2.916 0.003549 ** 
  Quality.of.Sleep  -0.9884     0.2971  -3.327 0.000879 ***
  Stress.Level      -0.7030     0.1581  -4.446 8.74e-06 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 507.47  on 373  degrees of freedom
Residual deviance: 440.16  on 370  degrees of freedom
AIC: 448.16

Number of Fisher Scoring iterations: 4

# Results provide information about the relationships between the predictor 
# variables (Sleep Duration, Quality of Sleep, and Stress Level) and the binary
# response variable (Sleep Disorder).
#
# Interpretation of the coefficients:
# Sleep Duration (-0.9331): For each one-unit decrease in Sleep Duration, 
# the log-odds of having a sleep disorder decrease by 0.9331 * 100 = 93.31% 
# holding all variables constant. The p-value (0.003549) indicates that Sleep 
# Duration is statistically significant.
#
# Quality of Sleep (-0.9884): For each one-unit decrease in Quality of Sleep, 
# the log-odds of having a sleep disorder decrease by 0.9884 * 100 = 98.84% 
# holding all variables constant. The p-value (0.000879) suggests that Quality 
# of Sleep is statistically significant.
#
# Stress Level (-0.7030): For each one-unit decrease in Stress Level, 
# the log-odds of having a sleep disorder decrease by 0.7030 * 100 = 70.30% 
# holding all variables constant. The p-value (8.74e-06) indicates that 
# Stress Level is statistically significant.
#
# Percentage of individuals with sleep disorders based on their occupation
> occupation_sleep_table <- table(data$Occupation, data$Sleep.Disorder)
> occupation_sleep_df <- as.data.frame(occupation_sleep_table)
> occupation_sleep_df$Percentage_Sleep_Disorder <- (occupation_sleep_df$Freq / 
                                         rowSums(occupation_sleep_table)) * 100
> print(occupation_sleep_df)
Var1 Var2 Freq Percentage_Sleep_Disorder
1            Accountant    0   30                 81.081081
2                Doctor    0   64                 90.140845
3              Engineer    0   57                 90.476190
4                Lawyer    0   42                 89.361702
5               Manager    0    1                100.000000
6                 Nurse    0    9                 12.328767
7  Sales Representative    0    0                  0.000000
8           Salesperson    0    2                  6.250000
9             Scientist    0    2                 50.000000
10    Software Engineer    0    3                 75.000000
11              Teacher    0    9                 22.500000
12           Accountant    1    7                 18.918919
13               Doctor    1    7                  9.859155
14             Engineer    1    6                  9.523810
15               Lawyer    1    5                 10.638298
16              Manager    1    0                  0.000000
17                Nurse    1   64                 87.671233
18 Sales Representative    1    2                100.000000
19          Salesperson    1   30                 93.750000
20            Scientist    1    2                 50.000000
21    Software Engineer    1    1                 25.000000
22              Teacher    1   31                 77.500000
# Intereptation Of Sleep Disorder in Occupation 
#
# Occupation : Accountant
# Individuals with sleep disorders: 7 (18.92%)
# Individuals without sleep disorders: 30 (81.08%)
#
# Occupation : Doctor
# Individuals without sleep disorders: 64 (90.14%)
# Individuals with sleep disorders: 7 (9.86%)
#
# Occupation : Enginner
# Individuals without sleep disorders: 57 (90.47%)
# Individuals with sleep disorders: 6 (9.523%)
#
# Occupation : Lawyer
# Individuals without sleep disorders:42 (89.36%)
# Individuals with sleep disorders: 5 (10.64%)
#
# Occupation: Manager
# Individuals without sleep disorders:1 (100.00%)
# Individuals with sleep disorders: 0 (0.00%)
#
# Occupation : Nurse
# Individuals without sleep disorders:9 (12.33%)
# Individuals with sleep disorders: 64 (87.67%)
#
# Occupation : Sales Representative
# Individuals without sleep disorder: 0 (0.00%)
# Individuals with sleep disorder: 2 (100.00%)
#
# Occupation : Salesperson
# Individuals without sleep disorder: 2 (6.25%)
# Individuals with sleep disorder:30 (93.75%)
#
# Occupation : Scientist
# Individuals without sleep disorder: 2 (50.00%)
# Individuals with sleep disorder: 2 (50.00%)
#
# Occupation: Software Engineer 
# Individuals without sleep disorder: 3 (75.00%)
# Individuals with sleep disorder:1 (25.00%)
#
# Occupation: Teacher
# Individuals without sleep disorder:  9 (22.50%)
# Individuals with sleep disorder: 31 (77.50%)
#
# Sales Representative has the most sleep disorders
#
# Bar Plot of People with Sleep Disorders by Age Group
> age_sleep_order <- subset(Sleep_health_and_lifestyle_dataset, 
                  Sleep_health_and_lifestyle_dataset$`Sleep.Disorder` != "None")
  > ggplot(age_sleep_order, aes(x = Age, fill = `Sleep.Disorder`)) +
  +     geom_bar(position = "stack") +
  +     labs(title = "Count of People with Sleep Disorders by Age Group",
             +          x = "Age",
             +          y = "Count") +
  +     theme_minimal()
> 
# Insomnia is most prevalent in the Age Group: 40
# Sleep Apnea is most prevalent in the Age Group: 50
#
# Effect of Sleep Duration on Sleep Disorder
  ggplot(Sleep_health_and_lifestyle_dataset, aes(x = `Sleep.Duration`, y = 
                                                   `Sleep.Disorder`)) +
  +     geom_point(aes(color = `Sleep.Disorder`)) +
  +     geom_smooth(method = "lm", se = FALSE) +
  +     labs(title = "Effect of Sleep Duration on Sleep Disorder",
             +          x = "Sleep Duration",
             +          y = "Sleep Disorder") +
  +     theme_minimal()
#
# Comparing the percentage of people with sleep disorders among individuals with
# different BMI categories 
#
# Assuming 'BMI Category' has levels 'Normal', 'Overweight', and 'Obese'
> and 'Sleep Disorder' has levels 'None', 'Insomnia', 'Sleep Apnea'
> 
> contingency_table <- table(Sleep_health_and_lifestyle_dataset$BMI.Category, 
                             Sleep_health_and_lifestyle_dataset$Sleep.Disorder)
> 
> normal_counts <- contingency_table['Normal', ]
> overweight_counts <- contingency_table['Overweight', ]
> obese_counts <- contingency_table['Obese', ]
> 
> percentage_normal_sleep_disorder <- (normal_counts / sum(normal_counts)) * 100
> percentage_overweight_sleep_disorder <- (overweight_counts / 
                                             sum(overweight_counts)) * 100
> percentage_obese_sleep_disorder <- (obese_counts / sum(obese_counts)) * 100
> 
> cat("Percentage of people with sleep disorder in the 'Normal' weight category
      :\n")
Percentage of people with sleep disorder in the 'Normal' weight category:
> print(percentage_normal_sleep_disorder)
Insomnia        None Sleep Apnea 
3.589744   93.846154    2.564103 
> 
  > cat("\nPercentage of people with sleep disorder in the 'Overweight' weight 
        category:\n")

Percentage of people with sleep disorder in the 'Overweight' weight category:
  > print(percentage_overweight_sleep_disorder)
Insomnia        None Sleep Apnea 
43.24324    12.83784    43.91892 
> 
  > cat("\nPercentage of people with sleep disorder in the 'Obese' weight 
        category:\n")

Percentage of people with sleep disorder in the 'Obese' weight category:
  > print(percentage_obese_sleep_disorder)
Insomnia        None Sleep Apnea 
40           0          60 
> 
# percentage of people with sleep disorders for each level of stress
#
>contingency_table <- table(Sleep_health_and_lifestyle_dataset$Stress.Level, 
                            Sleep_health_and_lifestyle_dataset$Sleep.Disorder)
> 
> percentage_stress_sleep_disorder <- prop.table(contingency_table, margin = 1)
* 100
> 
> cat("Percentage of people with sleep disorder for each stress level:\n")
Percentage of people with sleep disorder for each stress level:
  > print(percentage_stress_sleep_disorder)

Insomnia      None Sleep Apnea
3  1.408451 56.338028   42.253521
4 34.285714 61.428571    4.285714
5  8.955224 85.074627    5.970149
6  4.347826 93.478261    2.173913
7 82.000000  6.000000   12.000000
8  4.285714 47.142857   48.571429
#
# Stress Level 3:
Insomnia: 1.41%
None: 56.34%
Sleep Apnea: 42.25%

#Stress Level 4:
Insomnia: 34.29%
None: 61.43%
Sleep Apnea: 4.29%

#Stress Level 5:
Insomnia: 8.96%
None: 85.07%
Sleep Apnea: 5.97%

#Stress Level 6:
Insomnia: 4.35%
None: 93.48%
Sleep Apnea: 2.17%

#Stress Level 7:
Insomnia: 82.00%
None: 6.00%
Sleep Apnea: 12.00%

#For Stress Level 8:
Insomnia: 4.29%
None: 47.14%
Sleep Apnea: 48.57%

