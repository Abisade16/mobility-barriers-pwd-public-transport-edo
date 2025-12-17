library(tidyverse)
library(readxl)

edo_survey<- read_excel("Disability_IN_EDO_State(cleaning).xlsx")

#SECTION ONE
#percentage of gender
gender <- edo_survey|>
  group_by(sex)|>
  summarise(n=n())|>
  mutate(percentage = (n / sum(n)) * 100)
gender

#categorising age column
age_category <- edo_survey|>
  mutate(
    age_grouping= case_when(
    age>=12&age<=19 ~ "12-19",
    age>=20&age<=27 ~ "20-27",
    age>=28&age<=35 ~ "28-35",
    age>=36&age<=43 ~ "36-43",
    age>=44&age<=51 ~ "44-51",
    age>=52&age<=59 ~ "52-59",
    age>=60&age<=67 ~ "60-67",
    TRUE ~ "68+"
    ))|>
  group_by(age_grouping)|>
  summarise(count = n())|>
  mutate(percentage = (count / sum(count)) * 100)
age_category

educational_status <- edo_survey|>
  group_by(educational_status)|>
  summarise(number=n())|>
  mutate(percentage = number/sum(number)*100)
educational_status

occupation <- edo_survey|>
  group_by(occupation)|>
  summarise(Count=n())|>
  mutate(percentage = Count/sum(Count)*100)
occupation

disability_type <- edo_survey|>
  group_by(disability_type)|>
  summarise(Number=n())|>
  mutate(percentage = Number/sum(Number)*100)
disability_type

disability_cause <- edo_survey|>
  group_by(disability_cause)|>
  summarise(numbeR=n())|>
  mutate(percentage = numbeR/sum(numbeR)*100)
disability_cause

## SECTION TWO
assistive_device <- edo_survey|>
  group_by(`assistive mobility device`)|>
  summarise(numbER=n())|>
  mutate(percentage = numbER/sum(numbER)*100)
assistive_device

crosstab <- table(edo_survey$`assistive mobility device`, edo_survey$disability_type)
crosstab

## SECTION THREE
crosstab2 <- table(edo_survey$home_to_busstop_mode, edo_survey$disability_type)
crosstab2

crosstab3 <- table(edo_survey$trip_purpose, edo_survey$disability_type)
crosstab3

crosstab4 <- table(edo_survey$time_to_use_public_transport, edo_survey$disability_type)
crosstab4

tab3 <- xtabs(~ reliability_1 + reliability_2 + disability_type, data = edo_survey)
round(prop.table(tab3, margin = 3)*100, 1)