library(tidyverse)
library(readxl)
library(MASS)
library(VGAM)
library(psych)

df <- read_excel("Disability_IN_EDO_State(cleaning).xlsx")

#converting categorical columns to factor
categorical_columns <- c('disability_type', 'LGA', 'educational_status','occupation')
df[categorical_columns] <- lapply(df[categorical_columns], as.factor)

#Hypothesis 1: The choice of travel mode used among PLWDs is not significantly influenced by age, income status and disability type.
df_clean <- df[,c("Public bus (government owned)", 
                           "Public bus (Privately owned)", 
                           "Public taxi (privately owned)", 
                           "Public tricycle/Keke", 
                           "Walking", 
                           "Public bike (Okada)", 
                           "Other_mode", 
                           "age", 
                           "monthly_income", 
                           "disability_type")]

#fitting the multinomial logistic regression model
multinom_fit <- vglm(cbind(`Public bus (government owned)`, `Public bus (Privately owned)`, `Public taxi (privately owned)`,
                  `Public tricycle/Keke`, `Walking`, `Public bike (Okada)`,	`Other_mode`) ~ `age` + `monthly_income` + `disability_type`,
            family = multinomial, data = df_clean)
summary(multinom_fit)
exp(coef(multinom_fit))

#Hypothesis 2: Public road transport infrastructure barriers experienced among PLWDs is not significantly influenced by age, disability types, and locations.
infrasturcture_barriers_columns <- c("bus_stop_convenience",	"accessible_pathways",	"accessible_handrails",
                           "ramp_incline_width",	"bus_stop_lighting_signage",	"boarding_path_clear",
                           "easy_navigation",	"bus_terminal_shelters",	"bus_stop_shelters",
                           "trained_staff_guidance",	"toilet_accessibility",	"toilets_manoeuvring_space"
                         )

df[infrasturcture_barriers_columns] <- lapply(df[infrasturcture_barriers_columns], function(x) {
  factor(x,
         levels = 1:5,
         ordered = TRUE)
  })

infrasturcture_barriers_columns_clean <- infrasturcture_barriers_columns[!(infrasturcture_barriers_columns %in% c("accessible_handrails", "trained_staff_guidance"))]

fits= list()

for (y in infrasturcture_barriers_columns_clean) {
  f <- as.formula(paste(y, "~ age + disability_type + LGA"))
  fits[[y]] <-polr(f, data = df, Hess = TRUE)
}

summary(fits[["bus_stop_convenience"]])
exp(coef(fits[["bus_stop_convenience"]]))

# Hypothesis 3:The choice of mobility coping strategies adopted by PLWDs is not significantly influenced by socio-demographic characteristics
coping_strategies_columns <- c("assistive_device", "travel_companion", "trip_planning",	"avoid_multiple_stops",
                               "easy_transport_choice",	"offpeak_travel", "essential_trips_only"
)

df[coping_strategies_columns] <- lapply(df[coping_strategies_columns], function(x) {
  factor(x,
         levels = 1:5,
         ordered = TRUE)
})

fits_2 <- list()

for (columns in coping_strategies_columns) {
  f_2 <- as.formula(paste(columns, "~ `age` + `disability_type` + `educational_status`+ `occupation`"))
  fits_2[[columns]] <- polr(f_2, data = df, Hess = TRUE)
}

summary(fits_2[["assistive_device"]])
exp(coef(fits[["assistive_device"]]))

#Hypothesis 4: The frequency of social participation by PLWDs is not significantly influenced by reliability of public road transport service, travel cost, waiting time, travel distance, age, income status, education status and disability type.
df <- df|>
  mutate(trip_time_mins= trip_time*60)

#checking for reliability
columns <- df[,c("reliability_1","reliability_2")]
alpha(columns)

df$reliability <- rowMeans(columns)
  
nb_model <- glm.nb(`frequency_social_participation`~`travel_cost`+ `trip_time_mins`+
                     `waiting_time(mins)`+`monthly income`+`age`+`reliability`, data=df)
summary(nb_model)
exp(coef(nb_model))
