# young lives 

library(tidyverse)
library(janitor)
library(tidymodels)

df <- read_csv("/Users/MehriaSaadatKhan/Desktop/Intro to Data science/Final_Project/young_lives_dropout_r3_r4.csv")

df <- df %>% clean_names() 
# to make sure the names are all okay to work, no issues of capital etc 


dim(df)          # number of rows and columns
glimpse(df)      # variable types + preview
names(df)[1:25]  # first 40 column names

df %>%
  count(childid) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  head()



df %>% count(country)

names(df)

#confirm duplicates by child id 
dup_check <- df %>%
  count(country, childid, name = "n_rows") %>%
  arrange(desc(n_rows))

#Define the analysis population (at-risk children) 

#since only children enrolled can drop out we define population= children who are enrolled 

analysis_df <- df %>%
  filter(!is.na(dropout)) %>%   # keep only children with defined dropout status
  mutate(
    dropout = factor(dropout, levels = c(0, 1)),
    country = factor(country),
    sex = factor(sex),
    region = factor(region),
    typesite = factor(typesite)
  )



# check drop-out rate
analysis_df %>%
  count(dropout) %>%
  mutate(rate = n / sum(n))

analysis_df %>%
  count(country, dropout) %>%
  group_by(country) %>%
  mutate(rate = n / sum(n))




#split the data 
set.seed(123)

split <- initial_split(
  analysis_df,
  prop = 0.7,
  strata = dropout
)

train <- training(split)
test  <- testing(split)

# check if we get similar dropout rates in train and test and it shoudl be non-zero 
train %>% count(dropout) %>% mutate(rate = n / sum(n))
test  %>% count(dropout) %>% mutate(rate = n / sum(n))

#  country x split table
bind_rows( #combines the test and train into one so we can do quality checks and then un-combine
  train %>% mutate(split = "train"), #label so we can tell which went to train which to test in the new column called split
  test  %>% mutate(split = "test")
) %>%
  count(split, country) %>%
  group_by(split) %>%
  mutate(rate = n / sum(n)) %>%
  arrange(split, country)

#The training and test samples are well balanced across countries, with approximately equal representation of children from India and Peru in both splits, ensuring that model training and evaluation are not driven by country-specific sample imbalances.

# also check if both test and train has dropout 
bind_rows(
  train %>% mutate(split = "train"),
  test  %>% mutate(split = "test")
) %>%
  count(split, country, dropout) %>%
  group_by(split, country) %>%
  mutate(rate = n / sum(n))

#Variables we are using 
logit_add <- glm(
  dropout ~
    zhfa + bmi +      # nutrition/ health/ nutrition 
    math +                  # cognition
    wi + hhsize +        # household SES/size
    sex + country,    # demographics + site + country FE
  data = train,
  family = binomial(link = "logit")
)

summary(logit_add)


#Block Models 

#Nutrition only 
logit_m1 <- glm(
  dropout ~ zhfa + zwfa + zbfa + bmi,
  data = train,
  family = binomial
)

summary(logit_m1)

#Nutrition measures on their own do not strongly predict dropout in the short run.
#Nutrition affects schooling indirectly through learning 

#Nutrition + cognition 
logit_m2 <- glm(
  dropout ~ zhfa + zwfa + zbfa + bmi +
    ppvt + math,
  data = train,
  family = binomial
)

summary(logit_m2)




# add household socioeconomics states 
logit_m3 <- glm(
  dropout ~ zhfa + zwfa + zbfa + bmi +
    ppvt + math +
    wi + hhsize,
  data = train,
  family = binomial
)

summary(logit_m3)


#logit on all the data 

# Full analysis sample (at-risk children with defined dropout)
full <- analysis_df

logit_full <- glm(
  dropout ~ zhfa + bmi + math + wi + hhsize + sex + country,
  data = full,
  family = binomial(link = "logit")
)

summary(logit_full)



cor.test(
  analysis_df$bmi,
  analysis_df$math,
  use = "complete.obs"
)


#OLS regression of Math and BMI controlling for hh and wi
lm_math <- lm(
  math ~ bmi + sex + country + hhsize + wi,
  data = analysis_df
)

summary(lm_math)







