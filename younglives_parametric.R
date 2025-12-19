# young lives 

library(tidyverse)
library(janitor)

df <- read_csv("/Users/MehriaSaadatKhan/Desktop/Intro to Data science/younglives/young_lives_dropout_sensitivity.csv")

df <- df %>% clean_names() 
# to make sure the names are all okay to work, no issues of capital etc 


dim(df)          # number of rows and columns
glimpse(df)      # variable types + preview
names(df)[1:40]  # first 40 column names

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


#confirm if they are identical or different rows 
df %>%
  filter(childid == "PE121021", country == "Peru") %>%
  select(childid, country, enrolled, dropout, sex, hhsize, region, typesite, math) %>%
  distinct() %>%
  print(n = 50)

#collapse
df_child <- df %>%
  arrange(country, childid) %>%
  distinct(country, childid, .keep_all = TRUE)

#check if it worked, we should get zero rows 
df_child %>%
  count(country, childid) %>%
  filter(n > 1)

#Define the analysis population (at-risk children) 

#since only children enrolled can drop out we define population= children who are enrolled 

analysis_df <- df_child %>%
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


library(tidymodels)

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
    zhfa + zwfa + zbfa + bmi +      # nutrition / anthropometrics
    ppvt + math +                  # cognition
    wi + hhsize +        # household SES/size
    sex + typesite + country,      # demographics + site + country FE
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


#When we only use health/nutrition measures (like height-for-age and BMI), they don’t do a good job predicting who drops out.
#But when we add children’s learning measures, math score becomes the clear strongest predictor: kids with lower math scores are much more likely to drop out.
#When we also add household factors like wealth and household size, the results don’t really change—math still matters most.
#Overall, this suggests that weak learning (especially in math) is the most direct warning sign for dropping out soon. Health and household disadvantage may still matter, but mainly because they affect how well children learn.


# Predictive Probablity Modeling

#create a prediction dataset from the variable that is most informative
# the following will create a hypothetical children who differ only in math scores
math_seq <- seq(
  from = min(train$math, na.rm = TRUE),
  to   = max(train$math, na.rm = TRUE),
  length.out = 50
)

pred_df <- tibble(
  math = math_seq,
  zhfa = mean(train$zhfa, na.rm = TRUE),
  zwfa = mean(train$zwfa, na.rm = TRUE),
  zbfa = mean(train$zbfa, na.rm = TRUE),
  bmi  = mean(train$bmi,  na.rm = TRUE),
  ppvt = mean(train$ppvt, na.rm = TRUE),
  wi   = mean(train$wi,   na.rm = TRUE),
  hhsize = mean(train$hhsize, na.rm = TRUE),
  sex = levels(train$sex)[1],
  typesite = levels(train$typesite)[1],
  country = levels(train$country)[1]
)

# predict dropout probability 
pred_df$pred_p <- predict(
  logit_m3,
  newdata = pred_df,
  type = "response"
)

library(ggplot2)

ggplot(pred_df, aes(x = math, y = pred_p)) +
  geom_line() +
  labs(
    x = "Math score (Round 3)",
    y = "Predicted probability of dropout",
    title = "Predicted dropout risk by baseline math achievement"
  ) +
  theme_minimal()



