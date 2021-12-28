# Generate a fake dataset to use for the examples

# dataset <- data.frame(id = 1:500)
# 
# dataset$gender <- sample(c('Male', 'Female'), 500, replace=TRUE, prob=c(0.55,0.45))
# 
# round(prop.table(table(dataset$gender)),2)
# dataset$educ <- sample(c('Less than HS', 
#                          'HS Grad',
#                          'Some College',
#                          'College Grad',
#                          'Grad School'), 500, 
#                        replace=TRUE,
#                        prob=c(0.05,
#                               0.25,
#                               0.35,
#                               0.30,
#                               0.05))
# round(prop.table(table(dataset$educ)),2)
# 
# dataset$age <- sample(c('18-19',
#                        '20-29',
#                        '30-39',
#                        '40-49',
#                        '50-59',
#                        '60+'), 500,
#                      replace = TRUE,
#                      prob=c(0.02,
#                             0.07,
#                             0.30,
#                             0.35,
#                             0.20,
#                             0.06))
# round(prop.table(table(dataset$age)),2)


#NOTE: THIS EXAMPLE CORRESPONDS TO WORKSHOP PPT FROM 9/30/2015

#############################################################

# Install this package below the first time you run this script.
# After installing it the first time, you won't need to reinstall every time.

#############################################################

install.packages("survey")

#############################################################

# Load the package (run every time)

#############################################################

library(survey)

#############################################################

# Set your working directory

#############################################################

setwd('C:\\data\\')

#############################################################

# Load your data in the working directory

#############################################################

d <- read.csv('example data.csv')

#############################################################

# Recode Sample Data

#############################################################

table(d$gender)
#No recoding of gender necessary


table(d$educ)
#Recode education

d$educ2 <- as.character(d$educ)
d$educ2[d$educ == "Less than HS"] <- "Not College Grad"
d$educ2[d$educ == "HS Grad"] <- "Not College Grad"
d$educ2[d$educ == "Some College"] <- "Not College Grad"
d$educ2[d$educ == "College Grad"] <- "College Grad"
d$educ2[d$educ == "Grad School"] <- "College Grad"
d$educ2 <- factor(d$educ2,
                  levels = c('Not College Grad',
                             'College Grad'))

table(d$educ2)


table(d$age)
#recode age

d$age2 <- as.character(d$age)
d$age2[d$age == "18-19"] <- "Under 30"
d$age2[d$age == "20-29"] <- "Under 30"
d$age2[d$age == "30-39"] <- "30-49"
d$age2[d$age == "40-49"] <- "30-49"
d$age2[d$age == "50-59"] <- "50+"
d$age2[d$age == "60+"] <- "50+"
d$age2 <- factor(d$age2,
                 levels = c('Under 30',
                            '30-49',
                            '50+'))
prop.table(table(d$age2))


#############################################################

# Population counts

#############################################################

# Let's say we want to weight by gender, education, and age. Assume sample size is 411.
# Multiply the pop % by the sample size to get the counts you see below.
# For example, let's say 24% of the pop lives in the Northeast...411*0.24 = 99 (rounded).

pop.gender <- data.frame(gender=c('Female',
                                  'Male'),
                         freq=c(250,
                                250))

pop.educ <- data.frame(educ2=c('Not College Grad',
                              'College Grad'),
                       freq=c(350,
                              150))

pop.age <- data.frame(age2=c('Under 30',
                            '30-49',
                            '50+'),
                      freq=c(100,
                             225,
                             175))


#############################################################

# Create survey design object with no fancy sampling options.

#############################################################

d.svy <- svydesign(~0, data=d)

#############################################################

# Rake to get weights.

#############################################################

# "gender","educ", and "age" are where your survey data variable names should go.
# "pop.gender", "pop.educ", and "pop.age" are the population counts you created above.
# Replace variable names as your data necessitates

d.svy2 <- rake(d.svy,list(~gender, ~educ2, ~age2),
               list(pop.gender, pop.educ, pop.age))

wgts <- weights(d.svy2)

#############################################################

# Append weights to your data

#############################################################

df <- cbind(d,wgts)

#############################################################

# Check for extreme weights and make sure average weight = 1

#############################################################

summary(df$wgts)

#############################################################

# Output data

#############################################################

# change name of file below if you want

write.table(df,
            'weighted data.csv',
            sep = ",",
            row.names = F,
            col.names = T)

