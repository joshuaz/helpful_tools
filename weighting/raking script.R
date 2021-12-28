
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

d <- read.csv('unweighted data.csv')

#############################################################

# Population counts

#############################################################

# Let's say we want to weight by region and age. Assume sample size is 411.
# Multiply the pop % by the sample size to get the counts you see below.
# For example, let's say 24% of the pop lives in the Northeast...411*0.24 = 99 (rounded).

pop.region <- data.frame(region=c('Northeast',
                                  'Midwest',
                                  'South',
                                  'West'), 
                         freq=c(99,
                                106,
                                123,
                                83))


pop.age <- data.frame(enroll=c('Under 30',
                               '30 to 59',
                               '60+'),
                      freq=c(179,
                             130,
                             102))

#############################################################

# Create survey design object with no fancy sampling options.

#############################################################

d.svy <- svydesign(~0, data=d)

#############################################################

# Rake to get weights.

#############################################################

# "region" and "age" are where your survey data variable names should go.
# "pop.region" and "pop.age" are the population counts you created above.
# replace "region","age","pop.region", and "pop.age" below with your variable names.

d.svy2 <- rake(d.svy,list(~region, ~age),
               list(pop.region, pop.age))

wgts <- weights(d.svy2)

#############################################################

# Append weights to your data

#############################################################

df <- cbind(d,wgts)

#############################################################

# Output data

#############################################################

# change name of file below if you want

write.table(df,
            'weighted data.csv',
            sep = ",",
            row.names = F,
            col.names = T)

