# Loading  dataset
protests <- read.csv("Iran Protests Dataset (Ver 2.21.23).csv")

# look first few rows
head(protests)

# Check structure (column names and type)
str(protests)

# make sure date is treated as an actual date not  text
protests$Date <- as.Date(protests$Date, format = "%m/%d/%Y")

# sort by date  to be safer
protests <- protests[order(protests$Date), ]

# Turn cumulative columns into daily new counts
protests$daily_deaths <- c(protests$Death.Toll.of.Protestors[1],
                           diff(protests$Death.Toll.of.Protestors))

protests$daily_arrests <- c(protests$Number.of.Individuals.Arrested[1],
                            diff(protests$Number.of.Individuals.Arrested))

# Check it worked
head(protests[, c("Date", "Death.Toll.of.Protestors", "daily_deaths",
                  "Number.of.Individuals.Arrested", "daily_arrests")])

# summary() will gives  min, max and other quick stats
summary(protests$daily_deaths)
summary(protests$daily_arrests)

#sd() = standard deviation, tell how "spread out" the numbers are
## a big standard deviation means the daily numbers bouncing around lot
sd(protests$daily_deaths)
sd(protests$daily_arrests)

# histogram show  how daily death numbers is distributed 
# are most days low with a few very bad days or is it evenly spread?
hist(protests$daily_deaths,
     main = "Distribution of Daily Protester Deaths",
     xlab = "Deaths per day",
     col = "lightpink")


# were labeling each row as either "early" (first 30 days of the protests)
# or "later" (everything after that).
# this is for being able tocompare the two groups

protests$period <- ifelse(protests$Date <= min(protests$Date) + 29,
                          "early_30_days", "later")

# check: how many days are in each one ofgroup?
table(protests$period)

# Split  daily death to 2 separate lists
early_deaths <- protests$daily_deaths[protests$period == "early_30_days"]
later_deaths <- protests$daily_deaths[protests$period == "later"]

boxplot(daily_deaths ~ period, data = protests,
        main = "Daily Deaths: First 30 Days vs. Later",
        xlab = "Period", ylab = "Deaths per day",
        col = c("tomato", "lightgray"))

# shapiro.test() check if a set of numbers looks "normal" (bell curve).
shapiro.test(early_deaths)
shapiro.test(later_deaths)

#  shapiro.test() unfortunately showed both of the groups are NOT normally distributed
# a just standard t-test would not be valid in this scenario
# We use the Wilcoxon rank-sum test instead 
wilcox.test(early_deaths, later_deaths, alternative = "greater")


# same cumulative to aily fix like before now just for cities and universities
protests$daily_cities <- c(protests$Number.of.Cities.Involved[1],
                           diff(protests$Number.of.Cities.Involved))

protests$daily_universities <- c(protests$Number.of.Universities.Involved[1],
                                 diff(protests$Number.of.Universities.Involved))

# quick check
head(protests[, c("Date", "daily_cities", "daily_universities")])

#check if our dailycities and dailyuniversities are normally distributed or they are not
shapiro.test(protests$daily_cities)
shapiro.test(protests$daily_universities)

# Spearman correlation because  data isn't normally distributed.
# we are checking : when the number of cities involved in protest goes up,
# does the  universities number that were involved goes up too?
cor.test(protests$daily_cities, protests$daily_universities, method = "spearman")


# samme cumulative todaily fix like the before now for number.of.Protests
protests$daily_protests <- c(protests$Number.of.Protests[1],
                             diff(protests$Number.of.Protests))

#quick check
head(protests[, c("Date", "daily_protests", "daily_arrests")])

# now check if daily_protests and daily_arrests are normally distributed or they arent
shapiro.test(protests$daily_protests)
shapiro.test(protests$daily_arrests)

# Spearman correlation because neither variable is normally distributed
# checking : on days with more protests do arrests get higher too?
cor.test(protests$daily_protests, protests$daily_arrests, method = "spearman")


# i want to look  the TOTAL deaths by the end of the dataset (the last row
# has the final cumulative totals, since these columns are running totals)

total_protester_deaths <- max(protests$Death.Toll.of.Protestors)
total_security_deaths <- max(protests$Number.of.Military.Security.Personnel.Killed)

# just print  out so i can see the raw numbers
total_protester_deaths
total_security_deaths

#  lets calculate the ratio: for every 1 security force member killed,
# how many protesters were killed?
ratio <- total_protester_deaths / total_security_deaths

barplot(c(total_protester_deaths, total_security_deaths),
        names.arg = c("Protesters", "Security Forces"),
        main = "Total Deaths by Group",
        ylab = "Number Killed",
        col = c("firebrick", "steelblue"))
ratio




plot(protests$Date, protests$daily_deaths, type = "l", 
     main = "Daily Protester Deaths Over Time", 
     xlab = "Date", ylab = "Deaths per day", 
     col = "firebrick", lwd = 2)




plot(protests$daily_cities, protests$daily_universities, 
     main = "Cities Involved vs. Universities Involved (Daily)", 
     xlab = "New cities involved that day", 
     ylab = "New universities involved that day", 
     pch = 19, col = "darkred")



plot(protests$daily_protests, protests$daily_arrests, 
     main = "Protests vs. Arrests (Daily)", 
     xlab = "New protests that day", 
     ylab = "New arrests that day", 
     pch = 19, col = "darkblue")
