library(marinecs100b)


# Review: write a function ------------------------------------------------

# P1 Describe succinctly what the following code does. This should be a
# high-level, one-sentence description, not a line-by-line breakdown.


# This code defines the number of hours per day the temperature is less than or
# equal to -4 degrees celsius during a month in late winter in Nuka Pass.



site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day




# P2 Let's turn that code chunk into a function. What would you call that
# function? How many parameters should it take and what would you call them?

# function_name <- hours_cold_per_day
# parameters <- site, season


# P3 Write a function to encapsulate the code chunk above. Check that it
# contains all five parts of a function.


hours_cold_per_day <- function(site, season) {
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold_function <- n_cold * 30 / 60
  days_total_function <- n_total * 30 / 60 / 24
  hours_cold_per_day_function <- hours_cold_function / days_total_function
  return(hours_cold_per_day_function)
}

hours_cold_per_day("Harris", "Spring")

?kefj_season
?kefj_site

# Make an extreme choice --------------------------------------------------

# P4 Fill in the code below to create a logical vector indicating extreme
# temperatures.

extreme_type <- "cold"
extreme_type <- "hot"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == "hot") {
  is_extreme <- kefj_temperature >= 25
}

is_extreme

# P5 Copy-paste the code from P1 and edit it to incorporate the is_extreme
# vector into the extreme temperature exposure procedure.

n_extreme <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_extreme <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_extreme_per_day <- hours_extreme / days_total
hours_extreme_per_day



# P6 Copy-paste the function you wrote in P3 and edit it to add a parameter that
# lets you switch between extreme heat and cold exposure.
extremehours_perday <- function(site, season, extreme_type) {
  if (extreme_type == "cold") {
    is_extreme <- kefj_temperature <= -4
  } else if (extreme_type == "hot") {
    is_extreme <- kefj_temperature >= 25
  }
  n_extreme_function <- sum(kefj_site == site &
                  kefj_season == season &
                  is_extreme &
                  kefj_exposure == "air")
  n_total_function <- sum(kefj_site == site &
                   kefj_season == season)
  hours_extreme <- n_extreme_function * 30 / 60
  days_total <- n_total_function * 30 / 60 / 24
  hours_extreme_per_day_function <- hours_extreme / days_total
  return(hours_extreme_per_day_function)
}

extremehours_perday("Nuka_Pass", "Late winter", "cold")


# Season to taste ---------------------------------------------------------

# P7 What seasons are in the kefj dataset? What function would you use to
# identify them?
?kefj_season
# Late winter, Spring, Summer, Fall, Early winter

# P8 Fill in the blanks below to make a for loop that prints the extreme hot and
# cold exposure across seasons at site Aialik.

seasons <- c("Late winter", "Spring", "Summer", "Fall", "Early winter")
  for (i in 1: length(seasons)) {
    heat_exposure <- extremehours_perday(site, seasons[i], "hot")
    cold_exposure <- extremehours_perday(site, seasons[i], "cold")
    print(paste("Aialik", seasons[i], heat_exposure, cold_exposure))
}

# P9 Copy-paste your answer to P8 and add a nested for loop to iterate across
# sites as well as seasons.

seasons <- c("Late winter", "Spring", "Summer", "Fall", "Early winter")
site <- c("Aialik", "Nuka_Pass", "Harris", "McCarty", "Nuka_Bay")
for (i in 1: length(seasons)) {
  for (j in 1:length(site))
  heat_exposure <- extremehours_perday(site[j], seasons[i], "hot")
  cold_exposure <- extremehours_perday(site[j], seasons[i], "cold")
  print(paste(site[j], seasons[i], heat_exposure, cold_exposure))
}


# P10 Examine your results from P9. You should find two outputs where both
# extreme heat and cold exposure were 0. What season were they in?

# Nuka_Bay Fall
# Harris Fall



