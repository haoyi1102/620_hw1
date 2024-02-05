rm(list = ls())
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

# haoyi's data
setwd("C:/Users/24468/Desktop/620/hw1")
ST00_hy = read_excel(
  path = "ScreenTime_haoyi.xlsx",
  col_types = c("date", "text", "numeric", "text", "numeric", "numeric", "date")
)
ST00_hy_tempt_first_pick_up = ST00_hy$Pickup.1st
# correct the errorenous in the data
ST00_hy = ST00_hy %>%
  mutate(Pickup.1st = as.POSIXct(paste(as.character(Date), unlist(
    lapply(Pickup.1st, function(x) {
      strsplit(as.character(x), split = "_")[[1]][2]
    })
  ))))

hm_to_min = function(hm) {
  unlist(lapply(hm, function(x) {
    splt = strsplit(x, "h")[[1]]
    hr = as.numeric(splt[1])
    mn = as.numeric(strsplit(splt[2], "m")[[1]][1])
    return(60 * hr + mn)
  }))
}

ST00_hy = ST00_hy %>%
  mutate(
    Total.ST.min.true = hm_to_min(Total.ST),
    Social.ST.min.true = hm_to_min(Social.ST),
    Total.ST.match = Total.ST.min.true == Total.ST.min,
    Social.ST.match = Social.ST.min.true == Social.ST.min
  ) %>%
  relocate(
    Date,
    Total.ST,
    Total.ST.min,
    Total.ST.min.true,
    Total.ST.match,
    Social.ST,
    Social.ST.min,
    Social.ST.min.true,
    Social.ST.match
  )

ST00_hy

ST00_hy = ST00_hy %>%
  mutate(
    Daily_Prop_Social_ST = Social.ST.min.true / Total.ST.min.true,
    Daily_Duration_Per_Use = Total.ST.min.true / Pickups
  )
colnames(ST00_hy)

Sys.setlocale("LC_TIME", "C")
ggplot(ST00_hy, aes(x = Date, y = Total.ST.min.true)) +
  geom_line() +
  labs(title = "Daily Total Screen Time", x = "Date", y = "Screen Time (min)")

ggplot(ST00_hy, aes(x = Date, y = Social.ST.min.true)) +
  geom_line() +
  labs(title = "Daily Total Social Screen Time", x = "Date", y = "Social Screen Time (min)")

ggplot(ST00_hy, aes(x = Date, y = Pickups)) +
  geom_line() +
  labs(title = "Daily Number of Pickups", x = "Date", y = "Number of Pickups")

ggplot(ST00_hy, aes(x = Date, y = Daily_Prop_Social_ST)) +
  geom_line() +
  labs(title = "Daily Proportion of Social Screen Time", x = "Date", y = "Proportion")

ggplot(ST00_hy, aes(x = Date, y = Daily_Duration_Per_Use)) +
  geom_line() +
  labs(title = "Daily Duration Per Use", x = "Date", y = "Duration Per Use (min)")

library(GGally)

ggpairs(ST00_hy[, c(
  "Total.ST.min.true",
  "Social.ST.min.true",
  "Pickups",
  "Daily_Prop_Social_ST",
  "Daily_Duration_Per_Use"
)])

cor_matrix <-
  cor(ST00_hy[, c(
    "Total.ST.min.true",
    "Social.ST.min.true",
    "Pickups",
    "Daily_Prop_Social_ST",
    "Daily_Duration_Per_Use"
  )])

max_cor <- max(cor_matrix[lower.tri(cor_matrix)])
max_cor_pair <- which(cor_matrix == max_cor, arr.ind = TRUE)
highest_correlated_pair <-
  names(ST00_hy)[c(max_cor_pair[1, 1], max_cor_pair[1, 2])]
(highest_correlated_pair)

library(ggplot2)

ggplot(ST00_hy, aes(x = Total.ST.min.true)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF of Total Screen Time", x = "Total Screen Time (min)", y = "Proportion")

ggplot(ST00_hy, aes(x = Social.ST.min.true)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF of Social Screen Time", x = "Social Screen Time (min)", y = "Proportion")

ggplot(ST00_hy, aes(x = Pickups)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF of Number of Pickups", x = "Number of Pickups", y = "Proportion")

ggplot(ST00_hy, aes(x = Daily_Prop_Social_ST)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF of Daily Proportion of Social Screen Time", x = "Proportion of Social Screen Time", y = "Proportion")

ggplot(ST00_hy, aes(x = Daily_Duration_Per_Use)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF of Daily Duration Per Use", x = "Duration Per Use (min)", y = "Proportion")

acf(ST00_hy$Total.ST.min.true, plot = FALSE)
acf(ST00_hy$Social.ST.min.true, plot = FALSE)
acf(ST00_hy$Pickups, plot = FALSE)
acf(ST00_hy$Daily_Prop_Social_ST, plot = FALSE)
acf(ST00_hy$Daily_Duration_Per_Use, plot = FALSE)

ST00_hy$Pickup.1st = ST00_hy_tempt_first_pick_up
ST00_hy = ST00_hy %>%
  mutate(Pickup.1st.angular = (hour(Pickup.1st) * 60 + minute(Pickup.1st)) /
           (24 * 60) * 360)
head (ST00_hy$Pickup.1st.angular)

library(circular)
first.pickup.cir = circular(ST00_hy$Pickup.1st.angular,
                            units = "degrees",
                            template = "clock24")
plot(first.pickup.cir, col = "blue")

plot(
  first.pickup.cir,
  stack = TRUE,
  bins = 288,
  col = "blue"
)

model <-
  glm(Pickups ~ offset(log(Total.ST.min.true / 60)), family = poisson, data = ST00_hy)
summary(model)

library(lubridate)
ST00_hy$Xt <- ifelse(wday(ST00_hy$Date) %in% 2:6, 1, 0)

ST00_hy$Zt <- ifelse(ST00_hy$Date >= as.Date("2023-01-10"), 1, 0)

model <-
  glm(Pickups ~ Xt + Zt + offset(log(Total.ST.min.true / 60)),
      family = poisson,
      data = ST00_hy)

summary(model)

estimates <- mle.vonmises(ST00_hy$Pickup.1st.angular)
print(estimates)

time_rad <- (8.5 / 24) * 2 * pi

cdf_830 <-
  pvonmises(time_rad, mu = estimates$mu, kappa = estimates$kappa)

prob_830_or_later <- 1 - cdf_830

print(prob_830_or_later)
