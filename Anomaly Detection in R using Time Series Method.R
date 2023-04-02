# Completing install packages
install.packages("tidyverse")
install.packages("anomalize")
install.packages("tibbletime")
install.packages("timetk")
#Calling libraries whose packages are installed
library(tidyverse)
library(tibbletime)
library(anomalize)
library(timetk)
library(readxl)
# Calling the variable and store all the data set in it.
climatedata <- read.csv("Desktop/Anomaly Detection/climatedata.csv")
View(climatedata)
hist(climatedata$humidity,
     xlab = "Humidity",
     main = "Histogram of Humidity Record",
     breaks = sqrt(nrow(climatedata))
)
ggplot(climatedata) +
  aes(x = humidity) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

boxplot(climatedata$humidity,
        ylab = "Humidity"
)
boxplot.stats(climatedata$humidity)$out
out <- boxplot.stats(climatedata$humidity)$out
out_ind <- which(climatedata$humidity %in% c(out))
out_ind

climatedata[out_ind, ]


boxplot(climatedata$humidity,
        ylab = "humidity",
        main = "Boxplot of Humidity Record"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

lower_bound <- quantile(climatedata$humidity, 0.025)
lower_bound

upper_bound <- quantile(climatedata$humidity, 0.975)
upper_bound

outlier_ind <- which(climatedata$humidity < lower_bound | climatedata$humidity > upper_bound)
outlier_ind

climatedata[outlier_ind, "humidity"]

climatedata[outlier_ind, ]


# Setting the percentiles to 1 and 99 provdes the similier outliers like IQR criterion.

lower_bound <- quantile(climatedata$humidity, 0.01)
upper_bound <- quantile(climatedata$humidity, 0.99)

outlier_ind <- which(climatedata$humidity < lower_bound | climatedata$humidity > upper_bound)

climatedata[outlier_ind, ]

subset(climatedata, humidity != 501.6)


iqr <- IQR(climatedata$humidity)
up <-  quantile(climatedata$humidity, 0.75) + 1.5*iqr # Upper Range  
low<- quantile(climatedata$humidity, 0.25) - 1.5*iqr # Lower Range???

#Eliminating undesired data 
eliminated <- subset(climatedata, climatedata$humidity > low & climatedata$humidity < up)
eliminated

#Converting the data format 
str(climatedata)
#Storing the data into a new variable
df <- climatedata
# Changing Factor to Date format
df$date <- paste(df$date, "01", sep="-")
# Selecting only relevant columns to convert in a new data frame
df$date <- as.Date(df$date,format="%Y-%m-%d")#In month column format of Date change
df <- df %>% select(date,humidity)
# Converting df to a tibble
df <- as_tibble(df)
class(df)
df_anomalized <- df %>%
  time_decompose(humidity, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized %>% glimpse()
df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)
plot1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")
plot1


