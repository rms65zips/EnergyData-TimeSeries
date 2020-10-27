# R-Pipeline-Filtering-TS-Student-2019

# Name: Mick Shaw
# Last updated:


###############
# Project notes
###############

# Summarize project


###############
# Housekeeping
###############

# Clear all variables from R
rm(list = ls())

# Set working directory
getwd()
setwd()
dir()


################################
## Install and load packages
################################

install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("forecast")
install.packages("TTR")
install.packages("RMySQL")
install.packages("plotly")
install.packages("ggfortify")
install.packages("ggplot2")
install.packages("reshape")
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(tidyr)
library(lubridate) # work with dates
library(forecast)
library(TTR)
library(RMySQL)
library(plotly)
library(ggfortify)
library(ggplot2)
library(reshape)



###############
# Load dataset 
###############

# Load using sql
## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)
## Lists attributes contained in a table
# dbListFields(con,'iris')
# ## Use asterisk to specify all attributes for download
# irisALL <- dbGetQuery(con, "SELECT * FROM iris")
# colnames(irisALL)
# ## Use attribute names to specify specific attributes for download
# irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
# colnames(irisSELECT)


## Energy data
dbListFields(con,'yr_2007')

## Exclude 2006 - mostly incomplete
## ** Include 2010 since it's mostly complete even though POA states to exclude 
##    all years that are incomplete
energy2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
energy2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
energy2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
energy2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

str(energy2007)

energy <- bind_rows(energy2007, energy2008, energy2009, energy2010)
# Confirm
head(energy)
tail(energy)
class(energy)
# Remove ID
energy$id <- NULL
str(energy)

##################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- energy %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "Europe/Paris")

str(hhpwrDT)
class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) # "Europe/Paris"

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%Y-%m-%d")
str(hhpwrDT)


##------- Change data types---------##

# Be sure to use as.numeric(as.character()), if needed, or may end up with index value I/O actual
# dataset$feature <- as.numeric(as.character(dataset$feature))


## ------ Evaluate NA values ----------##

# Are there any NAs in df?
any(is.na(hhpwrDT)) 
# Use summary to identify where NAs are 
summary(hhpwrDT)

## -------- Save pre-processed dataset --------##

# Save file (export to other programs), or
# Save object (for R only)


#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): add column with desired time interval (e.g., Yr/Mo/Day) using lubridate 
# 2. dplyr::filter(): select cols to filter by; full ds + added col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select which time intervals to subset and their order 
# 4. dplyr::summarize(): select the vars and any calculations for these vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 


#############
## Subsets
#############

########TASK 1#############
###########################
# 1) Annual subset
###########################

Yr.sum <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009) %>%
  group_by(Year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1/1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2/1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3/1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance
Yr.sum

# A tibble: 4 x 5
# Year   SM1   SM2   SM3 DateTime           
# <dbl> <dbl> <dbl> <dbl> <dttm>             
# 1  2007  643.  854. 3023. 2007-01-01 00:00:00
# 2  2008  585.  662. 3178. 2008-01-01 00:00:00
# 3  2009  593.  592. 3557. 2009-01-01 00:00:00

?plot_ly

# Plot sub-meter 1, 2 and 3 with title, legend and labels - Year frequency
plot_ly(Yr.sum, x = ~Yr.sum$Year, y = ~Yr.sum$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~Yr.sum$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~Yr.sum$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Annual consumption (kWh) 2007~2009",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (kWh)"))  

plot(hhpwrDT$Sub_metering_1)
#--- Create average annual dataset ---#

Yr.mean <- Yr.sum %>%
  summarize(SM1 = mean(SM1), 
            SM2 = mean(SM2), 
            SM3 = mean(SM3),
            DateTime = first(DateTime))   # To verify date of first instance
Yr.mean


plot_ly(Yr.mean, x ="", y = ~Yr.mean$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~Yr.mean$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~Yr.mean$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Annual Consumption: 2007~2009",
         xaxis = list(title = "Submeter"),
         yaxis = list (title = "Power (kWh)"))  


################
##Monthly Subset
################

month.sum <- hhpwrDT%>%
  mutate(Year = year(DateTime), Month = month(DateTime))%>%
  filter(Date >= "2007-01-01" & Date <= "2010-10-31") %>%
  group_by(Year,Month)%>%
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>%
  filter(!is.na(Month))
month.sum

# plot_ly(month.sum, x = ~month.sum$DateTime, y = ~month.sum$SM1, 
#         name = 'SM1-Kitchen', type = 'bar', orientation ='v',
#         marker = list(color = 'rgba(246, 78, 139, 0.6)',
#                       line = list(color = 'rgba(246, 78, 139, 1.0)',
#                                   width = 3))) %>%
#   add_trace(y = ~month.sum$SM2, name = 'SM2-Laundry Room') %>%
#   add_trace(y = ~month.sum$SM3, name = 'SM3-Water Heater & AC') %>%
#   layout(title = "Monthly Consumption (kWh) Jan-1 2007 - Oct-31 2010",
#          xaxis = list(title = "Month"),
#          yaxis = list (title = "Power (kWh)")) 


plot_ly(month.sum, x = ~month.sum$DateTime, y = ~month.sum$SM1, 
  name = 'SM1-Kitchen', type = 'scatter', mode ='line') %>%
        add_trace(y = ~month.sum$SM2, name = 'SM2-Laundry Room') %>%
        add_trace(y = ~month.sum$SM3, name = 'SM3-Water Heater & AC') %>%
        layout(title = "Monthly Consumption (kWh) Jan-1 2007 - Oct-31 2010",
                  xaxis = list(title = "Month"),
                  yaxis = list (title = "Power (kWh)")) 

###########################
##Monthy Consumption 2007##
###########################

month.avg.07 <- month.sum %>%
  mutate(Month = lubridate::month(DateTime,1)) %>%  # Add col by adding "1"
  filter(Year==2007) %>%   # Filter after mutate, but before group_by
  group_by(Month) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
month.avg.07

plot_ly(month.avg.07, x = ~month.avg.07$Month, y = ~month.avg.07$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~month.avg.07$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~month.avg.07$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Monthly Consumption (kWh) 2007",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (kWh)")) 

###########################
##Monthy Consumption 2008##
###########################

month.avg.08 <- month.sum %>%
  mutate(Month = lubridate::month(DateTime,1)) %>%  # Add col by adding "1"
  filter(Year==2008) %>%   # Filter after mutate, but before group_by
  group_by(Month) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
month.avg.08

plot_ly(month.avg.08, x = ~month.avg.08$Month, y = ~month.avg.08$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~month.avg.08$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~month.avg.08$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Monthly Consumption (kWh) 2008",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (kWh)"))  

###########################
##Monthy Consumption 2009##
###########################

month.avg.09 <- month.sum %>%
  mutate(Month = lubridate::month(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Year==2009) %>%   # Filter after mutate, but before group_by
  group_by(Month) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
month.avg.09

plot_ly(month.avg.09, x = ~month.avg.09$Month, y = ~month.avg.09$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~month.avg.09$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~month.avg.09$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Monthly Consumption (kWh) 2009",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (kWh)")) 

############################
##Monthly Consumption 2010##
############################

month.avg.10 <- month.sum %>%
  mutate(Month = lubridate::month(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Year==2010) %>%   # Filter after mutate, but before group_by
  group_by(Month) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
month.avg.10

plot_ly(month.avg.10, x = ~month.avg.10$Month, y = ~month.avg.10$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~month.avg.10$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~month.avg.10$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Monthly Consumption (kWh) 2010",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (kWh)")) 


###############################
## Weekday subset
###############################

day.sum <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime)) %>%  
  # Don't add "1" after month (or label) - causes subset class=tibble when subsetting for seasons below 
  filter(Date >= "2007-01-01" & Date <= "2010-11-25") %>%
  group_by(Year, Month, Day) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(Day)) # Remove the last row that has NA 

head(day.sum)


#################
##Winter Subset##
#################


wday.avg.winter <- day.sum %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Month==12 | Month==1 | Month==2) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
wday.avg.winter
any(is.na(wday.avg.winter))  # FALSE


plot_ly(wday.avg.winter, x = ~wday.avg.winter$wDay, y = ~wday.avg.winter$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.winter$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~wday.avg.winter$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Winter Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)"))



#################
##Spring Subset##
#################


wday.avg.spring <- day.sum %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Month==3 | Month==4 | Month==5) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
wday.avg.spring
any(is.na(wday.avg.spring))  # FALSE


plot_ly(wday.avg.spring, x = ~wday.avg.spring$wDay, y = ~wday.avg.spring$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.spring$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~wday.avg.spring$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Spring Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)")) 

#################
##Summer Subset##
#################


wday.avg.summer <- day.sum %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Month==6 | Month==7 | Month==8) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
wday.avg.summer
any(is.na(wday.avg.summer))  # FALSE


plot_ly(wday.avg.summer, x = ~wday.avg.summer$wDay, y = ~wday.avg.summer$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.summer$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~wday.avg.summer$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Summer Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)")) 


#################
##Autumn Subset##
#################

wday.avg.autumn <- day.sum %>%
  mutate(wDay = lubridate::wday(DateTime,1)) %>%  # Add col wDay by adding "1"
  filter(Month==9 | Month==10 | Month==11) %>%   # Filter after mutate, but before group_by
  group_by(wDay) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3), 
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))
wday.avg.autumn
any(is.na(wday.avg.autumn))  # FALSE


plot_ly(wday.avg.autumn, x = ~wday.avg.autumn$wDay, y = ~wday.avg.autumn$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.autumn$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~wday.avg.autumn$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Autumn Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)")) 


########################
###Seasonal SM1 Chart###
########################

seasonalSM1 <- plot_ly(wday.avg.winter, x = ~wday.avg.winter$wDay, y = ~wday.avg.winter$SM1, 
        name = 'Winter-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.spring$SM1, name = 'Spring-Kitchen') %>%
  add_trace(y = ~wday.avg.summer$SM1, name = 'Summer-Kitchen') %>%
  add_trace(y = ~wday.avg.autumn$SM1, name = 'Autumn-Kitchen') %>%
  layout(title = "Seasonal Kitchen Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)"))

seasonalSM1

########################
###Seasonal SM2 Chart###
########################

seasonalSM2 <- plot_ly(wday.avg.winter, x = ~wday.avg.winter$wDay, y = ~wday.avg.winter$SM2, 
        name = 'Winter-Laundry Room', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.spring$SM2, name = 'Spring-Laundry Room') %>%
  add_trace(y = ~wday.avg.summer$SM2, name = 'Summer-Laundry Room') %>%
  add_trace(y = ~wday.avg.autumn$SM2, name = 'Autumn-Laundry Room') %>%
  layout(title = "Seasonal Laundry Room Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)"))

seasonalSM2

########################
###Seasonal SM3 Chart###
########################

seasonalSM3 <- plot_ly(wday.avg.winter, x = ~wday.avg.winter$wDay, y = ~wday.avg.winter$SM3, 
        name = 'Winter-Water Heater|AC', type = 'scatter', mode='lines') %>%
  add_trace(y = ~wday.avg.spring$SM3, name = 'Spring-Water Heater|AC') %>%
  add_trace(y = ~wday.avg.summer$SM3, name = 'Summer-Water Heater|AC') %>%
  add_trace(y = ~wday.avg.autumn$SM3, name = 'Autumn-Water Heater|AC') %>%
  layout(title = "Seasonal Water Heater|AC Consumption (kWh) 2007~2010",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (kWh)"))

seasonalSM3

########################
# 3) Hourly subset
########################


hr.sum <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime), Hour = hour(DateTime)) %>%  
  # Don't add "1" after month (or label) - causes subset class=tibble when subsetting for seasons below 
  filter(Date >= "2007-01-01" & Date <= "2010-11-25") %>%
  group_by(Year, Month, Day, Hour) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1/1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2/1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3/1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(Hour)) # Remove the last row that has NA 
hr.sum
plot(x=hr.sum$DateTime, y=hr.sum$SM1)


hr.avg.jan10 <- hr.sum %>%
  mutate(Hours= lubridate::hour(DateTime)) %>% 
  filter(Year==2010 & Month==1) %>% # Filter after mutate, but before group_by
  group_by(Hours) %>%  
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3),  
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))

hr.avg.jan10
any(is.na(hr.avg.jan10))  # FALSE


plot_ly(hr.avg.jan10, x = ~hr.avg.jan10$Hours, y = ~hr.avg.jan10$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~hr.avg.jan10$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~hr.avg.jan10$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Hourly Consumption (kWh) Jan 2010",
         xaxis = list(title = "Hours in Day"),
         yaxis = list (title = "Power (kWh)"))  


hr.avg.jan08 <- hr.sum %>%
  mutate(Hours= lubridate::hour(DateTime)) %>% 
  filter(Year==2008 & Month==1) %>% # Filter after mutate, but before group_by
  group_by(Hours) %>%  
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3),  
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))

hr.avg.jan08
any(is.na(hr.avg.jan08))  # FALSE


plot_ly(hr.avg.jan08, x = ~hr.avg.jan08$Hours, y = ~hr.avg.jan08$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~hr.avg.jan08$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~hr.avg.jan08$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Hourly Consumption (kWh) Jan 2008",
         xaxis = list(title = "Hours in Day"),
         yaxis = list (title = "Power (kWh)"))  



########END OF TASK 1#############


#########################
# Task 2 Subsets
#########################

plot(hhpwrDT$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek2 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Week==2)
summary(houseWeek2)
houseWeek2

## Plot subset houseWeek
plot(houseWeek2$Sub_metering_1)

########################################

houseDay <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Month==1 & Day==12)
summary(houseDay)
houseDay

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


###############################################################
houseDay9 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Month==1 & Day==9 & (Min == 0 | Min == 10 | Min == 20 | Min == 30 | Min == 40 | Min == 50))
summary(houseDay9)
houseDay9

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay9, x = ~houseDay9$DateTime, y = ~houseDay9$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay9$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay9$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




houseDay10 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Month==1 & Day==10 & (Min == 0 | Min == 10 | Min == 20 | Min == 30 | Min == 40 | Min == 50))
summary(houseDay10)
houseDay10

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 10th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
##############################################################

houseWeek26 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Week==26  & (Min==0 | Min==10 | Min==20 | Min==30 | Min==40 | Min==50))
summary(houseWeek26)
houseWeek26

plot_ly(houseWeek26, x = ~houseWeek26$DateTime, y = ~houseWeek26$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2008",
         xaxis = list(title = "DateTime"),
         yaxis = list (title = "Power (watt-hours)"))


houseWeek26sum <- hr.sum %>%
  mutate(Week = week(DateTime), Hour = hour(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Week==26)
  group_by(Hour)
summary(houseWeek26sum)
houseWeek26sum

plot_ly(houseWeek26sum, x = ~houseWeek26sum$DateTime, y = ~houseWeek26sum$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26sum$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26sum$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2008",
         xaxis = list(title = "DateTime"),
         yaxis = list (title = "Power (kWh)"))

###############################################################


##############################################
###Hourly consumption 2008, January, Day 12###
##############################################

hr.avg.jan12 <- hr.sum %>%
  mutate(Hour = lubridate::hour(DateTime)) %>% 
  filter(Year==2008 & Month==1 & Day==12) %>% # Filter after mutate, but before group_by
  group_by(Hour) %>%  # Group data by wDay
  summarize(SM1 = round(mean(SM1),3), 
            SM2 = round(mean(SM2),3),  
            SM3 = round(mean(SM3),3),
            DateTime = first(DateTime))

hr.avg.jan12
any(is.na(hr.avg.jan12))  # FALSE

plot_ly(hr.avg.jan12, x = ~hr.avg.jan12$Hour, y = ~hr.avg.jan12$SM1, 
        name = 'SM1-Kitchen', type = 'scatter', mode='lines') %>%
  add_trace(y = ~hr.avg.jan12$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~hr.avg.jan12$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Hourly Consumption (kWh) Jan. 12th, 2008",
         xaxis = list(title = "Hours in Day"),
         yaxis = list (title = "Power (kWh)")) 


houseWeek26flt10_08 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Week==26)%>%
  group_by(Day)%>%
  summarise(SM1 = round(sum(Sub_metering_1/1000, na.rm = TRUE),4),
            SM2 = round(sum(Sub_metering_2/1000, na.rm = TRUE),4),
            SM3 = round(sum(Sub_metering_3/1000, na.rm = TRUE),4),
            DateTime = first(DateTime))   # To verify date of first instance

summary(houseWeek26flt10_08)
houseWeek26flt10_08


plot_ly(houseWeek26flt10_08, x = ~houseWeek26flt10_08$DateTime, y = ~houseWeek26flt10_08$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26flt10_08$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26flt10_08$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2008",
         xaxis = list(title = "Day"),
         yaxis = list (title = "Power (kWh)"))


### Multiple Variable Plots Using ggplot2 ###
ggwk26_08 <- ggplot(houseWeek26flt10_08, aes(DateTime))+
  geom_line(aes(y=SM1, colour="Kitchen"))+geom_point(aes(y=SM1))+
  geom_line(aes(y=SM2, colour="Laundry Room"))+geom_point(aes(y=SM2))+
  geom_line(aes(y=SM3, colour="Water Heater & AC"))+geom_point(aes(y=SM3))+
  labs(x="Date", y="Power (kWh)", title = "Power Consumption Week 26 - 2008")+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  theme(plot.title = element_text(face="bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        plot.background=element_rect(fill="cyan3"),
        legend.key=element_rect(fill='white'),
        legend.text=element_text(face='bold'),
        legend.title = element_blank())
ggwk26_08



### Week 26 of 2009
houseWeek26flt10_09 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2009 & Week==26)%>%
  group_by(Date)%>%
  summarise(SM1 = round(mean(Sub_metering_1/1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(mean(Sub_metering_2/1000, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3/1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

summary(houseWeek26flt10_09)
houseWeek26flt10_09

plot_ly(houseWeek26flt10_09, x = ~houseWeek26flt10_09$DateTime, y = ~houseWeek26flt10_09$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26flt10_09$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26flt10_09$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2009",
         xaxis = list(title = "Day"),
         yaxis = list (title = "Power (kWh)"))


##ggplot2##
# ggwk26_09 <- ggplot(houseWeek26flt10_09, aes(DateTime))+
#   geom_line(aes(y=SM1, colour="Kitchen"))+geom_point(aes(y=SM1))+
#   geom_line(aes(y=SM2, colour="Laundry Room"))+geom_point(aes(y=SM2))+
#   geom_line(aes(y=SM3, colour="Water Heater & AC"))+geom_point(aes(y=SM3))+
#   labs(x="Date", y="Power (W-Hrs) - Avg. Every 10 Min", title = "Power Consumption Week 26 - 2009")+
#   guides(colour = guide_legend(override.aes = list(size=2)))+
#   theme(plot.title = element_text(face="bold"),
#         axis.title.x = element_text(face = "bold"),
#         axis.title.y = element_text(face = "bold"),
#         axis.text.x = element_text(colour = "black"),
#         axis.text.y = element_text(colour = "black"),
#         plot.background=element_rect(fill="cyan3"),
#         legend.key=element_rect(fill='white'),
#         legend.text=element_text(face='bold'),
#         legend.title = element_blank())
# ggwk26_09

###################
### Week 26 of 2010
###################

houseWeek26flt10_10 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2010 & Week==26 & (Min==0 | Min==10 | Min==20 | Min==30 | Min==40 | Min==50))%>%
  group_by(Date)%>%
  summarise(SM1 = round(mean(Sub_metering_1, na.rm = TRUE),3), 
            SM2 = round(mean(Sub_metering_2, na.rm = TRUE),3), 
            SM3 = round(mean(Sub_metering_3, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

summary(houseWeek26flt10_10)
houseWeek26flt10_10

plot_ly(houseWeek26flt10_10, x = ~houseWeek26flt10_10$DateTime, y = ~houseWeek26flt10_10$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26flt10_10$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26flt10_10$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2010",
         xaxis = list(title = "Day"),
         yaxis = list (title = "Power (W-hrs) - Avg. Every 10 Min"))


##ggplot2##
# ggwk26_10 <- ggplot(houseWeek26flt10_10, aes(DateTime))+
#   geom_line(aes(y=SM1, colour="Kitchen"))+geom_point(aes(y=SM1))+
#   geom_line(aes(y=SM2, colour="Laundry Room"))+geom_point(aes(y=SM2))+
#   geom_line(aes(y=SM3, colour="Water Heater & AC"))+geom_point(aes(y=SM3))+
#   labs(x="Date", y="Power (W-Hrs) - Avg. Every 10 Min", title = "Power Consumption Week 26 - 2009")+
#   guides(colour = guide_legend(override.aes = list(size=2)))+
#   theme(plot.title = element_text(face="bold"),
#         axis.title.x = element_text(face = "bold"),
#         axis.title.y = element_text(face = "bold"),
#         axis.text.x = element_text(colour = "black"),
#         axis.text.y = element_text(colour = "black"),
#         plot.background=element_rect(fill="cyan3"),
#         legend.key=element_rect(fill='white'),
#         legend.text=element_text(face='bold'),
#         legend.title = element_blank())
# ggwk26_10



############################
### Week 26 of 2008 - Monday
############################

houseWeek26Day2flt10_08 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Wday = wday(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Week==26 & Wday==2 & (Min==0 | Min==10 | Min==20 | Min==30 | Min==40 | Min==50))

summary(houseWeek26Day2flt10_08)
houseWeek26Day2flt10_08


plot_ly(houseWeek26Day2flt10_08, x = ~houseWeek26Day2flt10_08$DateTime, y = ~houseWeek26Day2flt10_08$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26Day2flt10_08$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26Day2flt10_08$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Watt-hrs)"))

##ggplot2##
# ggwk26Mon_10 <- ggplot(houseWeek26Day1flt10_08, aes(DateTime))+
#   geom_line(aes(y=Sub_metering_1, colour="Kitchen"))+
#   geom_line(aes(y=Sub_metering_2, colour="Laundry Room"))+
#   geom_line(aes(y=Sub_metering_3, colour="Water Heater & AC"))+
#   labs(x="Date", y="Power (W-Hrs) - Avg. Every 10 Min", title = "Power Consumption Week 26 - 2009")+
#   guides(colour = guide_legend(override.aes = list(size=2)))+
#   theme(plot.title = element_text(face="bold"),
#         axis.title.x = element_text(face = "bold"),
#         axis.title.y = element_text(face = "bold"),
#         axis.text.x = element_text(colour = "black"),
#         axis.text.y = element_text(colour = "black"),
#         plot.background=element_rect(fill="cyan3"),
#         legend.key=element_rect(fill='white'),
#         legend.text=element_text(face='bold'),
#         legend.title = element_blank())
# ggwk26Mon_10

#############################
### Week 26 of 2008 - Tuesday
#############################
houseWeek26Day3flt10_08 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Wday = wday(DateTime), Day = day(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2008 & Week==26 & Wday==3 & (Min==0 | Min==10 | Min==20 | Min==30 | Min==40 | Min==50))

summary(houseWeek26Day3flt10_08)
houseWeek26Day3flt10_08

plot_ly(houseWeek26Day3flt10_08, x = ~houseWeek26Day3flt10_08$DateTime, y = ~houseWeek26Day3flt10_08$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek26Day3flt10_08$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek26Day3flt10_08$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 26 - 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Watt-hrs)"))

tblPlotly <- plot_ly(
  type = 'table',
  columnwidth = c(100, 100),
  columnorder = c(0, 1),
  header = list(
    values = c("DateTime","Kitchen"),
    align = c("center", "center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = c("grey", "grey")),
    font = list(family = "Arial", size = 14, color = "white")
  ),
  cells = list(
    values = rbind(houseWeek26Day3flt10_08$DateTime, houseWeek26Day3flt10_08$Sub_metering_1),
   align = c("center", "center"),
   line = list(color = "black", width = 1),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))

tblPlotly



##############################################
####Section 2 - Prepare to Analyze the Data###
##############################################

########################
###Sub-meter 3 Subset###
########################

house070809weekly <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Wday = wday(DateTime), Day = day(DateTime), Hour = hour(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009, Wday==2 & Hour==20 & Min==1)

summary(house070809weekly)
house070809weekly

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
logtsSM3_070809weekly <- log(tsSM3_070809weekly)
autoplot(logtsSM3_070809weekly)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

########################
###Sub-meter 1 Subset###
########################

house070809weeklySM1 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Week = week(DateTime), Wday = wday(DateTime), Day = day(DateTime), Hour = hour(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009, Wday==1 & Hour==20 & Min==1)

summary(house070809weeklySM1)
house070809weeklySM1

## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weeklySM1$Sub_metering_1, frequency=52, start=c(2007,1))

autoplot(tsSM1_070809weekly)

## Plot sub-meter 1 with autoplot - add labels, color

autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly)



########################
###Sub-meter 2 Subset###
########################


house070809weeklySM2 <- hhpwrDT %>%
  mutate(Year = year(DateTime), Month = month(DateTime), Week = week(DateTime), Wday = wday(DateTime), Day = day(DateTime), Hour = hour(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009, Wday==6 & Hour==8 & Min==1)%>%
  group_by(Date)

summary(house070809weeklySM2)
house070809weeklySM2

## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weeklySM2$Sub_metering_2, frequency=52, start=c(2007,1), end = c(2009,12))
tsSM2_070809weekly
summary(tsSM2_070809weekly)
autoplot(tsSM2_070809weekly)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")

## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809weekly)

tsSM2_070809weeklySMA2 <- SMA(tsSM2_070809weekly,n=2)
plot.ts(tsSM2_070809weeklySMA2)





##############################################
####Section 3 - Forecasting a Time Series###
##############################################

#############
# TSLM Forecast
#############

?tslm
#################
###Sub-meter 3###
#################

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=26)

## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=26, level=c(80,90))
forecastfitSM3c
summary(forecastfitSM3c)
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 3 forecast with confidence levels 90 and 95
forecastfitSM3c95 <- forecast(fitSM3, h=20, level=c(70,90))

summary(forecastfitSM3c95)
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c95, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time")


## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
plot(components070809SM3weekly$seasonal)
plot(components070809SM3weekly$trend)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)


## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
tsSM3_HW070809


SM3HWpred <- predict(tsSM3_HW070809, n.ahead = 26, prediction.interval = T, level = 0.90)
plot(tsSM3_HW070809,SM3HWpred)
summary(SM3HWpred)
## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=26)
plot(tsSM3_HW070809for, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")



## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=26, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(5, 15), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010,1))


# HWplot<-function(tsSM3_070809Adjusted,  n.ahead=20,  CI=.95,  error.ribbon='green', line.size=1){
#   
#   hw_object<-HoltWinters(tsSM3_070809Adjusted)
#   
#   forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
#   
#   
#   for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
#   
#   fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
#   
#   actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
#   
#   
#   graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
#   graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
#   graphset[is.na(graphset$dev),  ]$dev<-0
#   
#   graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
#   
#   
#   graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
#   
#   p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable), size=line.size) + geom_vline(x=max(actual_values$time),  lty=2) + xlab('Time') + ylab('Value') + opts(legend.position='bottom') + scale_colour_hue('')
#   return(p)
#   
# }
# 
# HWplot
#################
###Sub-meter 1###
#################

fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)

## Create the forecast for sub-meter 1. Forecast ahead 20 time periods 
forecastfitSM1 <- forecast(fitSM1, h=26)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM1)

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=26, level=c(80,90))
forecastfitSM1c
summary(forecastfitSM1c)
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time")

components070809SM1weekly <- decompose(tsSM1_070809weekly)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly)
plot(components070809SM1weekly$random)
## Check summary statistics for decomposed sub-meter 1 
summary(components070809SM1weekly)
summary(components070809SM1weekly$seasonal)
summary(components070809SM1weekly$trend)
summary(components070809SM1weekly$random)

## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 35))

## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=26)
plot(tsSM1_HW070809for, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

#################
###Sub-meter 2###
#################

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)
## Create the forecast for sub-meter 2. Forecast ahead 20 time periods 
forecastfitSM2 <- forecast(fitSM2, h=20)
## Plot the forecast for sub-meter 2. 
plot(forecastfitSM2)
forecastfitSM2
## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=26, level=c(80,90))
forecastfitSM2c
summary(forecastfitSM2c)
## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time")

components070809SM2weekly <- decompose(tsSM2_070809weekly)
## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM2weekly$x)
components070809SM2weekly

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 35))

## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 4), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))




################################################################################
# 4) TSLM annual forecast. Using the annual subset above (2007-09), forecast for
# submeter 3 for 2010 and 2011. 
################################################################################


# house070809yrly <- hhpwrDT %>%
#   mutate(Year = year(DateTime), Week = week(DateTime), Wday = wday(DateTime), Day = day(DateTime), Hour = hour(DateTime), Min = minute(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
#   filter(Year==2007 | Year==2008 | Year==2009)
# plot(x=house070809yrly$DateTime, y=house070809yrly$Sub_metering_3)
# 
# ## Create TS object with SubMeter3
tsSM3_070809yrly <- ts(Yr.sum$SM3, frequency=1, start=c(2007,1))
plot.ts(tsSM3_070809yrly)
autoplot(tsSM3_070809yrly)
tsSM3_070809yrly
summary(tsSM3_070809yrly)
# 
fitSM31011 <- tslm(tsSM3_070809yrly ~ trend)
summary(fitSM31011)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809yrly$SM3, ts.colour = 'red', xlab = "Time", ylab = "kWh", main = "Sub-meter 3")
plot(tsSM3_070809yrly)# 
# ## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809yrly)
# ## Create the forecast for sub-meter 3. Forecast ahead for 2010 and 2011
forecastfit1011SM3 <- forecast(fitSM31011, h=2)
forecastfit1011SM3
summary(forecastfit1011SM3)
plot(forecastfit1011SM3)
## Plot the forecast for sub-meter 3. 

# ## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfit1011SM3c <- forecast(fitSM31011, h=2, level=c(85,95))
forecastfit1011SM3c
summary(forecastfit1011SM3c)
# ## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfit1011SM3c, ylab= "kWh", xlab="Time", type = "b")
# 

# Answers  

# ---- Fit using liner model ----# 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)  2718.93     138.15  19.681   0.0323 *
#   trend         266.99      63.95   4.175   0.1497 (Can't rej Ho of no-trend) 
#
# Residual standard error: 90.44 on 1 degrees of freedom
# Multiple R-squared:  0.9457,	Adjusted R-squared:  0.8915 
# F-statistic: 17.43 on 1 and 1 DF,  p-value: 0.1497

#--- Forecast using linear model

# Error measures:
#                        ME     RMSE      MAE         MPE     MAPE      MASE       ACF1
# Training set 1.515825e-13 52.21701 49.23067 -0.02111001 1.527482 0.1843928 -0.6666667
# 
# Forecasts:
#      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2010       3786.881 3278.680 4295.082 1688.773 5884.989
# 2011       4053.869 3381.582 4726.156 1278.333 6829.405


# Plot


# ## Create TS object with SubMeter2
tsSM2_070809yrly <- ts(Yr.sum$SM2, frequency=1, start=c(2007,1))
plot.ts(tsSM2_070809yrly)
autoplot(tsSM2_070809yrly)
tsSM2_070809yrly
summary(tsSM2_070809yrly)
# 
fitSM21011 <- tslm(tsSM2_070809yrly ~ trend)
summary(fitSM21011)
## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809yrly$SM2, ts.colour = 'red', xlab = "Time", ylab = "kWh", main = "Sub-meter 2")
plot(tsSM2_070809yrly)# 
# ## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809yrly)
# ## Create the forecast for sub-meter 2. Forecast ahead for 2010 and 2011
forecastfit1011SM2 <- forecast(fitSM21011, h=2)
forecastfit1011SM2
summary(forecastfit1011SM2)
plot(forecastfit1011SM2)
## Plot the forecast for sub-meter 2. 

# ## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfit1011SM2c <- forecast(fitSM21011, h=2, level=c(85,95))
forecastfit1011SM2c
summary(forecastfit1011SM2c)
# ## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfit1011SM2c, ylab= "kWh", xlab="Time", type = "b")



# ## Create TS object with SubMeter1
tsSM1_070809yrly <- ts(Yr.sum$SM1, frequency=1, start=c(2007,1))
plot.ts(tsSM1_070809yrly)
autoplot(tsSM1_070809yrly)
tsSM1_070809yrly
summary(tsSM1_070809yrly)
# 
fitSM11011 <- tslm(tsSM1_070809yrly ~ trend)
summary(fitSM11011)
## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809yrly$SM2, ts.colour = 'red', xlab = "Time", ylab = "kWh", main = "Sub-meter 1")
plot(tsSM1_070809yrly)# 
# ## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809yrly)
# ## Create the forecast for sub-meter 1. Forecast ahead for 2010 and 2011
forecastfit1011SM1 <- forecast(fitSM11011, h=2)
forecastfit1011SM1
summary(forecastfit1011SM1)
plot(forecastfit1011SM1)
## Plot the forecast for sub-meter 1. 

# ## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfit1011SM1c <- forecast(fitSM11011, h=2, level=c(85,95))
forecastfit1011SM1c
summary(forecastfit1011SM1c)
# ## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfit1011SM1c, ylab= "kWh", xlab="Time", type = "b")

######
# 5) Create a subset that shows the total kWh per month for submeter 3 for the 
# months Jan-07 through Oct-10. Forecast for Nov-10 through Dec-11. Note: Be 
# sure to make the adjustment depending on if the ts is seasonal. Also, be sure 
# to record the summary metrics and know how to interpret the output; specifically, 
# R-squared, Adjusted R-squared, F-stat, and p-value. Also, understand how the 
# p-value relates to the null hypothesis regarding the statistics (i.e., slope 
# coefficients). For an additional resource for learning about regression output, 
# I suggest Brandon Foltz's tutorials on YouTube for statistics/regression. 
######


# Your code
month.sum2 <- hhpwrDT%>%
  mutate(Year = year(DateTime), Month = month(DateTime))%>%
  filter(Date >= "2007-01-01" & Date <= "2010-10-31") %>%
  group_by(Year, Month)%>%
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>%
  filter(!is.na(Month))
month.sum2

tsSM3_07080910mnthly <- ts(month.sum2$SM3, frequency=12, start=c(2007,1))
tsSM3_07080910mnthly
plot(tsSM3_07080910mnthly)

dd_tsSM3_07080910mnthly <- decompose(tsSM3_07080910mnthly)
plot(dd_tsSM3_07080910mnthly)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_07080910mnthlyAdjusted <- tsSM3_07080910mnthly - dd_tsSM3_07080910mnthly$seasonal
autoplot(tsSM3_07080910mnthlyAdjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal

dd_tsSM3_07080910mnthlyADJ <- decompose(tsSM3_07080910mnthlyAdjusted)
plot(dd_tsSM3_07080910mnthlyADJ)
plot(dd_tsSM3_07080910mnthlyADJ$seasonal)
plot(dd_tsSM3_07080910mnthlyADJ$trend)
plot(dd_tsSM3_07080910mnthlyADJ$random)
summary(dd_tsSM3_07080910mnthlyADJ$trend)

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809mnthly <- HoltWinters(tsSM3_07080910mnthlyAdjusted, beta=FALSE, gamma=FALSE)
tsSM3_HW070809mnthly
summary(tsSM3_HW070809mnthly)
plot(tsSM3_HW070809mnthly, ylim = c(150, 400), main = "HW Exponential Smoothing: Sub-meter 3")

fitSM3mnth1011 <- tslm(formula=tsSM3_07080910mnthly ~ trend + season)
fitSM3mnth1011
summary(fitSM3mnth1011)

forfitmnth1011SM3c <- forecast(fitSM3mnth1011, h=14, level=c(80,95),start=c(2010,11))
forfitmnth1011SM3c
summary(forfitmnth1011SM3c)
autoplot (forfitmnth1011SM3c, ylab= "kWh", xlab="Time", title="Monthly Usage Forecast")
plot(forfitmnth1011SM3c, ylab= "kWh", xlab="Time", main="Sub-meter 3 Forecast")

## HoltWinters forecast & plot
tsSM3_HW070809mnthlyFor <- forecast(tsSM3_HW070809mnthly, h=25)
plot(tsSM3_HW070809mnthlyFor, ylim = c(100, 500), ylab= "kWh", xlab="Time - Sub-meter 3")
summary(tsSM3_HW070809mnthlyFor)
## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809mnthlyForC <- forecast(tsSM3_HW070809mnthly, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809mnthlyForC, ylim = c(250, 350), ylab= "kWh", xlab="Time - Sub-meter 3", start(2010))

##SM1 Monthly Forecast##

tsSM1_07080910mnthly <- ts(month.sum2$SM1, frequency=12, start=c(2007,1))
tsSM1_07080910mnthly
plot(tsSM1_07080910mnthly)

dd_tsSM1_07080910mnthly <- decompose(tsSM1_07080910mnthly)
plot(dd_tsSM1_07080910mnthly)

## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_07080910mnthlyAdjusted <- tsSM1_07080910mnthly - dd_tsSM1_07080910mnthly$seasonal
autoplot(tsSM1_07080910mnthlyAdjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_07080910mnthlyAdjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809mnthly <- HoltWinters(tsSM1_07080910mnthlyAdjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809mnthly, ylim = c(25, 80), main = "HW Exponential Smoothing: Sub-meter 1")


fitSM1mnth1011 <- tslm(formula=tsSM1_07080910mnthly ~ trend + season)
fitSM1mnth1011
summary(fitSM1mnth1011)

forfitmnth1011SM1c <- forecast(fitSM1mnth1011, h=14, level=c(80,95),start=c(2010,11))
forfitmnth1011SM1c
summary(forfitmnth1011SM1c)
autoplot (forfitmnth1011SM1c, ylab= "kWh", xlab="Time", title="Monthly Usage Forecast")
plot(forfitmnth1011SM1c, ylab= "kWh", xlab="Time", main = "Sub-meter 1 Forecast")

## HoltWinters forecast & plot
tsSM1_HW070809mnthlyFor <- forecast(tsSM1_HW070809mnthly, h=25)
plot(tsSM1_HW070809mnthlyFor, ylim = c(0, 100), ylab= "kWh", xlab="Time - Sub-meter 1")

## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809mnthlyForC <- forecast(tsSM1_HW070809mnthly, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809mnthlyForC, ylim = c(35, 55), ylab= "kWh", xlab="Time - Sub-meter 1", start(2010))




##SM2 Monthly Forecast##

tsSM2_07080910mnthly <- ts(month.sum2$SM2, frequency=12, start=c(2007,1))
tsSM2_07080910mnthly
plot(tsSM2_07080910mnthly)

dd_tsSM2_07080910mnthly <- decompose(tsSM2_07080910mnthly)
plot(dd_tsSM2_07080910mnthly)

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_07080910mnthlyAdjusted <- tsSM2_07080910mnthly - dd_tsSM2_07080910mnthly$seasonal
autoplot(tsSM2_07080910mnthlyAdjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_07080910mnthlyAdjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809mnthly <- HoltWinters(tsSM2_07080910mnthlyAdjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809mnthly, ylim = c(25, 100), main = "HW Exponential Smoothing: Sub-meter 2")


fitSM2mnth1011 <- tslm(formula=tsSM2_07080910mnthly ~ trend + season)
fitSM2mnth1011
summary(fitSM2mnth1011)

forfitmnth1011SM2c <- forecast(fitSM2mnth1011, h=14, level=c(80,95),start=c(2010,11))
forfitmnth1011SM2c
summary(forfitmnth1011SM2c)
autoplot (forfitmnth1011SM2c, ylab= "kWh", xlab="Time", title="Monthly Usage Forecast")
plot(forfitmnth1011SM2c, ylab= "kWh", xlab="Time", main = "Sub-meter 2 Forecast")

## HoltWinters forecast & plot
tsSM2_HW070809mnthlyFor <- forecast(tsSM2_HW070809mnthly, h=25)
plot(tsSM2_HW070809mnthlyFor, ylim = c(0, 100), ylab= "kWh", xlab="Time - Sub-meter 2")

## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809mnthlyForC <- forecast(tsSM2_HW070809mnthly, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809mnthlyForC, ylim = c(30, 60), ylab= "kWh", xlab="Time - Sub-meter 2", start(2010))

# Answers:

# ---- Filter  ---- #

# Groups:   Year [1]
#    Year Month    SM1     SM2     SM3   DateTime
#   <dbl> <ord>  <dbl>   <dbl>   <dbl>     <dttm>
# 1  2007   Jan 56.433  79.274 329.578 2007-01-01
# 2  2007   Feb 47.584  64.604 270.274 2007-02-01
# 3  2007   Mar 60.769 104.733 290.361 2007-03-01
# 4  2007   Apr 42.078  38.417 189.503 2007-04-01


# ---- Fit using linear model ----#

# Call:
# tslm(formula = YrMo0710tsSM3 ~ trend + season)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -73.206 -22.304  -0.428  19.314  95.510 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  305.4980    20.6848  14.769 4.22e-16 ***
# trend          1.9116     0.4308   4.437 9.59e-05 ***
# season2      -35.2116    26.8680  -1.311  0.19906    
# season3      -39.9132    26.8783  -1.485  0.14705    
# season4      -65.2747    26.8956  -2.427  0.02085 *  
# season5      -50.5291    26.9197  -1.877  0.06938 .  
# season6      -89.9117    26.9507  -3.336  0.00211 ** 
# season7     -162.5647    26.9886  -6.023 9.00e-07 ***
# season8     -190.8591    27.0333  -7.060 4.42e-08 ***
# season9      -90.8444    27.0847  -3.354  0.00201 ** 
# season10     -65.2370    27.1429  -2.403  0.02202 *  
# season11     -44.1863    29.0681  -1.520  0.13801    
# season12      -0.1499    29.0968  -0.005  0.99592    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 37.99 on 33 degrees of freedom
# Multiple R-squared:  0.7666,	Adjusted R-squared:  0.6817 
# F-statistic:  9.03 on 12 and 33 DF,  p-value: 2.522e-07


# ---- Forecast using linear model ----@

# Call:
# tslm(formula = ds_YrMoSM3ts ~ trend + season)
#
# Coefficients:
#  (Intercept)       trend      season2      season3      season4      season5      season6  
#    305.4980       1.9116     -35.2116     -39.9132     -65.2747     -50.5291     -89.9117  
#     season7      season8      season9     season10     season11     season12  
#   -162.5647    -190.8591     -90.8444     -65.2370     -44.1863      -0.1499  
#
# Error measures:
#                        ME     RMSE     MAE       MPE     MAPE      MASE      ACF1
# Training set -2.161824e-15 32.17895 25.4186 -2.412659 11.10408 0.5400616 0.1368401
#
# Forecasts:
#          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Nov 2010       351.1559 292.2140 410.0978 259.4569 442.8550
# Dec 2010       397.1039 338.1620 456.0458 305.4049 488.8030
# Jan 2011       399.1654 341.1026 457.2283 308.8340 489.4969
# Feb 2011       365.8654 307.8026 423.9283 275.5340 456.1969
# Mar 2011       363.0754 305.0126 421.1383 272.7440 453.4069
# Apr 2011       339.6254 281.5626 397.6883 249.2940 429.9569
# May 2011       356.2827 298.2198 414.3455 265.9513 446.6141
# Jun 2011       318.8117 260.7488 376.8745 228.4803 409.1431
# Jul 2011       248.0702 190.0073 306.1330 157.7388 338.4016
# Aug 2011       221.6874 163.6246 279.7503 131.3560 312.0189
# Sep 2011       323.6137 265.5508 381.6765 233.2823 413.9451
# Oct 2011       351.1327 293.0698 409.1955 260.8013 441.4641
# Nov 2011       374.0949 313.2450 434.9448 279.4275 468.7623
# Dec 2011       420.0429 359.1930 480.8928 325.3755 514.7103



###########
# Decompose
###########

######
# 6) Using the ts for SM3 that shows kWh by month over the Jan-07 thru Oct-10 
# time period (monthly subset above), decompose this ts into seasonal, trend, 
# and random components. Also provide a plot for these components.
######

# Your code for decomposing - show summary statistics for seasonal, trend, random






######
# 7) Create a subset that shows kWh by hour over each day during Feb-10. Create 
# a ts object for SM3 and decompose this ts into seasonal, trend, and random 
# components. Also provide a plot for these components. Things to consider: 1) the 
# number of seasonal periods in this month, and 2) the frequency value needed for 
# each of these seasonal periods (you may need to research how to set the frequency 
# argument for seasonal periods less than a year). 
######

# Your code to subset





# A tibble: 6 x 6
# Groups:   Day [1]
# Day  Hour   SM1   SM2   SM3            DateTime
# <int> <int> <dbl> <dbl> <dbl>              <dttm>
# 1     1     0     0 0.000 0.041 2010-02-01 00:00:00
# 2     1     1     0 0.000 0.041 2010-02-01 01:00:00
# 3     1     2     0 0.047 0.306 2010-02-01 02:00:00
# 4     1     3     0 0.049 0.431 2010-02-01 03:00:00
# 5     1     4     0 0.000 0.041 2010-02-01 04:00:00
# 6     1     5     0 0.000 0.041 2010-02-01 05:00:00


# Filter from the hourly subset (3) created above.


# Your code for decomposing - show summary statistics for seasonal, trend, random





###################
# Holt-Winters (HW)
###################

######
# 8) Need non-seasonal ts for HW. Therefore, create a ts object for SM3 for the 
# Winter months Dec-09 thru Feb-10 season. To do this, create a subset that shows 
# kWh by day over the season, then forecast the next 30 days. Plot the fit and 
# forecast objects for the season. (The plot will include the data leading up to 
# the forecast, and the forecast itself. In the POA, it shows how to plot the 
# 'forecast only', which you could do as well. The plot will show the actual data 
# and the forecasted (in red) in the same chart. Note: to create the HW forecast 
# object, you may need to use forecast() I/O forecast.HoltWinters(). You may want 
# to consider using decompose to remove any seasonality that may be present in these 
# ts objects. Be sure to evaluate the residuals using the Ljung-Box test, etc. 
# Refer to The Little Book of R.                                                                                                                                                                                                                                                                                                
######                                                                                                                                                                                                                                                                                                                 


# Your code for the subset





# Groups:   Year, Month [1]
#    Year Month   Day   SM1   SM2    SM3   DateTime
#   <dbl> <dbl> <int> <dbl> <dbl>  <dbl>     <dttm>
# 1  2009    12     1 0.000 0.354 10.821 2009-12-01
# 2  2009    12     2 1.143 7.052 12.851 2009-12-02
# 3  2009    12     3 1.039 0.402  9.906 2009-12-03
# 4  2009    12     4 1.158 0.352  9.134 2009-12-04


# Your code for ts





# Your code for HW





