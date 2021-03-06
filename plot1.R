## Script for Programming Assignment 2 Exploratory Data Analysis course
## Author: Gerrit Versteeg
## Last saved: Sept 20th, 2016
##-----------------------------------------------------------------------------
## Required files 'Source_Classification_Code.rds'and 'summarySCC_PM25.rds'
## have already been downloaded and available in de working directory
##
## 1. Files are read into SCC (11717 x 15) and in NEI (6497651 x 6) 
## using tibbles for speed in dplyr.
##
## 2. Relevant data is selected and prepared for analysis and plotting
##
## 3. The plot in .png is generated
##-----------------------------------------------------------------------------
##
## PLOT 1: Results in a plot of TOTAL emissions in the U.S.
## in the years 1999, 2002, 2005 and 2008
##
##-----------------------------------------------------------------------------
##---------------------------------- CODING -----------------------------------
##
##---- step 0. Loading relevant packages
library("dplyr")
##
##-----------------------------------------------------------------------------
##------------------ step 1: read data into tibbles for speed in dplyr --------
##
NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))         ## use readRDS, files are in WD
SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
##
##-----------------------------------------------------------------------------
##------------------ step 2: Select and prepare the required data -------------
##
NEIY <-                                               ## dplyr-code to create NEIY
        NEI %>%                                       ## use NEI to
        select(year, Emissions) %>%                   ## select relevant columns
        group_by(year) %>%                            ## group them by year
        summarize(totalTons = sum(Emissions)) %>%     ## calculate the sum of emissions
        mutate(totalMTons = totalTons/1000000)       ## add column for millions of tons

##
##-----------------------------------------------------------------------------
##------------------ step 3: Setup & draw plot in png-format ------------------
##
if (file.exists("plot1.png")) {unlink("plot1.png")}   ## delete existing plot.png
png(filename = "plot1.png",                           ## prepare png-file
    width = 480, 
    height = 480)

YRange <- c(0, max(NEIY$totalMTons))                  ## set the Y-range for the plot
with(NEIY, plot(year, totalMTons,                     ## plot total em's over years
     type = "b",                                      ## use dots and connecting lines
     main = "Total pm2.5 emission in the U.S.",       ## set main title
     ylim = YRange,                                   ## set range Y-axis
     xlab = "Year",                                   ## set label X-axis
     ylab = "Total emission (millions of tons)"))     ## set label Y-axis

dev.off(which = dev.cur())                            ## release device
##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

