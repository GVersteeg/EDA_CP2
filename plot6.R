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
## PLOT 6: Results in a comparison plot of PERCENTUAL changes year-by-year in 
## motor vehicle emissions in Baltimore City (24510) vs. Los Angeles County (06037)
## for the years 1999, 2002, 2005 and 2008
##
## NOTE: This plot shows the percentual change of pm2.5 emissions year over year:
##      1999 = 0%
##      2002 = (2002-1999)/1999 percentual change of 2002 compared to 1999
##      2005 = (2005-2002)/2002 percentual change of 2005 compared to 2002
##      2008 = (2008-2005)/2005 percentual change of 2008 compared to 2005
##
##-----------------------------------------------------------------------------
##---------------------------------- CODING -----------------------------------
##
library("dplyr")
library("ggplot2")
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
fips_sel <- c("24510", "06037")                       ## row-selector for Baltimore & LA
type_sel <- c("ON-ROAD")                              ## row-selector for motor vehicles
NEIY <-                                               ## dplyr-code to create NEIY
        NEI %>%                                       ## use NEI to:
        filter(fips %in% fips_sel) %>%                ## select rows for relevant fips
        filter(type %in% type_sel) %>%                ## select rows for relevant types
        select(fips, year, Emissions) %>%             ## select relevant columns
        group_by(fips, year) %>%                      ## group them by fips and then year
        summarize(totalTons = sum(Emissions)) %>%     ## calculate the sum of emissions
        mutate(change = (totalTons-lag(totalTons,1))/lag(totalTons,1)) ## add a perc.change column

NEIY[is.na(NEIY$change),4] <- 0                       ## fill NA's with 0 (NA's due to lag function)
NEIY[NEIY$fips == "24510",1] <- "Baltimore City"      ## meaningfull names
NEIY[NEIY$fips == "06037",1] <- "Los Angeles County"
##
##-----------------------------------------------------------------------------
##------------------ step 3: Setup & draw plot in png-format ------------------
##
if (file.exists("plot6.png")) {unlink("plot6.png")}   ## delete existing plot.png
png(filename = "plot6.png",                           ## prepare png-file
        width = 700, 
        height = 480)

g <- ggplot(NEIY, aes(year, change))               ## setup graphic object for ggplot
g+geom_point()+                                       ## plot points
        geom_path() +                                 ## plot line-segments (connect the dots)
        scale_y_continuous(labels=percent) +          ## set Y-axis on percentages
        facet_grid(.~fips) +                          ## setup the grid (1 row x 2 columns)
        xlab("Year") +                                ## label X-axis
        ylab("Change in pm2.5 Emission (percentage)") +   ## label Y-axis
        ggtitle("Change in PM2.5 Emission by motor vehicles in Baltimore vs. LA (year over year)")

dev.off(which = dev.cur())                            ## release device
##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

