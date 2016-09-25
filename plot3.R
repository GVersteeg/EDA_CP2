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
## PLOT 3: Results in a plot of TOTAL emissions in Baltimore City (24510)
## per type of source for the years 1999, 2002, 2005 and 2008
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
fips_sel <- c("24510")                                ## row-selector for Baltimore
NEIY <-                                               ## dplyr-code to create NEIY
        NEI %>%                                       ## use NEI to:
        filter(fips %in% fips_sel) %>%                ## select rows for Baltimore city
        select(type, year, Emissions) %>%             ## select relevant columns
        group_by(type, year) %>%                      ## group them by type and then year
        summarize(totalTons = sum(Emissions))         ## calculate the sum of emissions

##
##-----------------------------------------------------------------------------
##------------------ step 3: Setup & draw plot in png-format ------------------
##
if (file.exists("plot3.png")) {unlink("plot3.png")}   ## delete existing plot.png
png(filename = "plot3.png",                           ## prepare png-file
        width = 960, 
        height = 480)

g <- ggplot(NEIY, aes(year, totalTons))               ## setup graphic object for ggplot
g+geom_point()+                                       ## plot points
        geom_path()+                                  ## plot line-segments (connect the dots)
        facet_grid(.~type)+                           ## setup the grid (1 row x 4 columns)
        xlab("Year") +                                ## label X-axis
        ylab("pm2.5 Emission (tons)") +               ## label Y-axis
        ggtitle("PM2.5 Emission per type in Baltimore City")

dev.off(which = dev.cur())                            ## release device
##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

