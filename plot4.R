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
## PLOT 4: Results in a plot of pm2.5 emissions by coal combustion-related sources
## in the U.S. in the years 1999, 2002, 2005 and 2008
##
##-----------------------------------------------------------------------------
##---------------------------------- CODING -----------------------------------
##
##---- step 0. Loading relevant packages
library("dplyr")
library("ggplot2")
##
##-----------------------------------------------------------------------------
##------------------ step 1: read data into tibbles for speed in dplyr --------
##
NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))           ## use readRDS, files are in WD
SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
##
##-----------------------------------------------------------------------------
##------------------ step 2: Select and prepare the required data -------------
##
SCC_sel <- SCC %>%                              ## select SCC-rows for sources that are
        filter(grepl('[Cc]ombustion', SCC.Level.One)) %>%  ## combustion-relayed
        filter(grepl('[Cc][Oo][Aa][Ll]', Short.Name))      ## and use coal in all varieties
Source_sel <- as.vector(SCC_sel$SCC)                       ## vector with selected SCC-codes

NEIY <-                                               ## dplyr-code to create NEIY
        NEI %>%                                       ## use NEI to
        filter(SCC %in% Source_sel) %>%               ## select rows for relevant SCC's
        select(year, Emissions) %>%                   ## select relevant columns
        group_by(year) %>%                            ## group them by year
        summarize(totalTons = sum(Emissions)) %>%     ## calculate the sum of emissions
        mutate(totalkTons = totalTons/1000)           ## add column for thousands of tons

##
##-----------------------------------------------------------------------------
##------------------ step 3: Setup & draw plot in png-format ------------------
##
if (file.exists("plot4.png")) {unlink("plot4.png")}   ## delete existing plot.png
png(filename = "plot4.png",                           ## prepare png-file
    width = 480, 
    height = 480)

YRange <- c(0, max(NEIY$totalkTons))                  ## set the Y-range for the plot
g <- ggplot(NEIY, aes(year, totalkTons))              ## setup graphic object for ggplot
g+geom_point()+                                       ## plot points
        geom_path() +                                 ## plot line-segments (connect the dots)
        ylim(YRange) +                                   ## set range Y-axis
        xlab("Year") +                                ## label X-axis
        ylab("Total pm2.5 emission (thousands of tons)") +   ## label Y-axis
        ggtitle("Total coal combustion-related pm2.5 emission in the U.S.")


dev.off(which = dev.cur())                            ## release device
##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

