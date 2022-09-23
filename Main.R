
#header #################################################################################
#'Main.R'

#Title: title
#Project ID: pid
#Client: client
#Author: <Eduardo> <Costa>, UFRGS

#Description: description

#Start date: date
#Last Update: {6:date}

#R version: r.version
#Scriptversion: version

#Dependencies
#<-Downstream
#->Upstream
  # -tidy_data.R
  # -Incidencia8_11.Rmd

#Input:
#-None

#Output:
#-None

#Peer reviewer(s)

#Please ensure directories are relative. Please comment code sufficiently.

#Script start#############################################################################






#################
# Call packages #
#################


#Packages to be used
packages<-c("readxl","here","tidyquant","tidyverse","lubridate","ggplot2","lme4","car","knitr","glmmsr","plotly","gridExtra","grid","ggridges","ggthemes","bbmle","lattice","psych","lmtest","dynlm","GeneCycle","tseries","TTR","forecast","kableExtra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


# Packages loading
invisible(lapply(packages, library, character.only = TRUE))  


##################
# Create folders #
##################

dir.create(here("Figures"))
dir.create(here("Output"))



#################
# Tidy the data #
#################

source(here("Scripts","tidy_data.R"))

####################
# Run the analysis #
####################

knitr::knit(here("Scripts","Incidencia8_11.Rmd"), output = here("Outputs","results.html"))

