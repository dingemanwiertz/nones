#############################################################
###   5. Repeated MRP analyses (five-year time windows)   ###
#############################################################



# This script conducts Repeated MRP analyses as described in the methods
# section of the paper. It represents the foundation for the Repeated MRP 
# estimates that are referred to in Figures 1 & 2. 

# The script produces estimates for the years 1990, 2001, 2008, and 2014. 
# It is based on five-year samples from the GSS and merges in additional 
# information from the Census/ACS, RCMS, and American Presidency Project
# (see the files "01 - Preparation GSS data.do", "02 - Preparation state-level 
# data.do", and "03 - Preparation poststratification data.do" for details).  

# The script proceeds in the following steps: 
#   5.1. Loading required packages; 
#   5.2. Data preparation; 
#   5.3. Multilevel regressions;
#   5.4. Obtaining predicted probabilities for each respondent type; 
#   5.5. Weighting predictions by prevalence of respondent types;
#   5.6. Aggregating weighted predictions for each state-year combination;  
#   5.7. Collecting all estimates in a table; 
#   5.8. Saving the table as a Stata dataset.



##########################################
###   5.1. Loading required packages   ###
##########################################


rm(list=ls(all=TRUE))

library("arm")
library("foreign")
library("haven")
library("fastDummies")



#################################
###   5.2. Data preparation   ###
#################################


# Set working directory:
setwd("... ... ...")

# Read in GSS data:
GSS <- read_dta("GSS7318_MRP.dta")
GSS <- GSS[order(GSS$stnum, GSS$year),]

# Read in state-level data:
Statelevel <- read_dta("State_allvars_7318.dta")
Statelevel <- Statelevel[order(Statelevel$stnum, Statelevel$year),]

# Read in Census/ACS data:
Census <- read_dta("Census_poststrat_7318.dta") 
Census <- Census[order(Census$stnum, Census$year),]

# Read in auxiliary data: 
Statelevel_key <- read_dta("state identifiers crosswalk.dta")
  # This file, containing state identifiers, is available through the 
  #   replication repository: https://github.com/dingemanwiertz/nones.  


# Prepare explanatory variables:

  # At level of GSS dataset:
GSS$race_female <- as.factor((GSS$female * 4) + GSS$race_wbho)  
# 1 = White man, 2 = Black man, 3 = Hisp man, 4 = Other man, etc (up to 8).  
GSS$age_cat <- as.factor(GSS$age_cat)  
# 1 = 18-29, 2 = 30-44, 3 = 45-64, 4 = 65+. 
GSS$edu_cat <- as.factor(GSS$edu_cat)  
# 1 = no HS degree, 2 = HS degree, 3 = some college, 4 = college degree. 
GSS_Statelevel_match <- match(paste(GSS$stnum, GSS$year), 
                              paste(Statelevel$stnum, Statelevel$year))
# Function to facilitate merges between GSS and Statelevel data 
GSS$region <- as.factor(Statelevel$region[GSS_Statelevel_match])  
# Indicator for major Census region.  
GSS$totcng <- Statelevel$totcng[GSS_Statelevel_match]  
# Total density of congregations per 1,000 people.  
GSS$evancng <- Statelevel$evancng[GSS_Statelevel_match]  
# Density of Evangelical congregations per 1,000 people.  
GSS$maincng <- Statelevel$maincng[GSS_Statelevel_match]  
# Density of Mainline congregations per 1,000 people.  
GSS$cathcng <- Statelevel$cathcng[GSS_Statelevel_match]  
# Density of Catholic congregations per 1,000 people.  
GSS$blackcng <- Statelevel$blackcng[GSS_Statelevel_match]  
# Density of Black Protestant congregations per 1,000 people.  
GSS$othcng <- GSS$totcng - GSS$evancng - GSS$maincng - GSS$cathcng  
# Density of Other congregations per 1,000 people (incl Black Protestants). 
GSS$repvote <- (Statelevel$repvote[GSS_Statelevel_match] - 50) * 0.1  
# Vote share of Republican presidential candidates. 

  # At census level (same coding and definitions as above):
Census$race_female <- as.factor((Census$female * 4) + Census$race_wbho)
Census$age_cat <- as.factor(Census$age_cat)  
Census$edu_cat <- as.factor(Census$edu_cat) 
Census_Statelevel_match <- match(paste(Census$stnum, Census$year), 
                                 paste(Statelevel$stnum, Statelevel$year))
Census$region <- as.factor(Statelevel$region[Census_Statelevel_match])
Census$totcng <- Statelevel$totcng[Census_Statelevel_match]  
Census$evancng <- Statelevel$evancng[Census_Statelevel_match]  
Census$maincng <- Statelevel$maincng[Census_Statelevel_match]  
Census$cathcng <- Statelevel$cathcng[Census_Statelevel_match]  
Census$blackcng <- Statelevel$blackcng[Census_Statelevel_match]  
Census$othcng <- Census$totcng - Census$evancng - Census$maincng - Census$cathcng     
Census$repvote <- (Statelevel$repvote[Census_Statelevel_match] - 50) * 0.1  

  # Turning categorical variables at Census level into dummy variables: 
Census <- dummy_cols(Census, 
                     select_columns = c("race_female", "edu_cat", "age_cat", "region"))


# Create subsets of data: 
for (t in c(1990, 2001, 2008, 2014)) {
  dataname <- paste("GSS", t, sep = "")
  assign(dataname, subset(GSS, year > t-3 & year < t+3))
  dataname <- paste("Statelevel", t, sep = "")
  assign(dataname, subset(Statelevel, year == t))
  dataname <- paste("Census", t, sep = "")
  assign(dataname, subset(Census, year == t))
}
Census2001 <- subset(Census, year == 2000)
GSS <- list(GSS1990, GSS2001, GSS2008, GSS2014)
Statelevel <- list(Statelevel1990, Statelevel2001, Statelevel2008, Statelevel2014)
Census <- list(Census1990, Census2001, Census2008, Census2014)


# Create  vectors including object names that we later draw upon: 
cong_dens <- c("", "evancng", "maincng", "blackcng", "cathcng")
outcome <- c("None", "Evangelical", "Mainline", "BlackProt", "Catholic", 
             "WeeklyAttend", "NeverAttend", "StrongID")
year <- c("1990", "2001", "2008", "2014")


# Create lists that we will store our results in: 
model <- list()
mod_sum1 <- list()
mod_sum2 <- list()
stranefs <- list()
cellpred <- list()
cellprwt <- list()
estimate <- list() 



#######################################
###   5.3. Multilevel regressions   ###
#######################################


for (t in 1:4) {
  
  # For religious nones: 
  model[[year[t]]][["None"]] <- glmer(formula = None ~ 
                             race_female + age_cat + edu_cat 
                           + (1 | stname) + region 
                           + evancng + maincng + cathcng + othcng, 
                           data=GSS[[t]], family=binomial(link="logit"), 
                           control=glmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=1e6)), nAGQ = 1)
  mod_sum1[[year[t]]][["None"]] <- summary(model[[year[t]]][["None"]]) 
  mod_sum2[[year[t]]][["None"]] <- c(isSingular(model[[year[t]]][["None"]]),
                                     summary(rePCA(model[[year[t]]][["None"]])))
 
  # For Christian denominations: 
  for (i in 2:5) {
    f <- as.formula(paste(
      outcome[i], 
      "~ race_female + age_cat + edu_cat", 
      "+ (1 | stname) + region +", 
      cong_dens[i]))
    model[[year[t]]][[outcome[i]]] <- glmer(formula = f,
                              data=GSS[[t]], family=binomial(link="logit"), 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=1e6)), nAGQ = 1)
    mod_sum1[[year[t]]][[outcome[i]]] <- summary(model[[year[t]]][[outcome[i]]])
    mod_sum2[[year[t]]][[outcome[i]]] <- c(isSingular(model[[year[t]]][[outcome[i]]]),
                                           summary(rePCA(model[[year[t]]][[outcome[i]]])))
  }
  
  # For religious attendance and identification: 
  for (i in 6:8) {
    f <- as.formula(paste(
      outcome[i], 
      "~ race_female + age_cat + edu_cat", 
      "+ (1 | stname) + region +", 
      "+ evancng + maincng + cathcng + othcng + repvote" ))
    model[[year[t]]][[outcome[i]]] <- glmer(formula = f,
                              data=GSS[[t]], family=binomial(link="logit"), 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=1e6)), nAGQ = 1)
    mod_sum1[[year[t]]][[outcome[i]]] <- summary(model[[year[t]]][[outcome[i]]])
    mod_sum2[[year[t]]][[outcome[i]]] <- c(isSingular(model[[year[t]]][[outcome[i]]]),
                                           summary(rePCA(model[[year[t]]][[outcome[i]]])))
  }
  
  # Collecting the state-level random intercepts: 
  for (i in 1:8) {
    stranefs[[year[t]]][[outcome[i]]] <- array(NA,c(51,1))
    dimnames(stranefs[[year[t]]][[outcome[i]]]) <- list(c(Statelevel[[t]]$stname),"effect")
    for (s in Statelevel[[t]]$stname) {
      stranefs[[year[t]]][[outcome[i]]][s,1] <- ranef(model[[year[t]]][[outcome[i]]])$stname[s,1]
    }
    stranefs[[year[t]]][[outcome[i]]][,1][is.na(stranefs[[year[t]]][[outcome[i]]][,1])] <- 0  
  }
  
}

# Printing model summaries: 
mod_sum1 
mod_sum2 



###########################################################################
###   5.4. Obtaining predicted probabilities for each respondent type   ###
###########################################################################


for (t in 1:4) {
  
  # For religious nones: 
  M <- model[[year[t]]][["None"]] 
  cellpred[[year[t]]][["None"]] <- invlogit(fixef(M)["(Intercept)"]
                                 + (fixef(M)["race_female2"] * Census[[t]]$race_female_2)
                                 + (fixef(M)["race_female3"] * Census[[t]]$race_female_3)
                                 + (fixef(M)["race_female4"] * Census[[t]]$race_female_4)
                                 + (fixef(M)["race_female5"] * Census[[t]]$race_female_5)
                                 + (fixef(M)["race_female6"] * Census[[t]]$race_female_6)
                                 + (fixef(M)["race_female7"] * Census[[t]]$race_female_7)
                                 + (fixef(M)["race_female8"] * Census[[t]]$race_female_8)
                                 + (fixef(M)["age_cat2"] * Census[[t]]$age_cat_2)
                                 + (fixef(M)["age_cat3"] * Census[[t]]$age_cat_3)
                                 + (fixef(M)["age_cat4"] * Census[[t]]$age_cat_4)
                                 + (fixef(M)["edu_cat2"] * Census[[t]]$edu_cat_2)
                                 + (fixef(M)["edu_cat3"] * Census[[t]]$edu_cat_3)
                                 + (fixef(M)["edu_cat4"] * Census[[t]]$edu_cat_4)
                                 + stranefs[[year[t]]][["None"]][Census[[t]]$stnum,1]
                                 + (fixef(M)["region2"] * Census[[t]]$region_2)
                                 + (fixef(M)["region3"] * Census[[t]]$region_3)
                                 + (fixef(M)["region4"] * Census[[t]]$region_4)
                                 + (fixef(M)["evancng"] * Census[[t]]$evancng)
                                 + (fixef(M)["maincng"] * Census[[t]]$maincng)
                                 + (fixef(M)["cathcng"] * Census[[t]]$cathcng)
                                 + (fixef(M)["othcng"] * Census[[t]]$othcng)
  )
  
  # For Christian denominations: 
  for (i in 2:5) {
    M <- model[[year[t]]][[outcome[i]]] 
    cellpred[[year[t]]][[outcome[i]]] <- unlist(invlogit(fixef(M)["(Intercept)"]
                                              + (fixef(M)["race_female2"] * Census[[t]]$race_female_2)
                                              + (fixef(M)["race_female3"] * Census[[t]]$race_female_3)
                                              + (fixef(M)["race_female4"] * Census[[t]]$race_female_4)
                                              + (fixef(M)["race_female5"] * Census[[t]]$race_female_5)
                                              + (fixef(M)["race_female6"] * Census[[t]]$race_female_6)
                                              + (fixef(M)["race_female7"] * Census[[t]]$race_female_7)
                                              + (fixef(M)["race_female8"] * Census[[t]]$race_female_8)
                                              + (fixef(M)["age_cat2"] * Census[[t]]$age_cat_2)
                                              + (fixef(M)["age_cat3"] * Census[[t]]$age_cat_3)
                                              + (fixef(M)["age_cat4"] * Census[[t]]$age_cat_4)
                                              + (fixef(M)["edu_cat2"] * Census[[t]]$edu_cat_2)
                                              + (fixef(M)["edu_cat3"] * Census[[t]]$edu_cat_3)
                                              + (fixef(M)["edu_cat4"] * Census[[t]]$edu_cat_4)
                                              + stranefs[[year[t]]][[outcome[i]]][Census[[t]]$stnum,1]
                                              + (fixef(M)["region2"] * Census[[t]]$region_2)
                                              + (fixef(M)["region3"] * Census[[t]]$region_3)
                                              + (fixef(M)["region4"] * Census[[t]]$region_4)
                                              + (fixef(M)[cong_dens[i]] * Census[[t]][,cong_dens[i]])
    ))
  }
  
  # For religious attendance and strong identification: 
  for (i in 6:8) {
    M <- model[[year[t]]][[outcome[i]]] 
    cellpred[[year[t]]][[outcome[i]]] <- invlogit(fixef(M)["(Intercept)"]
                                       + (fixef(M)["race_female2"] * Census[[t]]$race_female_2)
                                       + (fixef(M)["race_female3"] * Census[[t]]$race_female_3)
                                       + (fixef(M)["race_female4"] * Census[[t]]$race_female_4)
                                       + (fixef(M)["race_female5"] * Census[[t]]$race_female_5)
                                       + (fixef(M)["race_female6"] * Census[[t]]$race_female_6)
                                       + (fixef(M)["race_female7"] * Census[[t]]$race_female_7)
                                       + (fixef(M)["race_female8"] * Census[[t]]$race_female_8)
                                       + (fixef(M)["age_cat2"] * Census[[t]]$age_cat_2)
                                       + (fixef(M)["age_cat3"] * Census[[t]]$age_cat_3)
                                       + (fixef(M)["age_cat4"] * Census[[t]]$age_cat_4)
                                       + (fixef(M)["edu_cat2"] * Census[[t]]$edu_cat_2)
                                       + (fixef(M)["edu_cat3"] * Census[[t]]$edu_cat_3)
                                       + (fixef(M)["edu_cat4"] * Census[[t]]$edu_cat_4)
                                       + stranefs[[year[t]]][[outcome[i]]][Census[[t]]$stnum,1]
                                       + (fixef(M)["region2"] * Census[[t]]$region_2)
                                       + (fixef(M)["region3"] * Census[[t]]$region_3)
                                       + (fixef(M)["region4"] * Census[[t]]$region_4)
                                       + (fixef(M)["evancng"] * Census[[t]]$evancng)
                                       + (fixef(M)["maincng"] * Census[[t]]$maincng)
                                       + (fixef(M)["cathcng"] * Census[[t]]$cathcng)
                                       + (fixef(M)["othcng"] * Census[[t]]$othcng)
                                       + (fixef(M)["repvote"] * Census[[t]]$repvote)
    )
  }
  
}
  
  
  
########################################################################
###   5.5. Weighting predictions by prevalence of respondent types   ###
########################################################################


for (t in 1:4) {

  for (i in 1:8) {
    cellprwt[[year[t]]][[outcome[i]]] <- 
      as.vector(cellpred[[year[t]]][[outcome[i]]] * Census[[t]]$prop_state)
  }
  
}



#################################################################################
###   5.6. Aggregating weighted predictions for each state-year combination   ###
#################################################################################


for (t in 1:4) {
  
  for (i in 1:8) {
    estimate[[year[t]]][[outcome[i]]] <- 
      100 * as.vector(tapply(cellprwt[[year[t]]][[outcome[i]]], 
                             Census[[t]]$stnum, sum))
  }

}



####################################################
###   5.7. Collecting all estimates in a table   ###
####################################################


StateYear1990 <- expand.grid(State = Statelevel_key$stname, Year = 1990)
StateYear2001 <- expand.grid(State = Statelevel_key$stname, Year = 2001)
StateYear2008 <- expand.grid(State = Statelevel_key$stname, Year = 2008)
StateYear2014 <- expand.grid(State = Statelevel_key$stname, Year = 2014)

rMRP1990 <- data.frame(StateYear1990, estimate[["1990"]])
rMRP2001 <- data.frame(StateYear2001, estimate[["2001"]])
rMRP2008 <- data.frame(StateYear2008, estimate[["2008"]])
rMRP2014 <- data.frame(StateYear2014, estimate[["2014"]])

rMRP <- rbind(rMRP1990, rMRP2001, rMRP2008, rMRP2014) 



####################################################
###   5.8. Saving the table as a Stata dataset   ###
####################################################


write_dta(rMRP, "Repeated MRP estimates.dta", version = 14)
