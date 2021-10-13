##############################################################
###   4. Dynamic MRP analyses underlying our key results   ###
##############################################################



# This script implements the Dynamic MRP analyses described in the methods 
# section of the paper. It represents the foundation of the results reported 
# in Tables A3 & A4 and Figures 1-5, although the results reported in Figures 
# 1 & 2 involve comparisons with further data (see scripts 5, 6, 7, and 8). 

# The script is based on the cumulative GSS dataset 1973-2018 and merges in
# information from the Census/ACS, RCMS, and American Presidency Project 
# (see the files "01 - Preparation GSS data.do", "02 - Preparation state-level 
# data.do", and "03 - Preparation poststratification data.do" for details). 

# The script proceeds in the following steps: 
#   4.1. Loading required packages; 
#   4.2. Data preparation;
#   4.3. Multilevel regressions; 
#   4.4. Obtaining predicted probabilities for each respondent type; 
#   4.5. Weighting predictions by prevalence of respondent types;
#   4.6. Aggregating weighted predictions for each state-year combination;
#   4.7. Repeating last 3 steps for the simulated parameter estimates;
#   4.8. Calculating 95% confidence bounds for each estimate;
#   4.9. Collecting all estimates in a table;
#   4.10. Saving the table as a Stata dataset. 



##########################################
###   4.1. Loading required packages   ###
##########################################


rm(list=ls(all=TRUE))

library("arm")
library("foreign")
library("haven")
library("fastDummies")



#################################
###   4.2. Data preparation   ###
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
Year_key <- read_dta("year identifiers crosswalk.dta")
  # Both of these files, containing state and year identifiers, are available 
  #   in the replication repository: https://github.com/dingemanwiertz/nones.  
  

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

  # At Census level (same coding and definitions as above):
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


# Create  vectors including object names that we later draw upon: 
cong_dens <- c("", "evancng", "maincng", "blackcng", "cathcng")
outcome <- c("None", "Evangelical", "Mainline", "BlackProt", "Catholic", 
             "WeeklyAttend", "NeverAttend", "StrongID")


# Create lists that we will store our results in: 
model <- list()
mod_sum1 <- list()
mod_sum2 <- list()
mod_sims <- list()

cellpred <- list()
cellprwt <- list()
estimate <- list()

cellpred_sim <- list() 
cellprwt_sim <- list()
estimate_sim <- list()
estimate_bounds <- list()



#######################################
###   4.3. Multilevel regressions   ###
#######################################


# For religious nones: 
model[["None"]] <- glmer(formula = None ~ 
                            race_female + age_cat + edu_cat 
                            + (1 + year90 | stname) + region 
                            + evancng + maincng + cathcng + othcng 
                            + (1 | year_rec) + year90, 
                                data=GSS, family=binomial(link="logit"), 
                                control=glmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=1e6)), nAGQ = 1)
mod_sum1[["None"]] <- summary(model[["None"]]) 
mod_sum2[["None"]] <- c(isSingular(model[["None"]]),
                        summary(rePCA(model[["None"]])))
mod_sims[["None"]] <- sim(model[["None"]], n=1000)

# For Christian denominations: 
for (i in 2:5) {
  f <- as.formula(paste(
                    outcome[i], 
                    "~ race_female + age_cat + edu_cat", 
                    "+ (1 + year90 | stname) + region +", 
                    cong_dens[i],
                    "+ (1 | year_rec) + year90" ))
  model[[outcome[i]]] <- glmer(formula = f,
                                  data=GSS, family=binomial(link="logit"), 
                                  control=glmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=1e6)), nAGQ = 1)
  mod_sum1[[outcome[i]]] <- summary(model[[outcome[i]]])
  mod_sum2[[outcome[i]]] <- c(isSingular(model[[outcome[i]]]),
                              summary(rePCA(model[[outcome[i]]])))
  mod_sims[[outcome[i]]] <- sim(model[[outcome[i]]], n=1000)
}

# For religious attendance and identification: 
for (i in 6:8) {
  f <- as.formula(paste(
                    outcome[i], 
                    "~ race_female + age_cat + edu_cat", 
                    "+ (1 + year90 | stname) + region +", 
                    "+ evancng + maincng + cathcng + othcng + repvote", 
                    "+ (1 | year_rec) + year90" ))
  model[[outcome[i]]] <- glmer(formula = f,
                                  data=GSS, family=binomial(link="logit"), 
                                  control=glmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=1e6)), nAGQ = 1)
  mod_sum1[[outcome[i]]] <- summary(model[[outcome[i]]])
  mod_sum2[[outcome[i]]] <- c(isSingular(model[[outcome[i]]]),
                              summary(rePCA(model[[outcome[i]]])))
  mod_sims[[outcome[i]]] <- sim(model[[outcome[i]]], n=1000)
}

# Printing model summaries: 
mod_sum1 
mod_sum2 



###########################################################################
###   4.4. Obtaining predicted probabilities for each respondent type   ###
###########################################################################


# For religious nones (point predictions): 
M <- model[["None"]]
cellpred[["None"]] <- invlogit(fixef(M)["(Intercept)"]
                          + (fixef(M)["race_female2"] * Census$race_female_2)
                          + (fixef(M)["race_female3"] * Census$race_female_3)
                          + (fixef(M)["race_female4"] * Census$race_female_4)
                          + (fixef(M)["race_female5"] * Census$race_female_5)
                          + (fixef(M)["race_female6"] * Census$race_female_6)
                          + (fixef(M)["race_female7"] * Census$race_female_7)
                          + (fixef(M)["race_female8"] * Census$race_female_8)
                          + (fixef(M)["age_cat2"] * Census$age_cat_2)
                          + (fixef(M)["age_cat3"] * Census$age_cat_3)
                          + (fixef(M)["age_cat4"] * Census$age_cat_4)
                          + (fixef(M)["edu_cat2"] * Census$edu_cat_2)
                          + (fixef(M)["edu_cat3"] * Census$edu_cat_3)
                          + (fixef(M)["edu_cat4"] * Census$edu_cat_4)
                          + ranef(M)$stname[Census$stnum,1]
                          + (fixef(M)["region2"] * Census$region_2)
                          + (fixef(M)["region3"] * Census$region_3)
                          + (fixef(M)["region4"] * Census$region_4)
                          + (fixef(M)["evancng"] * Census$evancng)
                          + (fixef(M)["maincng"] * Census$maincng)
                          + (fixef(M)["cathcng"] * Census$cathcng)
                          + (fixef(M)["othcng"] * Census$othcng)
                          + ranef(M)$year_rec[Census$year_rec,1]
                          + ((fixef(M)["year90"] 
                              + ranef(M)$stname[Census$stnum,2]) * Census$year90)
)

# For Christian denominations (point predictions): 
for (i in 2:5) {
  M <- model[[outcome[i]]]
  cellpred[[outcome[i]]] <- unlist(invlogit(fixef(M)["(Intercept)"]
                                  + (fixef(M)["race_female2"] * Census$race_female_2)
                                  + (fixef(M)["race_female3"] * Census$race_female_3)
                                  + (fixef(M)["race_female4"] * Census$race_female_4)
                                  + (fixef(M)["race_female5"] * Census$race_female_5)
                                  + (fixef(M)["race_female6"] * Census$race_female_6)
                                  + (fixef(M)["race_female7"] * Census$race_female_7)
                                  + (fixef(M)["race_female8"] * Census$race_female_8)
                                  + (fixef(M)["age_cat2"] * Census$age_cat_2)
                                  + (fixef(M)["age_cat3"] * Census$age_cat_3)
                                  + (fixef(M)["age_cat4"] * Census$age_cat_4)
                                  + (fixef(M)["edu_cat2"] * Census$edu_cat_2)
                                  + (fixef(M)["edu_cat3"] * Census$edu_cat_3)
                                  + (fixef(M)["edu_cat4"] * Census$edu_cat_4)
                                  + ranef(M)$stname[Census$stnum,1]
                                  + (fixef(M)["region2"] * Census$region_2)
                                  + (fixef(M)["region3"] * Census$region_3)
                                  + (fixef(M)["region4"] * Census$region_4)
                                  + (fixef(M)[cong_dens[i]] * Census[,cong_dens[i]])
                                  + ranef(M)$year_rec[Census$year_rec,1]
                                  + ((fixef(M)["year90"]
                                      + ranef(M)$stname[Census$stnum,2]) * Census$year90)
  ))
}

# For religious attendance (point predictions): 
for (i in 6:7) {
  M <- model[[outcome[i]]]
  cellpred[[outcome[i]]] <- invlogit(fixef(M)["(Intercept)"]
                                    + (fixef(M)["race_female2"] * Census$race_female_2)
                                    + (fixef(M)["race_female3"] * Census$race_female_3)
                                    + (fixef(M)["race_female4"] * Census$race_female_4)
                                    + (fixef(M)["race_female5"] * Census$race_female_5)
                                    + (fixef(M)["race_female6"] * Census$race_female_6)
                                    + (fixef(M)["race_female7"] * Census$race_female_7)
                                    + (fixef(M)["race_female8"] * Census$race_female_8)
                                    + (fixef(M)["age_cat2"] * Census$age_cat_2)
                                    + (fixef(M)["age_cat3"] * Census$age_cat_3)
                                    + (fixef(M)["age_cat4"] * Census$age_cat_4)
                                    + (fixef(M)["edu_cat2"] * Census$edu_cat_2)
                                    + (fixef(M)["edu_cat3"] * Census$edu_cat_3)
                                    + (fixef(M)["edu_cat4"] * Census$edu_cat_4)
                                    + ranef(M)$stname[Census$stnum,1]
                                    + (fixef(M)["region2"] * Census$region_2)
                                    + (fixef(M)["region3"] * Census$region_3)
                                    + (fixef(M)["region4"] * Census$region_4)
                                    + (fixef(M)["evancng"] * Census$evancng)
                                    + (fixef(M)["maincng"] * Census$maincng)
                                    + (fixef(M)["cathcng"] * Census$cathcng)
                                    + (fixef(M)["othcng"] * Census$othcng)
                                    + (fixef(M)["repvote"] * Census$repvote)
                                    + ranef(M)$year_rec[Census$year_rec,1]
                                    + ((fixef(M)["year90"]
                                        + ranef(M)$stname[Census$stnum,2]) * Census$year90)
  )
}

# For strong identification (point predictions): 
# (incl. tweak to RE matrix because StrongID was not measured in 1973)
year.ranefs.StrongID <- array(NA, c(31,1))
for (t in 2:31) {
  year.ranefs.StrongID[t,1] <- ranef(model[["StrongID"]])$year_rec[t-1,1]
}
M <- model[["StrongID"]]
cellpred[["StrongID"]] <- invlogit(fixef(M)["(Intercept)"]
                               + (fixef(M)["race_female2"] * Census$race_female_2)
                               + (fixef(M)["race_female3"] * Census$race_female_3)
                               + (fixef(M)["race_female4"] * Census$race_female_4)
                               + (fixef(M)["race_female5"] * Census$race_female_5)
                               + (fixef(M)["race_female6"] * Census$race_female_6)
                               + (fixef(M)["race_female7"] * Census$race_female_7)
                               + (fixef(M)["race_female8"] * Census$race_female_8)
                               + (fixef(M)["age_cat2"] * Census$age_cat_2)
                               + (fixef(M)["age_cat3"] * Census$age_cat_3)
                               + (fixef(M)["age_cat4"] * Census$age_cat_4)
                               + (fixef(M)["edu_cat2"] * Census$edu_cat_2)
                               + (fixef(M)["edu_cat3"] * Census$edu_cat_3)
                               + (fixef(M)["edu_cat4"] * Census$edu_cat_4)
                               + ranef(M)$stname[Census$stnum,1]
                               + (fixef(M)["region2"] * Census$region_2)
                               + (fixef(M)["region3"] * Census$region_3)
                               + (fixef(M)["region4"] * Census$region_4)
                               + (fixef(M)["evancng"] * Census$evancng)
                               + (fixef(M)["maincng"] * Census$maincng)
                               + (fixef(M)["cathcng"] * Census$cathcng)
                               + (fixef(M)["othcng"] * Census$othcng)
                               + (fixef(M)["repvote"] * Census$repvote)
                               + year.ranefs.StrongID[Census$year_rec,1]
                               + ((fixef(M)["year90"]
                                   + ranef(M)$stname[Census$stnum,2]) * Census$year90)
)



########################################################################
###   4.5. Weighting predictions by prevalence of respondent types   ###
########################################################################


# For point predictions: 
for (i in 1:8) {
  cellprwt[[outcome[i]]] <- as.vector(cellpred[[outcome[i]]] * Census$prop_state)
}



#################################################################################
###   4.6. Aggregating weighted predictions for each state-year combination   ###
#################################################################################


# For point predictions: 
for (i in 1:8) {
  estimate[[outcome[i]]] <- 100 * as.vector(tapply(cellprwt[[outcome[i]]], 
                                                  list(Census$stnum, Census$year_rec), sum))
}



#############################################################################
###   4.7. Repeating last 3 steps for the simulated parameter estimates   ###
#############################################################################


# Steps:  1) Obtaining cell predictions; 
#         2) Weighting cell predictions; 
#         3) Aggregating weighted predictions. 


# For religious nones (simulations): 

    # Step 1: 
      cellpred_sim[["None"]] <- matrix(NA, 202368, 1000) 
      M <- attributes(mod_sims[["None"]])
      for (k in 1:1000) {
        cellpred_sim[["None"]][,k] <- invlogit(M$fixef[k,"(Intercept)"] 
                               + (M$fixef[k,"race_female2"] * Census$race_female_2)
                               + (M$fixef[k,"race_female3"] * Census$race_female_3)
                               + (M$fixef[k,"race_female4"] * Census$race_female_4)
                               + (M$fixef[k,"race_female5"] * Census$race_female_5)
                               + (M$fixef[k,"race_female6"] * Census$race_female_6)
                               + (M$fixef[k,"race_female7"] * Census$race_female_7)
                               + (M$fixef[k,"race_female8"] * Census$race_female_8)
                               + (M$fixef[k,"age_cat2"] * Census$age_cat_2)
                               + (M$fixef[k,"age_cat3"] * Census$age_cat_3)
                               + (M$fixef[k,"age_cat4"] * Census$age_cat_4)
                               + (M$fixef[k,"edu_cat2"] * Census$edu_cat_2)
                               + (M$fixef[k,"edu_cat3"] * Census$edu_cat_3)
                               + (M$fixef[k,"edu_cat4"] * Census$edu_cat_4)
                               + M$ranef$stname[k,Census$stnum,1]
                               + (M$fixef[k,"region2"] * Census$region_2)
                               + (M$fixef[k,"region3"] * Census$region_3)
                               + (M$fixef[k,"region4"] * Census$region_4)
                               + (M$fixef[k,"evancng"] * Census$evancng)
                               + (M$fixef[k,"maincng"] * Census$maincng)
                               + (M$fixef[k,"cathcng"] * Census$cathcng)
                               + (M$fixef[k,"othcng"] * Census$othcng)
                               + M$ranef$year_rec[k,Census$year_rec,1]
                               + ((M$fixef[k,"year90"]
                                   + M$ranef$stname[k,Census$stnum,2]) * Census$year90)
        )
      }

    # Step 2: 
      cellprwt_sim[["None"]] <- matrix(NA, 202368, 1000)
      for (k in 1:1000) {
        cellprwt_sim[["None"]][,k] <- as.vector(cellpred_sim[["None"]][,k] * Census$prop_state)
      }
      cellpred_sim[["None"]] <- NULL

    # Step 3: 
      estimate_sim[["None"]] <- matrix(NA, 1581, 1000)
      for (k in 1:1000) {
        estimate_sim[["None"]][,k] <- 100 * as.vector(tapply(cellprwt_sim[["None"]][,k], 
                                                      list(Census$stnum, Census$year_rec), sum))
      }
      cellprwt_sim[["None"]] <- NULL


# For Christian denominations (simulations): 

  for (i in 2:5) {
    
    # Step 1: 
      cellpred_sim[[outcome[i]]] <- matrix(NA, 202368, 1000) 
      M <- attributes(mod_sims[[outcome[i]]])
      for (k in 1:1000) {
        cellpred_sim[[outcome[i]]][,k] <- unlist(invlogit(M$fixef[k,"(Intercept)"] 
                                        + (M$fixef[k,"race_female2"] * Census$race_female_2)
                                        + (M$fixef[k,"race_female3"] * Census$race_female_3)
                                        + (M$fixef[k,"race_female4"] * Census$race_female_4)
                                        + (M$fixef[k,"race_female5"] * Census$race_female_5)
                                        + (M$fixef[k,"race_female6"] * Census$race_female_6)
                                        + (M$fixef[k,"race_female7"] * Census$race_female_7)
                                        + (M$fixef[k,"race_female8"] * Census$race_female_8)
                                        + (M$fixef[k,"age_cat2"] * Census$age_cat_2)
                                        + (M$fixef[k,"age_cat3"] * Census$age_cat_3)
                                        + (M$fixef[k,"age_cat4"] * Census$age_cat_4)
                                        + (M$fixef[k,"edu_cat2"] * Census$edu_cat_2)
                                        + (M$fixef[k,"edu_cat3"] * Census$edu_cat_3)
                                        + (M$fixef[k,"edu_cat4"] * Census$edu_cat_4)
                                        + M$ranef$stname[k,Census$stnum,1]
                                        + (M$fixef[k,"region2"] * Census$region_2)
                                        + (M$fixef[k,"region3"] * Census$region_3)
                                        + (M$fixef[k,"region4"] * Census$region_4)
                                        + (M$fixef[k,cong_dens[i]] * Census[,cong_dens[i]])
                                        + M$ranef$year_rec[k,Census$year_rec,1]
                                        + ((M$fixef[k,"year90"]
                                            + M$ranef$stname[k,Census$stnum,2]) * Census$year90)
        ))
      }
      
    # Step 2: 
      cellprwt_sim[[outcome[i]]] <- matrix(NA, 202368, 1000)
      for (k in 1:1000) {
        cellprwt_sim[[outcome[i]]][,k] <- as.vector(cellpred_sim[[outcome[i]]][,k] * Census$prop_state)
      }
      cellpred_sim[[outcome[i]]] <- NULL
  
    # Step 3:
      estimate_sim[[outcome[i]]] <- matrix(NA, 1581, 1000)
      for (k in 1:1000) {
        estimate_sim[[outcome[i]]][,k] <- 100 * as.vector(tapply(cellprwt_sim[[outcome[i]]][,k], 
                                                            list(Census$stnum, Census$year_rec), sum))
      }
      cellprwt_sim[[outcome[i]]] <- NULL
  }


# For religious attendance (simulations): 
      
  for (i in 6:7) {
    
    # Step 1: 
      cellpred_sim[[outcome[i]]] <- matrix(NA, 202368, 1000) 
      M <- attributes(mod_sims[[outcome[i]]])
      for (k in 1:1000) {
        cellpred_sim[[outcome[i]]][,k] <- invlogit(M$fixef[k,"(Intercept)"] 
                                                     + (M$fixef[k,"race_female2"] * Census$race_female_2)
                                                     + (M$fixef[k,"race_female3"] * Census$race_female_3)
                                                     + (M$fixef[k,"race_female4"] * Census$race_female_4)
                                                     + (M$fixef[k,"race_female5"] * Census$race_female_5)
                                                     + (M$fixef[k,"race_female6"] * Census$race_female_6)
                                                     + (M$fixef[k,"race_female7"] * Census$race_female_7)
                                                     + (M$fixef[k,"race_female8"] * Census$race_female_8)
                                                     + (M$fixef[k,"age_cat2"] * Census$age_cat_2)
                                                     + (M$fixef[k,"age_cat3"] * Census$age_cat_3)
                                                     + (M$fixef[k,"age_cat4"] * Census$age_cat_4)
                                                     + (M$fixef[k,"edu_cat2"] * Census$edu_cat_2)
                                                     + (M$fixef[k,"edu_cat3"] * Census$edu_cat_3)
                                                     + (M$fixef[k,"edu_cat4"] * Census$edu_cat_4)
                                                     + M$ranef$stname[k,Census$stnum,1]
                                                     + (M$fixef[k,"region2"] * Census$region_2)
                                                     + (M$fixef[k,"region3"] * Census$region_3)
                                                     + (M$fixef[k,"region4"] * Census$region_4)
                                                     + (M$fixef[k,"evancng"] * Census$evancng)
                                                     + (M$fixef[k,"maincng"] * Census$maincng)
                                                     + (M$fixef[k,"cathcng"] * Census$cathcng)
                                                     + (M$fixef[k,"othcng"] * Census$othcng)
                                                     + (M$fixef[k,"repvote"] * Census$repvote)
                                                     + M$ranef$year_rec[k,Census$year_rec,1]
                                                     + ((M$fixef[k,"year90"]
                                                         + M$ranef$stname[k,Census$stnum,2]) * Census$year90)
        )
      }
  
    # Step 2: 
      cellprwt_sim[[outcome[i]]] <- matrix(NA, 202368, 1000)
      for (k in 1:1000) {
        cellprwt_sim[[outcome[i]]][,k] <- as.vector(cellpred_sim[[outcome[i]]][,k] * Census$prop_state)
      }
      cellpred_sim[[outcome[i]]] <- NULL
  
    # Step 3: 
      estimate_sim[[outcome[i]]] <- matrix(NA, 1581, 1000)
      for (k in 1:1000) {
        estimate_sim[[outcome[i]]][,k] <- 100 * as.vector(tapply(cellprwt_sim[[outcome[i]]][,k], 
                                                            list(Census$stnum, Census$year_rec), sum))
      }
      cellprwt_sim[[outcome[i]]] <- NULL
  }


# For strong identification (simulations): 
# (incl. tweak to RE matrix because StrongID was not measured in 1973)

    # Step 1: 
      year.ranefs.StrongID.sims <- array(NA, c(1000,31)) 
      for (t in 2:31) {
        for (k in 1:1000) {
          year.ranefs.StrongID.sims[k,t] <- attributes(mod_sims[["StrongID"]])$ranef$year_rec[k,t-1,1]
        }
      }
      cellpred_sim[["StrongID"]] <- matrix(NA, 202368, 1000) 
      M <- attributes(mod_sims[["StrongID"]])
      for (k in 1:1000) {
        cellpred_sim[["StrongID"]][,k] <- invlogit(M$fixef[k,"(Intercept)"] 
                                   + (M$fixef[k,"race_female2"] * Census$race_female_2)
                                   + (M$fixef[k,"race_female3"] * Census$race_female_3)
                                   + (M$fixef[k,"race_female4"] * Census$race_female_4)
                                   + (M$fixef[k,"race_female5"] * Census$race_female_5)
                                   + (M$fixef[k,"race_female6"] * Census$race_female_6)
                                   + (M$fixef[k,"race_female7"] * Census$race_female_7)
                                   + (M$fixef[k,"race_female8"] * Census$race_female_8)
                                   + (M$fixef[k,"age_cat2"] * Census$age_cat_2)
                                   + (M$fixef[k,"age_cat3"] * Census$age_cat_3)
                                   + (M$fixef[k,"age_cat4"] * Census$age_cat_4)
                                   + (M$fixef[k,"edu_cat2"] * Census$edu_cat_2)
                                   + (M$fixef[k,"edu_cat3"] * Census$edu_cat_3)
                                   + (M$fixef[k,"edu_cat4"] * Census$edu_cat_4)
                                   + M$ranef$stname[k,Census$stnum,1]
                                   + (M$fixef[k,"region2"] * Census$region_2)
                                   + (M$fixef[k,"region3"] * Census$region_3)
                                   + (M$fixef[k,"region4"] * Census$region_4)
                                   + (M$fixef[k,"evancng"] * Census$evancng)
                                   + (M$fixef[k,"maincng"] * Census$maincng)
                                   + (M$fixef[k,"cathcng"] * Census$cathcng)
                                   + (M$fixef[k,"othcng"] * Census$othcng)
                                   + (M$fixef[k,"repvote"] * Census$repvote)
                                   + year.ranefs.StrongID.sims[k,Census$year_rec]
                                   + ((M$fixef[k,"year90"]
                                       + M$ranef$stname[k,Census$stnum,2]) * Census$year90)
        )
      }

    # Step 2: 
      cellprwt_sim[["StrongID"]] <- matrix(NA, 202368, 1000)
      for (k in 1:1000) {
        cellprwt_sim[["StrongID"]][,k] <- as.vector(cellpred_sim[["StrongID"]][,k] * Census$prop_state)
      }
      cellpred_sim[["StrongID"]] <- NULL

    # Step 3: 
      estimate_sim[["StrongID"]] <- matrix(NA, 1581, 1000)
      for (k in 1:1000) {
        estimate_sim[["StrongID"]][,k] <- 100 * as.vector(tapply(cellprwt_sim[["StrongID"]][,k], 
                                                      list(Census$stnum, Census$year_rec), sum))
      }
      cellprwt_sim[["StrongID"]] <- NULL



####################################################################
###   4.8. Calculating 95% confidence bounds for each estimate   ###
####################################################################


for (i in 1:7) {
  estimate_bounds[[outcome[i]]] <- matrix(NA, 1581, 2)
  for (l in 1:1581) {
    estimate_bounds[[outcome[i]]][l,] <- quantile(estimate_sim[[outcome[i]]][l,], c(0.025,0.975))
  }
}
      
estimate_bounds[["StrongID"]] <- matrix(NA, 1581, 2)
for (l in 52:1581) {
  estimate_bounds[["StrongID"]][l,] <- quantile(estimate_sim[["StrongID"]][l,], c(0.025,0.975))
}

for (i in 1:8) { 
  colnames(estimate_bounds[[outcome[i]]]) <- c("lb", "ub")
}



####################################################
###   4.9. Collecting all estimates in a table   ###
####################################################


StateYear <- expand.grid(State = Statelevel_key$stname, Year = Year_key$year)

MRT <- data.frame(StateYear, estimate, estimate_bounds)

for (i in 1:8) { 
  lb_old <- paste0(outcome[i], ".lb")
  ub_old <- paste0(outcome[i], ".ub")
  lb_new <- paste0(outcome[i], "_lb")
  ub_new <- paste0(outcome[i], "_ub")
  colnames(MRT)[colnames(MRT) == lb_old] <- lb_new
  colnames(MRT)[colnames(MRT) == ub_old] <- ub_new
}



#####################################################
###   4.10. Saving the table as a Stata dataset   ###
#####################################################


write_dta(MRT, "Dynamic MRP estimates 1973-2018.dta", version = 14)
