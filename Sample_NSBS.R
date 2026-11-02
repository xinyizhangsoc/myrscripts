## Sample Script: The Black Suburban Experience Project
## Name: Xinyi Zhang
###-----------Project Description----------------------------
## This is clodes from an on-going research project on the Black Suburban Experience, where I joined original survey data with data from the U.S. Census and American Community Survey (ACS) at both ZIP code and Core Based Statistical Area-level to ascertain the geographic contexts where the survey respondents reside. Codes complete the following tasks:
##    a. Using an API from the Department of Housing and Urban Development (HUD) to ascertain ZIP-to-CBSA crosswalk files to join ZIPs in survey data with American Community Survey at the CBSA level to understand the metropolitan area characteristics where respondents reside
##    b. Using times series data from the U.S. Census to ascertain ZIP-level population changes between 1990 and 2020 and visualizing these changes
##    c. Joining survey data with data from the ACS to ascertain ZIP-level characteristics
##    d. Principle component analysis of suburban ZIP-codes in the data to create typologies of suburbs
##    e. Regression analysis with principal components on attitudes towards policing

###-------------------------------------------###
### Clear Environment
rm(list = ls())

###------------load packages------------------###
library(readr)
library(haven)
library(dplyr)
library(sjlabelled)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ggpp)
library(table1)
library(reshape2)
library(broom)
library(stringr)
library(psych)
library(factoextra)
library(Hmisc)

###---------------------------------------------------###

### Get data from the HUD USPS ZIP Code Crosswalk Files API
key <- "masked information"

url <- "https://www.huduser.gov/hudapi/public/usps"

# Note that type is set to 3 which will return values for the ZIP to CBSA file and query is set to All which will return Zip Codes the entire US

response <- httr::GET(url, query = list(type = 3, query = "All")
                      , add_headers(Authorization = paste("Bearer", key)))

#check for errors
httr::http_error(response)

#access up the output
output <- httr::content(response)

#-----------------------------------------------------
## Turning list to a ZIP-CBSA data frame
zip_cbsa <- lapply(output$data$results, data.frame) %>%
  bind_rows() %>%
  select(zip, geoid, city, state)

#-----------------------------------------------------
## Read in census file with CBSA code and MSA names
census_cbsa_msa_2023 <- read_excel("census_cbsa_msa_2023.xlsx")[-1,] %>%
  set_names(as.character(slice(., 1))) %>%
  slice(-1) 

## ZIP-CBSA-MSA name master file
names(zip_cbsa)[2] <- "cbsa"
names(census_cbsa_msa_2023)[1] <- "cbsa"
cbsa_title <- census_cbsa_msa_2023[!duplicated(census_cbsa_msa_2023$cbsa), ]

zip_cbsa_title <- left_join(zip_cbsa, cbsa_title, by = "cbsa")
zip_cbsa_title <- zip_cbsa_title[!duplicated(zip_cbsa_title$zip),]

## check unique value of ZIP code
length(unique(zip_cbsa_title$zip))

##--------------------------NSBS Data---------------------------------
## Load NSBS merged data
library(haven)
nsbs_merged <- read_dta("NSBS_Merged.dta")

## left join data set based on ZIP code
nsbs_zip_msa <- left_join(nsbs_merged, zip_cbsa_title, by = "zip") %>%
  select("zip", "cbsa", "CBSA Title")

names(nsbs_zip_msa)[3] <- "msa"

## The most common MSAs in the NSBS data
ranked_msa <- nsbs_zip_msa %>%
  group_by(msa) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(rank = dense_rank(desc(count)))


##----------------------Top MSAs by Black pop------------------------
## Import MSA-level population data to check top 10 and 25 MSAs for Black population
acs_cbsa_race <- read_dta("ACS_CSBA_Race.dta") %>%
  select("Geo_CBSA","Geo_NAME", "Geo_qname","SE_A03001_003")

ranked_msa_bk <- acs_cbsa_race %>%
  group_by(Geo_CBSA) %>%
  mutate(black_pop = across(SE_A03001_003, sum)) %>%
  arrange(desc(black_pop))

###---------------------Demographic Changes from 1990 to 2020-----------------------###
## Load NHGIS Time Series
ts_19902020_zcta <- read_csv("nhgis0006_ts_geog2010_zcta.csv")

## Compute Demographic Change
zcta_19902020 <- ts_19902020_zcta %>%
  mutate(chgpop9020 = CL8AA2020-CL8AA1990, # population change
         pctchgpop9020 = (CL8AA2020-CL8AA1990)/CL8AA1990, # pct pop change
         pctnonhiswt90 = CY8AA1990/CL8AA1990, # pct nonhispanic white
         pctnonhisbk90 = CY8AB1990/CL8AA1990, # pct nonhispanic black
         pctlatinx90 = CY8AF1990/CL8AA1990, # pct latinx
         pctaapi90 = CY8AD1990/CL8AA1990, # pct aapi
         pctnonwt90 = (CL8AA1990-CY8AA1990)/CL8AA1990, # pct nonwhite
         pctnonbk90 = (CL8AA1990-CY8AB1990)/CL8AA1990, # pct nonblack
         pctnonhiswt20 = CY8AA2020/CL8AA2020,
         pctnonhisbk20 = CY8AB2020/CL8AA2020,
         pctlatinx20 = CY8AF2020/CL8AA2020,
         pctaapi20 = CY8AD2020/CL8AA2020,
         pctnonwt20 = (CL8AA2020-CY8AA2020)/CL8AA2020,
         pctnonbk20 = (CL8AA2020-CY8AB2020)/CL8AA2020) %>%
  mutate(chgpopwt9020 = CY8AA2020-CY8AA1990, # nonhis wt pop change
         chgpopbk9020 = CY8AB2020-CY8AB1990, # nonhis bk pop change
         chgpopaapi9020 = CY8AD2020-CY8AD1990, # appi pop change
         chgpoplatinx9020 = CY8AF2020-CY8AF1990, # latinx pop chg
         chgpopnonwt9020 = (CL8AA2020-CY8AA2020)-(CL8AA1990-CY8AA1990), # nonwt pop chg
         chgpopnonbk9020 = (CL8AA2020-CY8AB2020)-(CL8AA1990-CY8AB1990), # nonbk pop chg
         chgpopnonwtbk9020 = (CL8AA2020-CY8AA2020-CY8AB2020)-(CL8AA1990-CY8AA1990-CY8AB1990) # nonwt&pop chg
  ) %>%
  mutate(zip = ZCTAA) %>%
  mutate(ttlpop90 = CL8AA1990, ttlpop20 = CL8AA2020,
         nonhiswt90 = CY8AA1990, nonhiswt20 = CY8AA2020,
         nonhisbk90 = CY8AB1990, nonhisbk20 = CY8AB2020,
         latinx90 = CY8AF1990, latinx20 = CY8AF2020,
         aapi90 = CY8AD1990, aapi20 = CY8AD2020,
         nonwt90 = CL8AA1990-CY8AA1990, nonwt20 = CL8AA2020-CY8AA2020,
         nonbk90 = CL8AA1990-CY8AB1990, nonbk20 = CL8AA2020-CY8AB2020)

## Load NSBS ZIP data
zcta_nsbs <- readRDS(file = "zips_nsbs.rds")

## Merge Data
zcta_nsbs_popchg9020 <- left_join(zcta_nsbs, zcta_19902020, by = "zip")

## Summary of Population Changes
nsbs_zippopchgsum_9020 <- zcta_nsbs_popchg9020 %>%
  select(ttlpop90, ttlpop20, nonhiswt90, nonhiswt20, nonhisbk90, nonhisbk20, latinx90,    latinx20, aapi90, aapi20, nonwt90, nonwt20, nonbk90, nonbk20, chgpopwt9020, chgpopbk9020, chgpopaapi9020, chgpoplatinx9020, chgpopnonwt9020, chgpopnonbk9020, urbsub)


## Histograms: distributions of population change by urban/suburban designation
ggplot(melt(nsbs_zippopchgsum_9020[,c(1:10, 21)]), aes(x = value, fill = urbsub, color = urbsub)) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  geom_histogram(na.rm = T,
                 position = "identity",
                 alpha = .5,
                 bins = 30) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(melt(nsbs_zippopchgsum_9020[,c(15:20, 21)]), aes(x = value, fill = urbsub, color = urbsub)) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  geom_histogram(na.rm = T,
                 position = "identity",
                 alpha = .5,
                 bins = 30) +
  theme_minimal() +
  theme(legend.title = element_blank())

## Visualize
## Black vs. Non-Black Population Changes
ggplot(zcta_nsbs_subs_9020, aes(x = chgpopbk9020, y = chgpopnonbk9020, color = bknonbkchg9020)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(-60000, 60000, by = 10000)) +
  scale_y_continuous(breaks = seq(-60000, 60000, by = 10000)) +
  labs(title = "Black and Non-Black Population Change",
       subtitle = "NSBS Suburban ZIPs, 1990-2020",
       x = "Change in Black Population",
       y = "Change in Non-Black Population",
       color = "Population Trend") +
  scale_color_discrete(na.translate = FALSE) +
  stat_quadrant_counts() +
  theme_minimal() +
  coord_fixed(ratio=1)

##--------------Additional ZIP-level Characteristics and PCR-------------##
### Load NSBS merged data
nsbs_merged <- read_dta("NSBS_Merged.dta")

### Load additional vars from IPUMS NHGIS
nhgiszips_1 <- read_csv("nhgis0001_ds267_20235_zcta.csv") %>%
  mutate(zip = ZCTA5A)
nhgiszips_2 <- read_csv("nhgis0001_ds268_20235_zcta.csv") %>%
  mutate(zip = ZCTA5A)
nhgiszips <- left_join(nhgiszips_1, nhgiszips_2, by = "zip")

### Merging two datasets
nsbs_merged$urbsub <- factor(nsbs_merged$urbsub,
                             levels = c(0, 1),
                             labels = c("Urban", "Suburban"))

nsbs_merged <- left_join(nsbs_merged, nhgiszips, by = "zip")

###-------------------------PCA of Suburban ZIPs------------------------------
### Take only suburban ZIPs
nsbs_suburban <- filter(nsbs_merged, urbsub == "Suburban") # 702 obs

### To be included in the PCA
nsbs_subs_pca <- nsbs_suburban %>%
  select(ttlpop, popdense, medianage, avghhsize, pctnonhiswt, pctnonhisbk, pctlatinx, pctforeign, pctlths, pctba, pctunemploy, medianinc, pctfambelowpov, avgcomtime, medianbuilt, pctownerhome, tenure)

### Scaling the data
nsbs_subs_pca_scaled <- scale(nsbs_subs_pca)

### Optimal number of components
## Use Scree Plot to identify optimal number of components (we use a threshold hold Eigenvalue of 1 to generate)

## Omitting missing values
nsbs_scree <- na.omit(nsbs_subs_pca_scaled)
VSS.scree(cor(nsbs_scree)) # 5 components

### Run principal component analysis
nsbs_pca <- principal(nsbs_subs_pca_scaled, 
                      nfactors = 5, rotate = "varimax", scores=TRUE)

### Results
summary(nsbs_pca)
print(nsbs_pca$loadings)

### Extract scores for each component
nsbs_pca_scores <- as.data.frame(nsbs_pca$scores)

### Data frame for PCA regression
## Select variables
regdata_nopc <- nsbs_suburban %>%
  select(urbsub, policebrutal, Police_good, Police_discrimpoor, Police_discrimblack, Police_discrimlatinx,
         Police_xexplain, Police_abusepower, Police_quickrespond, Police_preventcrime, Police_trust,
         Police_crimevictim, Police_represent, Police_comp, # outcome vars
         ComQual, ComSafe, ComEnvrmt_belong, ComInst_govrn, # individual-level assessments of comm
         educ4, region4, age, female, # individual characteristics
         ttlpop, popdense, medianage, avghhsize, pctnonhiswt, pctnonhisbk, pctlatinx, pctforeign, pctlths, pctba, pctunemploy, medianinc, pctfambelowpov, avgcomtime, medianbuilt, pctownerhome, tenure,
         lnttlpop, lnpopdense, lnpctnonhiswt, lnpctnonhisbk, lnpctlatinx, lnpctforeign, lnmedianinc, lnpctlths, lnpctba,
         lnpctunemploy, lnpctfambelowpov, lnpctownerhome) # zip-level vars


### Dataset2 adding PCs
regdata_pc <- cbind(regdata_nopc, nsbs_pca_scores)

### Running OLS
## Outcome vars
outcomes <- c("policebrutal", "Police_good", "Police_discrimpoor", "Police_discrimblack", "Police_discrimlatinx",
              "Police_xexplain", "Police_abusepower", "Police_quickrespond", "Police_preventcrime", "Police_trust",
              "Police_crimevictim", "Police_represent", "Police_comp")

## OLS with PCs
models_pc <- lapply(outcomes, function(y_var){
  
  
  formula_str <- as.formula(paste(y_var, "~ ComQual + ComSafe + ComEnvrmt_belong + ComInst_govrn + educ4 + female + region4 + age + RC1 + RC2 + RC4 + RC3 + RC5"))
  lm(formula_str, data = regdata_pc, na.omit = T)
})

names(models_pc) <- outcomes

modelsummary(models_pc,
             stars = T,
             gof_omit = "AIC|BIC|Log.Lik|RMSE",
             output = "out_pc.xlsx")

