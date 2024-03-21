# Economics MSc dissertation on zombie firms
# Michael Hollins
# Last updated: 24/09/19

############################## Starting steps #################################

# Preliminaries -----------------------------------------------------------

# Clear environment 
rm(list=ls())

# Load packages
library(plm)
library(Matrix)
library(MatrixModels)
library(data.table)
library(readxl)
library(dplyr)
library(quantmod)
library(purrr)
library(tidyr)
library(stringr)
library(openxlsx)
library(lfe)
library(ggplot2)
library(pixiedust)
library(texreg)
# library(lmtest)

# Set working directory
setwd('C:/Users/mhollins1/Documents/msc_thesis/clean_data/')

# Define global variables and paths
raw_data_dir = 'C:/Users/mhollins1/Documents/msc_thesis/raw_data/'
dollar_index_dir = str_c('C:/Users/mhollins1/Documents/msc_thesis/reference_data/', 
                         'dollar_index.xlsx')
clean_data_path = 'C:/Users/mhollins1/Documents/msc_thesis/clean_data/'
macro_path = 'macro.csv'
interest_rates_path = 'interest_rates.csv'
output_dir = 'C:/Users/mhollins1/Documents/msc_thesis/outputs/'
sic_codes = paste0('C:/Users/mhollins1/Documents/msc_thesis/reference_data/',
                   'sic_2_digit_codes.csv')

## Helper functions

as.year <- function(x) floor(as.numeric(as.yearmon(x))) # to aggregate as year

winsor <- function (x, fraction=.05, tails=NA) {
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction),na.rm=T)
  if (!(tails%in%c("twosided","lower","upper"))) {
    stop("bad value for 'tails'")
  }
  if (tails=="twosided") {
    x[ x < lim[1] ] <- lim[1]
    x[ x > lim[2] ] <- lim[2]
  } else if (tails=="lower") {
    x[ x < lim[1] ] <- lim[1]
  } else if (tails=="upper") {
    x[ x > lim[2] ] <- lim[2]
  }
  x
}

# Load data ---------------------------------------------------------------

# Get files

files = list.files(raw_data_dir)

load_data = function(df, file_list, file_number) {
  
  # Define lists to structure the dataframe from raw Datastream call
  column_types = c('text', 'text', 'text', 'numeric', 'numeric', 'numeric', 
                   'text', 'numeric','numeric', 'numeric', 'numeric', 
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
  
  year_cols = as.character(c(1980:2018))
  
  worldscope_fields = c('WC02999', 'WC01001', 'WC02001', 'WC03501', 'WC03351',
                        'WC03051', 'WC03251', 'WC18198', 'WC01251', 'WC01451',
                        'WC05376', 'WC01201', 'WC02649', 'WC01101', 'WC02301',
                        'WC08001', 'WC02201', 'WC04601', 'WC01151', 'WC03255',
                        'WC01084', 'WC07011', 'WC04440', 'WC04551', 'WC18191',
                        'WC07230', 'WC04251', 'WC04351', 'WC02501', 'WC18195',
                        'WC01100')
  
  fields = length(worldscope_fields)
  times = length(year_cols)
  n_rows_per_firm = fields*times
  
  ##
  file = str_c(raw_data_dir, file_list[[file_number]])
  
  sheets = excel_sheets(file)
  
  df = map_df(sheets, ~ read_excel(file, 
                                   sheet = .x, 
                                   col_types = column_types,
                                   na = c('NA', 
                                          '$$ER: 4540,NO DATA VALUES FOUND',
                                          '$$ER: E100,INVALID CODE OR EXPRESSION ENTERED',
                                          '$$ER: E100,NO WORLDSCOPE DATA FOR THIS CODE')))
  
  df$Name = worldscope_fields
  
  static_vars = c('Name', 'WC06105', 'WC06001', 'WC07021', 'WC18272', 'WC18273', 
                  'WC06100')
  
  df = gather(df, year, value, -c(static_vars))
  df$year = as.numeric(df$year)
  
  # Drop any possible duplicated / rows ahead of the spread
  df = filter(df, !is.na(WC06105))
  
  df = df %>%
    distinct(Name, WC06105, year, .keep_all = TRUE) 
  # because rows sometimes only differ on value so the spread won't work
  
  df = spread(df, Name, value)
  
  # Create country column
  df = df %>%
    mutate(iso = str_remove(file_list[[file_number]], '\\.xlsx'))
  
  df = data.table(df)
  setkeyv(df, c("WC06105", "year"))
}

australia = load_data(df, files, 1)
belgium = load_data(df, files, 2)
canada = load_data(df, files, 3)
switzerland = load_data(df, files, 4)
denmark = load_data(df, files, 5)
spain = load_data(df, files, 6)
france = load_data(df, files, 7)
uk = load_data(df, files, 8)
germany = load_data(df, files, 9)
italy = load_data(df, files, 10)
japan = load_data(df, files, 11)
netherlands = load_data(df, files, 12)
sweden = load_data(df, files, 13)
usa = load_data(df, files, 14)

df = rbind(australia, belgium, canada, denmark, france, germany, 
           italy, japan, netherlands, spain, sweden, switzerland, uk, usa)

dat <- data.table(df)
setkeyv(dat, c("WC06105","year"))
temp = dat




######################### Cleaning and prepping data ##########################

# Clean original data call ------------------------------------------------

# drop ADRs from US data
dat <- subset(dat, WC06100!='A')

# SIC codes for industry classification at various levels
dat$SIC4 <- sprintf("%04d", dat$WC07021)
dat$SIC2 <- gsub('.{2}$', '', dat$SIC4)
dat$SIC1 <- gsub('.{1}$', '', dat$SIC4)

# I want firms with positive assets and revenue
dat <- subset(dat, WC02999>1e-6) # positive total assets
dat <- subset(dat, WC07230 > 1e-6) # positive total assets in dollars
dat <- subset(dat, WC01001>1e-6) # positive net sales

# I want to just study non-financial firms to get a more 'fundamentals' view
dat <- subset(dat, WC07021<6000 | WC07021>6999) # Drop financial firms
dat <- subset(dat, WC07021<4900 | WC07021>4999) # Drop utilities

# Remove observations where key variables/ denominators are missing
dat<- subset(dat, !is.na(WC02001)) #cash and equivalents
dat<- subset(dat, !is.na(WC02999)) #total assets
dat<- subset(dat, !is.na(WC01001)) #net sales
dat<- subset(dat, !is.na(WC03501)) #common equity
dat<- subset(dat, !is.na(WC03351)) #total liabilities
dat<- subset(dat, WC03351 >= 0) # non-negative liabilities


# Join macroeconomic OECD data --------------------------------------------

macro_data_path = paste0('C:/Users/mhollins1/Documents/msc_thesis/clean_data/',
                         'macroeconomic_variables_to_join.csv')
macro_data = as.data.table(readr::read_csv(macro_data_path))
macro_data[, X1:=NULL]
names(macro_data) = tolower(names(macro_data))

# Sometimes interest rates are only available from one call from OECD so merge cols
macro_data <- macro_data[is.na(intr_sr) & !is.na(irs), intr_sr := irs, ]
dat = merge(dat, macro_data, by=c('year', 'iso'), all.x = TRUE)
setkeyv(dat, c("WC06105","year"))

# Also add bank price to book ratio averages to proxy for bank health from BIS
# Kindly sent to me from Ryan Banerjee - consider doing an in-sample derivation

bank_p2b_path = paste0('C:/Users/mhollins1/Documents/msc_thesis/reference_data/',
                       'bank_p2b_ratios.xlsx')
bank_p2b = read_excel(bank_p2b_path, sheet = 'PBbanks2')
bank_p2b$year = as.year(bank_p2b$year)
bank_p2b_long = melt(bank_p2b, id.vars='year', variable.name='cc',
                     value.name='avg_p2b_ratio')
dat = merge(dat, bank_p2b_long, by=c('year', 'cc'), all.x = TRUE)
setkeyv(dat, c("WC06105","year"))

# Adding in labour productivity growth
# Read in labour productivity data from OECD and transform
lab_prod <- as.data.table(read.csv(
  'C:/Users/mhollins1/Documents/msc_thesis/reference_data/oecd_lab_prod.csv', 
  stringsAsFactors = F))
lab_prod <- lab_prod[LOCATION %in% c('AUS', 'BEL', 'CAN', 'CHE',
                                     'DEU',  'DNK', 'ESP', 'FRA', 
                                     'GBR','ITA', 'JPN', 'NLD', 
                                     'SWE', 'USA'), c('LOCATION', 'TIME', 'Value')]
names(lab_prod) <- c('iso', 'year', 'lp')
setkeyv(lab_prod, cols=c('iso', 'year'))
lab_prod[, d_lp := lp - shift(lp), by=iso]
lab_prod[, lp_growth := d_lp/lp]
lab_prod <- lab_prod[, c('iso', 'year', 'lp_growth')]
dat = merge(dat, lab_prod, by=c('year', 'iso'), all.x = TRUE)
setkeyv(dat, c("WC06105","year"))

op_income_dt <- as.data.table(read_excel(paste0(
  'C:/Users/mhollins1/Documents/msc_thesis/reference_data/', 'op_income.xlsx'),
  na = c('NA', 
         '$$ER: 4540,NO DATA VALUES FOUND',
         '$$ER: E100,INVALID CODE OR EXPRESSION ENTERED',
         '$$ER: E100,NO WORLDSCOPE DATA FOR THIS CODE')))
op_income_dt = op_income_dt[, Name:=NULL]
op_income_dt = subset(op_income_dt, !is.na(WC06105))
op_income_dt = melt(op_income_dt, 'WC06105', 2:40, 'year', 'WC04001')
op_income_dt$year <- as.character(op_income_dt$year)
op_income_dt$year <- as.numeric(op_income_dt$year)
op_income_dt$WC06105 <- as.character(op_income_dt$WC06105)
dat = merge(dat, op_income_dt, by=c('year', 'WC06105'), all.x = TRUE)
setkeyv(dat, c("WC06105","year"))

# Cash flows and leverage -----------------------------------------

dat$cashratio <- dat$WC02001/dat$WC02999 # cash ratio
dat$leverage <- (dat$WC03351)/dat$WC02999 # Leverage
dat$netleverage <- (dat$WC03051 + dat$WC03251 - dat$WC02001)/dat$WC02999
# This definition is important for the regressions later
dat$realsize <- dat$WC02999/dat$cpi


### Cash flow
# Definition in Bates et al. (2009), Journal of Finance 64(5)
# EBITDA - interest - taxes - common dividends
# This has fewer NAs for US
dat[, cashflow2:= WC18198 - WC01251 - WC01451 - WC05376]
dat[, cashflow2 :=  cashflow2/WC02999] 

dat_vol <- dat
dat_vol[, cashflow_sd :=rollapply(cashflow2, width=10, FUN=sd, partial=TRUE, 
                                  align='right',na.rm=T), by = "WC06105"]

# Test if at least 3 firm obs used to compute the standard deviation
dat_vol[,D_cashflow:=ifelse(!is.na(cashflow2),1,NA)]
dat_vol[,l.D_cashflow:= c(NA, head(D_cashflow,-1)),by="WC06105"]
dat_vol[,l2.D_cashflow:= c(NA, head(l.D_cashflow,-1)),by="WC06105"]
dat_vol[,D_cashflow_3obs := D_cashflow + l.D_cashflow + l2.D_cashflow]
# Select only SD obs computed with at least three obs
dat_vol[, cashflow_sd := ifelse(D_cashflow_3obs>2, cashflow_sd, NA)] 
dat_vol[, cashflow_sd_SIC2 := mean(cashflow_sd, na.rm=T),by=c("SIC2","year")]
dat_vol <- subset(dat_vol, !is.na(cashflow_sd_SIC2))

# Bates et al order industry quintiles by the increase in idiosyncratic risk 
table_vol <- dat_vol[,.(SIC2_vol=mean(cashflow_sd_SIC2,na.rm=T)),
                     by=c("SIC2","year")][order(year)]

dat <- merge(dat, table_vol, by=c("SIC2","year"), all.x=T)

setkeyv(dat, c("WC06105","year"))

# Tobin's Q and other variables -------------------------------------------

# Capitalise R&D expenses 
# If R&D is missing or less than zero, set it to zero
dat[,RandD_zeros := ifelse(is.na(WC01201 | WC01201<0), 0, WC01201)]

# Get some indexes for each firm: how many years in-sample and where in-sample
dat[,obsT := max(year) - year, by="WC06105"]
dat[,obs := year - min(year), by="WC06105"]

# Then use those indexes for R&D numbers using depreciation
dat[,depreciationrateRandD := (1-0.15)^obsT] # Assume 15% depreciation rate for now
dat[,KRandD_cum := cumsum(RandD_zeros*depreciationrateRandD), by="WC06105"]
dat[,KRandD := KRandD_cum / depreciationrateRandD]

# Intangible assets from balance sheet
dat[,intangassets := ifelse(is.na(WC02649| WC02649<0), 0, WC02649)]
dat[,KRandDplusintangassets:=KRandD + intangassets]

# Capitalise portion of Selling, General and Administrative expenses 
# subtract R&D from SG&A and only account for 30% to be investment in 
# organisational capital following papers cited in Peters and Taylor
dat[,SGandA:= 0.3*(WC01101 - WC01201)]  
dat[,SGandA_zeros := ifelse(is.na(SGandA | SGandA<0), 0, SGandA)]
dat[,depreciationrateSGandA := (1-0.2)^obsT] 
# 20% depreciation rate as in Peters and Taylor (2016), Journal of F Economics
dat[,KSGandA_cum := cumsum(SGandA_zeros*depreciationrateSGandA), by="WC06105"]
dat[,KSGandA:= KSGandA_cum / depreciationrateSGandA ]

# Define capital stock and investment as in Peters and Taylor
# Total intangible assets
dat[,Kintang := KRandD + intangassets + KSGandA]
dat [,Kfixed := ifelse(WC02301>0, WC02301, NA)] # note change 7.6.2017
# Total capital stock (tangibles plus intangibles)
dat[,Ktotal := Kintang + Kfixed]

# Tobin's q 
dat[, MVfirm := (WC08001 + (WC03051 + WC03251) - WC02201)] 
# Market cap + Total debt - Current assets 

dat[, qTotal := MVfirm / Ktotal]
dat[, qKfixed := MVfirm/Kfixed]
dat[, qTA := MVfirm/WC02999]
dat[,qSimple := (WC08001 + WC03351) / (WC03501 + WC03351)] 
# From http://finabase.blogspot.ch/2013/05/tobins-q-ratio-what-is-and-where-can-i.html 
# I will use qSimple for this paper

# Investment  
# Total intangible investment
dat[, Iintang := RandD_zeros + SGandA_zeros]
dat[, Iintang2 := RandD_zeros + SGandA_zeros + intangassets - shift(intangassets,1),
    by="WC06105"]
dat[, Iintang_K := Iintang/shift(Ktotal,1),by="WC06105"]

# Total physical investment
dat[, capex := WC04601]  # capital investment
dat[, capex_K := capex/shift(Ktotal,1),by="WC06105"]

# Standard empirical investment rate
dat[,capex_Kfixed := capex / shift(Kfixed,1),by="WC06105"]
dat[,capex_TA := capex / shift(WC02999,1),by="WC06105"]

# Total physical investment
dat[, RandD := WC01201]  # capital investment
dat[, RandD_K := RandD/shift(Ktotal,1),by="WC06105"]

# Total investment
dat[, TotalInv := Iintang + capex]
dat[, TotalInv_K := TotalInv/shift(Ktotal,1),by="WC06105"]

## Cashflow
dat[,cashflow := WC18198 + 0.7*Iintang] # following Peters and Taylor # EBITDA
dat[,cashflow2_K := (WC18191 + WC01151 + 0.7*Iintang)/shift(Ktotal,1),by="WC06105"] # different defn
dat[,cashflow3_K := (WC18191 + WC01151)/shift(Ktotal,1),by="WC06105"] # different defn
dat[,cashflow_K := cashflow/shift(Ktotal,1),by="WC06105"]
dat[,cashflow_Kfixed := cashflow/shift(Kfixed,1),by="WC06105"]
dat[,cashflow_TA := cashflow/shift(WC02999,1),by="WC06105"]
dat[,cashflow2_TA := cashflow2/shift(WC02999,1),by="WC06105"]


# Sales growth 
dat[,salesgrowth := 0.5*(WC01001-shift(WC01001,1))/(WC01001 + shift(WC01001,1)), 
    by="WC06105"]


# OECD zombie indicator and other vars ------------------------------------

### Zombie indicator: OECD definition, Abdrews et al (2017)
dat[, ebit_interest:= WC18191/(1+WC01251) ] # EBIT/ interest paid
# Add the one on denominator so as to not get infinite values

# Dummy if interest coverage ration < 1 ie paid more in interest than earned
dat[,D_ebitint:=ifelse(ebit_interest<1,1,0)]

dat[,l.D_ebitint:= shift(D_ebitint,1),by="WC06105"]
dat[,l2.D_ebitint:= shift(D_ebitint,2),by="WC06105"]
dat[,D_ebitint_3obs := D_ebitint + l.D_ebitint + l2.D_ebitint]
# if =3 then we have the ICR condition for zombification met

# Often we don't have the age of the firm explicitly from the fields so we use
# a conservative estimate of age being the first time the firm is observed in
# the sample
dat[,foundingyear:=as.numeric(substring(as.character(WC18272),1,4))]
dat[,incorpyear:=as.numeric(substring(as.character(WC18273),1,4))]
dat[,startyear := ifelse(!is.na(foundingyear),foundingyear, incorpyear)]
dat[,age := ifelse(year-startyear>=0,year-startyear,NA)]
dat[,ageinsample := year - head(year,1), by="WC06105"]
dat[,age:=ifelse(is.na(age),ageinsample,age)]

# OECD zombie definition: old and unproductive
dat[, Dzombie3 := ifelse(D_ebitint_3obs>2 & age>9, 1, 0)]

### Create variable which indicates transition to zombie firm
dat[,lDzombie3 := shift(Dzombie3,1), by="WC06105"]
dat[,enterZombie := ifelse(Dzombie3==1 & lDzombie3==0, 1, 0)]

### Create variable which indicates transition out of zombie
## Note that a zombie can exit for two reasons
# (a) increases increases interest coverage ratio to escape zombie definition 
# (either increase profitability or reduce debt) 

# Exit due to reason (a) increases increases interest coverage ratio to escape 
# zombie definition (either increase profitability or reduce debt)
dat[,lDzombie3 := shift(Dzombie3,1), by="WC06105"]
dat[,recoverZombie := ifelse(Dzombie3==0 & lDzombie3==1, 1, 0)] 
# a good exit ie reduce interest coverage

# Exit due to reason (b) exits the market (either Acquired/merger, 
# private buyout, insolvent)
dat[,fDzombie3 := shift(Dzombie3,1,type="lead"), by="WC06105"]
dat[,exitZombie := ifelse(Dzombie3==1 & is.na(fDzombie3) & year!=2016,1,0)] 
# Here market exit measured by not being in the database in the next year 

# Further variables defined -----------------------------------------------

# Define variables 
XM <- c("BE","DE","FR","ES","NL","IT")
other <- c("CH","SE","DK","AU")
dat$cc <- as.character(dat$cc)
dat[,cc2 := ifelse(cc%in%XM, "XM", cc)]
dat[,cc2 := ifelse(cc%in%other, "other", cc2)]
# This is important for the policy rates for ECB rates (BIS database)

# create size in USD and deflate with US cpi
cpiUS <- subset(dat,cc=="US")
cpiUS <- unique(cpiUS[,c("year","cpi"),with=F])
names(cpiUS) <- c("year","cpiUS") # real rates as of 2012 USD
dat <- merge(dat, cpiUS, by=c("year"), all.x=T)
dat[,realsizeUSD := WC07230/cpiUS]

# Factor to convert nominal variables to real USD
dat[,fxUSD := WC07230 / WC02999]
dat[,realUSD := fxUSD/cpiUS]
setkeyv(dat, c("WC06105","year"))

## Count number of obs by firm - useful for knowing how unbalanced the panel is
dat[,nobs_firm := .N, by="WC06105"]

# Net leverage testing 
dat[,leverage := (WC03255)/WC02999*100] # Total debt/ total assets
dat[,netleverage := (WC03255 - WC02001)/WC02999*100] 
# (Total debt - cash and equiv)/total assets

## Share of long-term debt
dat[,shareoflongtermdebt := WC03251/ (WC03051 + WC03251)*100]

# Sources of funds beyond debt 

# Follow Mckeon 2015 - identify equity issuance as proceeds from equity greater 
# than 3% of market cap. This helps separate equity issuance from Stock options
# I used 2% of market cap to get a large enough share for zombies
# This helps separate equity issuance from Stock options
dat[,Dequityissuance:= ifelse(WC04251 > 0.02*WC08001, 1, 0)]
dat[,equityissuance_TA := WC04251*Dequityissuance/shift(WC02999,1)*100]
dat[,equityissuance_TA2 := WC04251/WC02999*100]

# Proceeds from disposal of assets
dat[,assetdisposal_TA := WC04351/shift(WC02999,1)*100]

# BH Zombie definition ----------------------------------------------------

## Zombie if it fits the OECD definition and has below media Tobin's q in that year (or year sector)

dat[, qTA := qSimple]

dat[,qTA_less_median := qSimple - median(qSimple, na.rm=T), by=c("year","SIC2")]
dat[,lqTA_less_median := shift(qTA_less_median,1), by=c("WC06105")] # not needed?
dat[,l2qTA_less_median := shift(qTA_less_median,2), by=c("WC06105")]

dat[,leverage_less_median := leverage - median(leverage, na.rm=T), 
    by=c("year","SIC2")]
## Zombie if OECD definition plus above median netleverage (in a year sector)
dat[,netleverage_less_median := netleverage - median(netleverage, na.rm=T), 
    by=c("year","SIC2")]
dat[,Dzombie3netleverage := ifelse(Dzombie3==1 & netleverage_less_median > 0, 1,0)]
# Might not be required as it uses leverage rather than Tobin

dat <- subset(dat, !is.na(qTA))
dat <- subset(dat, !is.na(Dzombie3))
# If the zombie status (old definition or Tobibn) is undefined the drop obsvs

dat[, Dzombie1_noage := ifelse(D_ebitint==1, 1, 0)]
dat[, Dzombie3noage := ifelse(D_ebitint_3obs>2, 1, 0)]
# Define without the age condition: useful for robustness checks etc

dat[,Dzombie3q := ifelse(Dzombie3==1 & !is.na(Dzombie3) & qTA_less_median < 0 ,
                         1, 0)] # Definition for paper 
dat[,Dzombie3qnoage_missingage := ifelse(Dzombie3noage==1 & !is.na(Dzombie3) & 
                                           qTA_less_median < 0 & !is.na(startyear), 1,0)] 
# Definition for paper - more than one year as a low Tobin's q firm
dat[,Dzombie3qnoage := ifelse(Dzombie3noage==1 & !is.na(Dzombie3) & 
                                qTA_less_median < 0 , 1,0)] 
# Definition for paper - more than one year as a low Tobin's q firm (drop age from Zombie definition)
dat[,Dzombie3qnoage2 := ifelse(Dzombie3noage==1 & !is.na(Dzombie3) & 
                                 qTA_less_median < 0 & lqTA_less_median < 0, 1,0)] 
# more than one year as a low Tobin's q firm (drop age from Zombie definition)
dat[,Dzombie3qnoage3 := ifelse(Dzombie3noage==1 & !is.na(Dzombie3) & 
                                 qTA_less_median < 0 & lqTA_less_median < 0 & l2qTA_less_median < 0, 1,0)] 
# more than one year as a low Tobin's q firm (drop age from Zombie definition)


### Some further cleaning 
dat <- subset(dat, !is.na(Dzombie3noage)) # filter to where at least 3 EBITs observed
dat <- subset(dat, !is.na(Dzombie3qnoage))
dat <- unique(dat) # remove duplicates

dat[,Dzombie3q := ifelse(is.na(Dzombie3) , NA, Dzombie3q)] 
# Definition for paper - more than one year as a low Tobin's q firm

## Zombie if OECD definition and below average R&D to capital expenditure

dat[,realpolrate := bis_polrate - dlcpi]

# Compute value-added by country ------------------------------------------

## Compute table of no. of obs where value added can be computed by [net sales 
# or revenues (WC01001) - materials expense (WC18195)] (by country)
dat[,va_salesmaterials := WC01001 - WC18195]

## Compute table of no. of firms where value added can be computed by wages
# (staff costs) WC01084 + profits (EBITDA) WC18198
dat[,va_wagesebitda := WC01084 + WC18198 ]
dat[,va_wagesgrossprofit := WC01084 + WC01100]

# Check exit reasons ------------------------------------------------------

firm_statuses <- as.data.table(read_excel(paste0('C:/Users/mhollins1/Documents/',
'msc_thesis/reference_data/firm_statuses.xlsx')))

dat <- merge(dat, firm_statuses, by='WC06105', all.x=T)
setkeyv(dat, c("WC06105","year"))

dat[,trueexitold := ifelse(str_detect(name_and_status, c("DEAD|MERGER|TAKEOVER|LIQUIDATED")), 1, 0)]
dat[,trueexit := ifelse(str_detect(name_and_status, c("DEAD|MERGER|TAKEOVER|LIQUIDATED")), 1, 0)]
dat[,merger := ifelse(str_detect(name_and_status, c("MERGER|DELIST")), 1, 0)]
dat[,trueexit2 := ifelse(trueexit==1 & merger!=1, 1, 0)]
dat[,liquidated := ifelse(str_detect(name_and_status, c("LIQUIDATED")), 1, 0)]

exit_reasons <- firm_statuses %>%
  mutate(dead = ifelse(str_detect(name_and_status, c("DEAD|LIQUIDATED")), 1, 0),
         merged = ifelse(str_detect(name_and_status, c("MERGER")), 1, 0),
         taken_over = ifelse(str_detect(name_and_status, c("TAKEOVER")), 1, 0),
         suspended = ifelse(str_detect(name_and_status, c("SUSP")), 1, 0))

exit_reasons <- exit_reasons %>%
  summarise(dead = sum(dead),
            merged = sum(merged),
            taken_over = sum(taken_over),
            suspended = sum(suspended))

# Calculate zombie durations, exits etc. ----------------------------------


### Change enter zombie definition
dat[, enterZombieq := ifelse(enterZombie==1 & Dzombie3q==1, 1, 0)]
setkeyv(dat, c("WC06105","year"))

# Measure Zombie duration
dat[, zombieduration := cumsum(Dzombie3), 
    by = list(WC06105,rleid(Dzombie3 == 0L))]
dat[, zombiedurationq := cumsum(Dzombie3q), 
    by = list(WC06105,rleid(Dzombie3q == 0L))]
dat[, zombiedurationqnoage := cumsum(Dzombie3qnoage), 
    by = list(WC06105,rleid(Dzombie3qnoage == 0L))]
dat[, zombieduration3noage := cumsum(Dzombie3noage), 
    by = list(WC06105,rleid(Dzombie3noage == 0L))]  # 3 years ICR < 1
dat[, zombiedurationnetleverage := cumsum(Dzombie3netleverage), 
    by = list(WC06105,rleid(Dzombie3netleverage == 0L))]

### Enter zombie state for refined definition
### Create variable which indicates transition to zombie firm
dat[,lDzombie3q := shift(Dzombie3q,1), by="WC06105"]
dat[,enterZombieq := ifelse(Dzombie3q==1 & lDzombie3q==0, 1, 0)]
dat[,lDzombie3qnoage := shift(Dzombie3qnoage,1), by="WC06105"]
dat[,enterZombieqnoage := ifelse(Dzombie3qnoage==1 & lDzombie3qnoage==0, 1, 0)]


# Create variable which counts how many years before being a Zombie
dat$timetozombie <- 0
for (i in seq(1,4)){
  varname <- paste("timetozombie_",i, sep="")
  dat[,as.character(varname):=shift(enterZombieq,i,type="lead"), by="WC06105"]
  dat[,timetozombie:=ifelse(get(varname)==1 & !is.na(get(varname)),-i, timetozombie )]
}

dat[,min_timetozombie := min(timetozombie),by="WC06105"]

### Create variable which measures years since exiting Zombie state (and not delisting)
######## Check this zombie time indicator - potential issue with NAs - could also be a problem with zombie time!!!!

dat$timefromzombie <- 0
for (i in seq(1,4)){
  varname <- paste("timefromzombie_",i, sep="")
  dat[,as.character(varname):=shift(recoverZombie,i-1), by="WC06105"]
  dat[,timefromzombie:=ifelse(get(varname)==1  & !is.na(get(varname)),i, timefromzombie )]
}

# Create variable which counts how many years after entry into Zombie state - including firms which exit
dat$timefromzombiestart <- 0
#dat$timefromzombiestart <- dat$enterZombieq
for (i in seq(1,6)){
  varname <- paste("timefromzombiestart_",i, sep="")
  dat[,as.character(varname):=shift(enterZombieq,i-1,type="lag"), by="WC06105"]
  dat[,timefromzombiestart:=ifelse(get(varname)==1 & !is.na(get(varname)),i, timefromzombiestart )]
}

dat[,max_timefromzombiestart := max(timefromzombiestart),by="WC06105"]

###### Create time series of zombie states
dat[,zombietime_old := ifelse(zombieduration>0,zombieduration, timetozombie)] 
# only zombies - before and after
dat[,zombietime := ifelse(zombiedurationq>0,zombiedurationq, timetozombie)] 
# only zombies - before and after - with Tobin's q refinement
dat[,zombietimefromstart := ifelse(timefromzombiestart>0,timefromzombiestart, timetozombie)] 
# includes firms which recover from zombie state
dat[, zombiestate := ifelse(Dzombie3==1, "Zombie",ifelse(timefromzombie>0, "recoveringZombie", "NonZombie"))] 
# Measure time since recovering from zombie state

# More variables and winsorising ------------------------------------------

setkeyv(dat, c("WC06105","year"))
dat[,dividendstatus := ifelse(WC05376>0,"DividendPayer","NonDividendPayer")]
dat[,capex_TA:= WC04601/WC02999]
dat[,RandD_sales:=WC01201/WC01001] # R&D/sales
dat[,mkt_to_book:=(WC02999 - WC03501 + WC08001)/WC02999] 
# (Total assets - common equity + market cap)/total assets
dat[,industrySigma:= cashflow_sd_SIC2 ]
dat[,fixedassetratio := WC02501 / WC02999] # PPE/fixed assets
dat[,fdemp := (shift(WC07011,1,type="lead") - WC07011)/(0.5*((shift(WC07011,1,type="lead") + WC07011))), by="WC06105"]
dat[,fdlemp := log(shift(WC07011,1,type="lead")/ WC07011), by="WC06105"]

dat[,Dzombie3q_1:= shift(Dzombie3q,1,type="lead"),by="WC06105"]
dat[,Dzombie3q_2:= shift(Dzombie3q,2,type="lead"),by="WC06105"]
dat[,Dzombie3q_3:= shift(Dzombie3q,3,type="lead"),by="WC06105"]
dat[,Dzombie3q_4:= shift(Dzombie3q,4,type="lead"),by="WC06105"]
dat[,SIC2_year :=paste(SIC2,year,sep="+")]
dat[,cc_year :=paste(cc,year,sep="+")]

dat[,medianIndustrynetLeverage := median(netleverage,na.rm=T),by="SIC2"]

dat[,dnetleverage:= ((WC03255 - WC02001) - (shift(WC03255,1) - shift(WC02001,1)))/shift(WC02999),by="WC06105"]
dat[,dleverage:= (WC03255 - shift(WC03255,1))/shift(WC02999),by="WC06105"]
dat[,fdleverage:= (shift(WC03255,1, type="lead") - WC03255)/WC02999,by="WC06105"]
dat[,fdMVfirm:= (shift(MVfirm,1, type="lead") - MVfirm)/WC02999,by="WC06105"]
dat[,fdSales:= (shift(WC01001,1, type="lead") - WC01001)/(0.5*(shift(WC01001,1, type="lead") + WC01001)),by="WC06105"]
dat[,dcashratio:= (WC02001 - shift(WC02001,1))/shift(WC02999),by="WC06105"]
dat[,interestlessebit_TA := (WC01251 - WC18191)/shift(WC02999),by="WC06105"]
dat[,interest_TA := (WC01251)/shift(WC02999),by="WC06105"]
dat[,ebit_TA := (WC18191)/shift(WC02999),by="WC06105"]# Ebit
dat[,dividends_TA := (WC04551)/shift(WC02999),by="WC06105"]
dat[,decreaseininvestments_TA := (WC04440)/shift(WC02999),by="WC06105"]

dat[,aveinterestrate := (WC01251 / WC03255)*100]  # interest paid/total debt
dat[,aveinterestrate := ifelse(!is.infinite(aveinterestrate), aveinterestrate, NA)]
dat[,aveinterestrate := ifelse(aveinterestrate<0, NA, aveinterestrate)]
dat[,aveinterestrate := ifelse(aveinterestrate>100, NA, aveinterestrate)]
# Ie they don't pay more in interest than their total debts and they don't pay
# negative amounts in interest - could this be a problematic assumption? 

# Winsorise all variables at 1% level

dat[,realsizeUSD_w:=winsor(realsizeUSD, 0.01, tails = "twosided")]
dat[,logrealsizeUSD_w:=winsor(log(realsizeUSD), 0.01, tails = "twosided")] 
dat[,cashflow_K_w:=winsor(cashflow3_K, 0.01, tails = "twosided")]
dat[,cashflow2_K_w:=winsor(cashflow2_K, 0.01, tails = "twosided")] 
## Cashflow adding back in 0.7*Intangible expenditures (following Peters and Taylor)
dat[,cashflow3_K_w:=winsor(cashflow3_K, 0.01, tails = "twosided")] 
## Cashflow excluding intangible expenditures - normal definition

dat[,leverage_w := winsor(leverage,0.01, tails="twosided")]
dat[,netleverage_w:=winsor(netleverage, 0.01, tails = "twosided")]
dat[,cashratio_w:=winsor(cashratio, 0.01, tails = "twosided")]
dat[,capex_TA_w := winsor(capex_TA,0.01, tails="twosided")]
dat[,capex_K_w := winsor(capex_K,0.01, tails="twosided")]
dat[,capex_Kfixed_w := winsor(capex_Kfixed,0.01, tails="twosided")]
dat[,RandD_sales_w := winsor(RandD_sales,0.01, tails="twosided")]
dat[,RandD_K_w:=winsor(RandD_K, 0.01, tails = "twosided")] 
dat[,Iintang_K_w:=winsor(Iintang_K, 0.01, tails = "twosided")] 

dat[,mkt_to_book_w := winsor(mkt_to_book,0.01, tails="twosided")]
dat[,industrySigma_w := winsor(industrySigma,0.01, tails="twosided")]
dat[,fixedassetratio_w := winsor(fixedassetratio,0.01, tails="twosided")]

dat[,fdemp_w := winsor(fdemp,0.01, tails="twosided")]
dat[,fdlemp_w := winsor(fdlemp,0.01, tails="twosided")]
dat[,fcapex_K_w := shift(capex_K_w,1,type="lead"),by="WC06105"]

dat[,dnetleverage_w:=winsor(dnetleverage, 0.01, tails = "twosided")]
dat[,dleverage_w:=winsor(dleverage, 0.01, tails = "twosided")]
dat[,fdleverage_w:=winsor(fdleverage, 0.01, tails = "twosided")]
dat[,fdMVfirm_w:=winsor(fdMVfirm, 0.01, tails = "twosided")]
dat[,fdSales_w:=winsor(fdSales, 0.01, tails = "twosided")]
dat[,dcashratio_w:=winsor(dcashratio, 0.01, tails = "twosided")]

dat[,equityissuance_TA_w:=winsor(equityissuance_TA, 0.01, tails = "twosided")]
dat[,assetdisposal_TA_w:=winsor(assetdisposal_TA, 0.01, tails = "twosided")]
dat[,interestlessebit_TA_w:=winsor(interestlessebit_TA, 0.01, tails = "twosided")]
dat[,interest_TA_w:=winsor(interest_TA, 0.01, tails = "twosided")]
dat[,ebit_TA_w:=winsor(ebit_TA, 0.01, tails = "twosided")]
dat[,dividends_TA_w:=winsor(dividends_TA, 0.01, tails = "twosided")]
dat[,decreaseininvestments_TA_w:=winsor(decreaseininvestments_TA, 0.01, tails = "twosided")]

dat[,ebitinterest_w:=winsor(ebit_interest, 0.01, tails = "twosided")]
dat[,qTA_w:=winsor(qTA, 0.01, tails = "twosided")]
dat[,qTotal_w:=winsor(qTotal, 0.01, tails = "twosided")]
dat[,qKfixed_w:=winsor(qKfixed, 0.01, tails = "twosided")]
dat[,shareoflongtermdebt_w:=winsor(shareoflongtermdebt, 0.01, tails = "twosided")]

dat[,va_wagesgrossprofit_w:=winsor(va_wagesgrossprofit, 0.01, tails = "twosided")]

### Productivity
dat[,labourprod_perworker:=va_wagesgrossprofit*realUSD/WC07011] 
# Value added / staff costs

dat[,labourprod_w:=winsor(va_wagesgrossprofit/WC01084, 0.01, tails = "twosided")] #Value added / staff costs
dat[,labourprod_perworker_w:=winsor(va_wagesgrossprofit*realUSD/WC07011, 0.01, tails = "twosided")] #Value added / staff costs
dat[,capitalprod_w:=winsor(va_wagesgrossprofit/Kfixed, 0.01, tails = "twosided")] #Value added / staff costs
dat[,capitalprod_w:=winsor(va_wagesgrossprofit*realUSD/Kfixed*realUSD, 0.01, tails = "twosided")] #Value added / staff costs

### TFP estimates
# estimate cobb-douglas productions at the sector level - logs coerce to missings
dat[, lrealva_wagesgrossprofit:= ifelse(is.finite(log(va_wagesgrossprofit*realUSD)),log(va_wagesgrossprofit*realUSD), NA) ]
dat[, lrealKfixed:= ifelse(is.finite(log(Kfixed*realUSD)),log(Kfixed*realUSD), NA) ]
dat[,Employees := WC07011]
dat[, llabour:= ifelse(is.finite(log(WC07011)),log(WC07011), NA) ]
dat[, lreallabourcost:= ifelse(is.finite(log(WC01084*realUSD)),log(WC01084*realUSD), NA) ]

dat[,aveinterestrate_w:=winsor(aveinterestrate, 0.01, tails = "twosided")]
dat[,aveinterestrate_w5:=winsor(aveinterestrate, 0.01, tails = "twosided")]

### Subsidised Credit á la Achaya et al
# note: Follow Acharya et al OMT and zombies by using average interest rates 
# on firms with AAA interest coverage ratios 
# - see http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/ratings.htm
# Data at Jan 2018 ICR>8.5 is AAA for large firms (mkt cap > $5billion), for smaller firms ICR>12.5
dat[, AAAfirms := ifelse(ebitinterest_w>=8.5, "AAA","nonAAA")]
dat[, AAAfirms_aveinterestrate_w := ifelse(AAAfirms=="AAA", aveinterestrate_w, NA)]
dat[, shareoflongtermdebt_gr50 := ifelse(shareoflongtermdebt_w>50, "LTgr50", "LTle50")] 
# split group into above 50% longterm debt

dat[, AAAinterestrate := mean(AAAfirms_aveinterestrate_w, na.rm=T), 
    by=c("year", "shareoflongtermdebt_gr50", "cc")]
dat[, subsidcredit := ifelse(aveinterestrate_w < AAAinterestrate, 1, 0)]
dat[,Dzombie3noage_subsidisedcredit := ifelse(aveinterestrate_w < AAAinterestrate & Dzombie3noage==1, 1, 0)]
dat[,Dzombie3noage_subsidisedcredit := ifelse(is.na(Dzombie3qnoage), NA, Dzombie3noage_subsidisedcredit)]
dat[,Dzombie3q_subsidisedcredit := ifelse(aveinterestrate_w < AAAinterestrate & Dzombie3q==1, 1, 0)]
dat[,Dzombie3noage_subsidisedcredit := ifelse(is.na(Dzombie3q), NA, Dzombie3noage_subsidisedcredit)]
dat[,interest_subsidy := AAAinterestrate - aveinterestrate_w] # close to risk premium

# Variables Zombie congestion regressions ---------------------------------

# Capital, credit by sector and year contained in zombie firms as fraction of all firms
dat[,zshare_1 := sum(Dzombie1_noage, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA_1 := sum(Dzombie1_noage*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie_1 := 1-Dzombie1_noage]

dat[,zshare_3 := sum(Dzombie3noage, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA_3 := sum(Dzombie3noage*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie_3 := 1-Dzombie3noage]

dat[,zshare_3qnoage := sum(Dzombie3qnoage, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA_3qnoage := sum(Dzombie3qnoage*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie_3qnoage := 1-Dzombie3qnoage]

dat[,zshare_3qnoage2 := sum(Dzombie3qnoage2, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA_3qnoage2 := sum(Dzombie3qnoage2*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie_3qnoage2 := 1-Dzombie3qnoage2]

dat[,zshare_3qnoage3 := sum(Dzombie3qnoage3, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA_3qnoage3 := sum(Dzombie3qnoage3*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie_3qnoage3 := 1-Dzombie3qnoage3]

dat[,zshare_subsidisedcredit := sum(Dzombie3noage_subsidisedcredit, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA_subsidisedcredit := sum(Dzombie3noage_subsidisedcredit*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie3_subsidisedcredit := 1-Dzombie3noage_subsidisedcredit]

dat[,zshare3q_subsidisedcredit := sum(Dzombie3q_subsidisedcredit, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareTA3q_subsidisedcredit := sum(Dzombie3q_subsidisedcredit*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,NDzombie3q_subsidisedcredit := 1-Dzombie3q_subsidisedcredit]

# non-zombie dummy variables
dat[,NDzombie3q := 1-Dzombie3q]
dat[,NDzombieOECD := 1-Dzombie3]

#### Weighting congestion by number of firms
dat[,zshare := sum(Dzombie3q, na.rm=T)/.N, by=c("SIC2","year")]
dat[,zshareOECD := sum(Dzombie3, na.rm=T)/.N, by=c("SIC2","year")]

#### Weighting congestion by total assets
dat[,zshareTA := sum(Dzombie3q*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]
dat[,zshareTAOECD := sum(Dzombie3*realsizeUSD, na.rm=T)/sum(realsizeUSD,na.rm=T), by=c("SIC2","year")]

#### Weighting congestion by employment
dat[,zshareEmp := sum(Dzombie3q*Employees, na.rm=T)/sum(Employees,na.rm=T), by=c("SIC2","year")]
dat[,zshareEmpOECD := sum(Dzombie3*Employees, na.rm=T)/sum(Employees,na.rm=T), by=c("SIC2","year")]

#### Weighting congestion by book value of physical capital stock
dat[,zshareK := sum(Dzombie3q*Kfixed*realUSD, na.rm=T)/sum(Kfixed*realUSD,na.rm=T), by=c("SIC2","year")]
dat[,zshareKOECD := sum(Dzombie3*Kfixed*realUSD, na.rm=T)/sum(Kfixed*realUSD,na.rm=T), by=c("SIC2","year")]

#### Weighting congestion by Mkt value of equity
dat[,zshareMV := sum(Dzombie3q*MVfirm*realUSD, na.rm=T)/sum(MVfirm*realUSD,na.rm=T), by=c("SIC2","year")]
dat[,zshareMVOECD := sum(Dzombie3*MVfirm*realUSD, na.rm=T)/sum(MVfirm*realUSD,na.rm=T), by=c("SIC2","year")]

#### Weighting congestion by Debt
dat[,zshareDebt := sum(Dzombie3q*WC03255*realUSD, na.rm=T)/sum(WC03255*realUSD,na.rm=T), by=c("SIC2","year")]
dat[,zshareDebtOECD := sum(Dzombie3*WC03255*realUSD, na.rm=T)/sum(WC03255*realUSD,na.rm=T), by=c("SIC2","year")]

#### Weighting congestion by Sales
dat[,zshareSales := sum(Dzombie3q*WC01001*realUSD, na.rm=T)/sum(WC01001*realUSD,na.rm=T), by=c("SIC2","year")]
dat[,zshareSalesOECD := sum(Dzombie3*WC01001*realUSD, na.rm=T)/sum(WC01001*realUSD,na.rm=T), by=c("SIC2","year")]

dat[,zshare_1_w := winsor(zshare_1,0.01, tails="twosided")]
dat[,zshareTA_1_w := winsor(zshareTA_1,0.01, tails="twosided")]

dat[,zshare_3_w := winsor(zshare_3,0.01, tails="twosided")]
dat[,zshareTA_3_w := winsor(zshareTA_3,0.01, tails="twosided")]

dat[,zshare_3qnoage_w := winsor(zshare_3qnoage,0.01, tails="twosided")]
dat[,zshareTA_3qnoage_w := winsor(zshareTA_3qnoage,0.01, tails="twosided")]

dat[,zshare_3qnoage2_w := winsor(zshare_3qnoage2,0.01, tails="twosided")]
dat[,zshareTA_3qnoage2_w := winsor(zshareTA_3qnoage2,0.01, tails="twosided")]

dat[,zshare_3qnoage3_w := winsor(zshare_3qnoage3,0.01, tails="twosided")]
dat[,zshareTA_3qnoage3_w := winsor(zshareTA_3qnoage3,0.01, tails="twosided")]

dat[,zshare_subsidisedcredit_w := winsor(zshare_subsidisedcredit,0.01, tails="twosided")]
dat[,zshareTA_subsidisedcredit_w := winsor(zshareTA_subsidisedcredit,0.01, tails="twosided")]

dat[,zshare3q_subsidisedcredit_w := winsor(zshare3q_subsidisedcredit,0.01, tails="twosided")]
dat[,zshareTA3q_subsidisedcredit_w := winsor(zshareTA3q_subsidisedcredit,0.01, tails="twosided")]

dat[,zshare_w := winsor(zshare,0.01, tails="twosided")]
dat[,zshareOECD_w := winsor(zshareOECD,0.01, tails="twosided")]

dat[,zshareTA_w := winsor(zshareTA,0.01, tails="twosided")]
dat[,zshareTAOECD_w := winsor(zshareTAOECD,0.01, tails="twosided")]

dat[,zshareEmp_w := winsor(zshareEmp,0.01, tails="twosided")]
dat[,zshareEmpOECD_w := winsor(zshareEmpOECD,0.01, tails="twosided")]

dat[,zshareK_w := winsor(zshareK,0.01, tails="twosided")]
dat[,zshareKOECD_w := winsor(zshareKOECD,0.01, tails="twosided")]

dat[,zshareMV_w := winsor(zshareMV,0.01, tails="twosided")]
dat[,zshareMVOECD_w := winsor(zshareMVOECD,0.01, tails="twosided")]

dat[,zshareDebt_w := winsor(zshareDebt,0.01, tails="twosided")]
dat[,zshareDebtOECD_w := winsor(zshareDebtOECD,0.01, tails="twosided")]

dat[,zshareSales_w := winsor(zshareSales,0.01, tails="twosided")]
dat[,zshareSalesOECD_w := winsor(zshareSalesOECD,0.01, tails="twosided")]

dat[,lrealva_wagesgrossprofit_w := winsor(lrealva_wagesgrossprofit, 0.01, tails = "twosided")]

# External finance dependence variables -----------------------------------

dat[, ExtFinDep_i := (capex - WC04001)/capex] # Cashflow - operating income

dat[, ExtFinDep := median(ExtFinDep_i, na.rm=T), by="SIC2"]
dat[, .(ExtFinDep = median(ExtFinDep_i, na.rm=T)), by="SIC2"]

dat.ind <- subset(dat, cc=="US")[, .(ExtFinDep = median(ExtFinDep_i, na.rm=T),
                                     Kfixed = sum(Kfixed*realUSD,na.rm=T)), by="SIC2"] ### US ratio is higher
dat.ind[, kweight := Kfixed/sum(Kfixed,na.rm=T)]
dat.mean <- dat.ind[,.(ExtFinDepWeighted = sum(ExtFinDep * kweight,na.rm=T))]
print("industry capital weighted external financial dependence:")
print(dat.mean)

# Scatter of external dependence and zombie share
dat_ExtFinDep <- subset(dat,!(SIC2%in%c(91,92, 81, 21))) [,.(ExtFinDep = median(ExtFinDep_i, na.rm=T), 
                                                                  zshare_tobinq = sum(Dzombie3q, na.rm=T)/.N), by="SIC2"]
# Removing public administration (executive, legislature & general, and justice, 
# public order & safety, legal services and tobacco products as they seem to be anomalous)
setkeyv(dat_ExtFinDep, "ExtFinDep")
p <- ggplot(dat_ExtFinDep, aes(ExtFinDep, zshare_tobinq)) + geom_point() 
+ ggtitle("External financial dependence and zombie share") + geom_smooth(method="lm")
print(p)

dat_reg <- unique(dat[, c("cc","SIC2","year", "bis_polrate","dlcpi","ExtFinDep", 
                          "zshareTA_w","zshareTAOECD_w", "zshare_w", "zshareTA", 
                          "zshareK_w", 'zshareEmp_w', "avg_p2b_ratio", 'intr_sr',
                          'intr_lr'), with=F])


setkeyv(dat_reg, c("cc", "SIC2", "year"))

dat_reg[,fzshareTA_w:= shift(zshareTA_w,1, type="lead"),by="SIC2"]
dat_reg[,f3zshareTA_w:= shift(zshareTA_w,3, type="lead"),by="SIC2"]
dat_reg[,f2zshareTA_w:= shift(zshareTA_w,2, type="lead"),by="SIC2"]
dat_reg[,cc_year := paste0(cc,year)]
dat_reg[,SIC2_year := paste0(SIC2,year)]

dat_reg[,fzshareEmp_w:= shift(zshareEmp_w,1, type="lead"),by="SIC2"]
dat_reg[,f2zshareEmp_w:= shift(zshareEmp_w,2, type="lead"),by="SIC2"]
dat_reg[,fzshareK_w:= shift(zshareK_w,1, type="lead"),by="SIC2"]
dat_reg[,f2zshareK_w:= shift(zshareK_w,2, type="lead"),by="SIC2"]

dat_reg[,fzshareTAOECD_w:= shift(zshareTAOECD_w,1, type="lead"),by="SIC2"]
dat_reg[,f3zshareTA_w:= shift(zshareTA_w,3, type="lead"),by="SIC2"]
dat_reg[,f2zshareTAOECD_w:= shift(zshareTAOECD_w,2, type="lead"),by="SIC2"]

# Write panel: end of data manipulation -----------------------------------
# The data quality and coverage improves significantly from 1985 according to
# the Worldscope User's Guide, so with the ICR criterion requiring at least 3 
# consecutive years, it makes sense to begin the sample in 1987. 

# Write into csv for analysis file
# write.csv(dat, 'C:/Users/mhollins1/Documents/msc_thesis/clean_data/all_data.csv',
#           row.names = FALSE)
# 
# write.csv(subset(dat, year > 1986), 
#           'C:/Users/mhollins1/Documents/msc_thesis/clean_data/sample_panel.csv',
#           row.names = FALSE)

# Set directory to output
setwd(output_dir)




##################### Analysis: Graphs and regressions ########################


# Get a list of example zombies for paper discussion ---------------------

big_zombies <- dat %>%
  filter(Dzombie3q==1) %>%
  select(year, WC06001, age, ebit_interest, qSimple, realsizeUSD) %>%
  arrange(desc(realsizeUSD)) %>%
  head(n=100)

aa <- dat %>%
  filter(WC06001 == 'AMERICAN AIRLINES GROUP INC' & Dzombie3q==1)

eurotunnel <- dat %>%
  filter(WC06001 == 'EUROTUNNEL PLC' & Dzombie3q==1)

egdon <- dat %>%
  filter(WC06001 == 'EGDON RESOURCES PLC' & Dzombie3q==1)

# hornby <- dat %>%
#   filter(WC06001 == 'HORNBY PLC' & Dzombie3q==1)


# Write outputs for statistical appendix ----------------------------------

country_data <- dat %>%
  filter(year>1986 & year < 2019) %>%
  group_by(country, year) %>%
  summarise(firms = n(),
            zombies = sum(Dzombie3q),
            percent_zombies = round(100*(zombies/firms), 2)) %>%
  ungroup()

# Too long!!
# sector_data <- dat %>%
#   filter(year>1986 & year < 2019) %>%
#   group_by(SIC2, year) %>%
#   summarise(firms = n(),
#             zombies = sum(Dzombie3q),
#             percent_zombies = round(100*(zombies/firms), 2)) %>%
#   ungroup()
# sic_codes_data = read_csv(sic_codes, skip=2)
# names(sic_codes_data) = c('SIC2', 'sector')
# sector_data = left_join(sector_data, sic_codes_data, by='SIC2')
# sector_data = sector_data %>% select('SIC2', 'sector', 'year', 'firms', 'zombies', 'percent_zombies')

sector_data <- dat %>%
  filter(year>1986 & year < 2019) %>%
  mutate(sector = ifelse(SIC2 %in% c(1:14), 'Agriculture', NA),
         sector = ifelse(SIC2 %in% c(15:39), 'Manufacturing', sector),
         sector = ifelse(SIC2 %in% c(40:98), 'Services', sector),
         sector = ifelse(is.na(sector), 'Unclassified', sector)) %>%
  group_by(sector, year) %>%
  summarise(firms = n(),
            zombies = sum(Dzombie3q),
            percent_zombies = round(100*(zombies/firms), 2)) %>%
  ungroup()

write.csv(country_data, paste0(output_dir, 'country_data_appendix.csv'),
          row.names=F)
write.csv(sector_data, paste0(output_dir, 'sector_data_appendix.csv'),
          row.names=F)

# Sample stylised facts ---------------------------------------------------

# Number of firms in the sample
print(sum(!duplicated(dat$WC06105)))

# Number of active firms per year
write.csv(dat %>% group_by(year) %>% summarise(n_firms = n()), 
          'n_firms_per_year.csv',
          row.names = FALSE)

# Sector shares for Fig 2
sector_breakdown <- unique(dat[,c("WC06105","SIC2", 'year', 'realsizeUSD_w',
                                  'Ktotal', 'Employees', 'va_wagesgrossprofit', 
                                  'realUSD', 'Dzombie3q'),with=F])
sector_breakdown <- sector_breakdown %>%
    mutate(broad_sector = ifelse(SIC2 %in% c(1:14), 'Agriculture', NA),
           broad_sector = ifelse(SIC2 %in% c(15:39), 'Manufacturing', broad_sector),
           broad_sector = ifelse(SIC2 %in% c(40:98), 'Services', broad_sector),
           broad_sector = ifelse(is.na(broad_sector), 'Unclassified', broad_sector),
           broad_sector = as.factor(broad_sector),
           value_added = va_wagesgrossprofit*realUSD,
           value_added_w = winsor(value_added,0.01, tails="twosided"))
  
sector_breakdowns <- write.csv(sector_breakdown,
                               'sector_breakdowns.csv', row.names = FALSE)


# Average ICR and Tobin by each definition across the sample
temp1 <- dat %>% 
  group_by(Dzombie3) %>% 
  summarise(average_icr = median(ebit_interest),
            average_tobin = median(qSimple))
temp1$definition <- 'OECD'
names(temp1) <- c('zombie status', 'average ICR', 'average Tobin q', 'definition')
temp2 <- dat %>% 
  group_by(Dzombie3q) %>% 
  summarise(average_icrq = median(ebit_interest),
            average_tobinq = median(qSimple))
temp2$definition <- 'BIS'
names(temp2) <- c('zombie status', 'average ICR', 'average Tobin q', 'definition')
write.csv(rbind(temp1, temp2), 'icr_and_tobin_each_defn.csv',
          row.names = FALSE)

# Average ICR and Tobin by zombie status (assuming with Tobin's Q)
prod_changes <- dat %>%
  group_by(year, Dzombie3q) %>%
  summarise(avg_icr = median(ebit_interest),
            avg_tobin = median(qSimple)) %>%
  ungroup()
write.csv(prod_changes, 'icr_and_tobin_changes_over_time.csv')

# Zombie shares over time
z_shares <- dat %>%
  group_by(year) %>%
  summarise(z_oecd = mean(Dzombie3),
            z_bis = mean(Dzombie3q),
            z_subsid = mean(subsidcredit, na.rm=T)) %>%
  ungroup()
write.csv(z_shares, 'z_shares_each_defn.csv', row.names = FALSE)

z_time <- dat %>%
  filter(Dzombie3q == 1) %>%
  group_by(year) %>%
  summarise(z_time = mean(zombiedurationq),
            intr_sr = mean(intr_sr)) %>%
  ungroup() 
write.csv(z_time, 'z_time.csv', row.names = FALSE) 


# Zombie share by country -------------------------------------------------

#### LHP in 2016, RHP change between 1987 to 2016


dat_shares_cc <- subset(dat, year > 1986 & year < 2019)[,.(
  OECD = sum(Dzombie3, na.rm=T)/.N*100,
  BH = sum(Dzombie3q, na.rm=T)/.N*100),
by=c("cc","year")]

dat_shares_cc <- subset(dat_shares_cc, year %in% c(1987, 2018))
dat_shares_cc_long <- melt(dat_shares_cc, id.vars=c("cc","year"), variable.name = "zombieDefinition", value.name="share")
dat_shares_cc_long[,panels := ifelse(zombieDefinition=="OECD", "Old & unprofitable", "Old, unprofitable\n& low Tobins'q")]
p <- ggplot(subset(dat_shares_cc_long, year==2018), aes(x=reorder(cc, -share), share, fill=panels))  + geom_col(position="dodge") + ylab("Zombie share")
print(p)
write.csv(subset(dat_shares_cc_long, year<2019), "Graph 2 LHP country zombie share in 2018.csv")

setkeyv(dat_shares_cc, c("cc","year"))
dat_shares_cc[, dOECD := OECD - shift(OECD,1), by="cc"]
dat_shares_cc[, dBH := BH - shift(BH,1), by="cc"]

dat_dshares_cc <- dat_shares_cc[,c("cc","dOECD", "dBH"), with=F]
dat_dshares_cc <- subset(dat_dshares_cc, !is.na(dOECD))
dat_dshares_cc_long <- melt(dat_dshares_cc, id.vars=c("cc"), variable.name = "zombieDefinition", value.name="dshare")
dat_dshares_cc_long[,panels := ifelse(zombieDefinition=="dOECD", "Old & unprofitable", "Old, unprofitable\n& low Tobins'q")]

p <- ggplot(subset(dat_dshares_cc_long), aes(x=reorder(cc, -dshare), dshare, fill=panels))  + geom_col(position="dodge") + ylab("Change in zombie share")
print(p)
write.csv(subset(dat_shares_cc_long, year<2019), "Graph 2 RHP country zombie change in share 2018 1987.csv")


# Zombie duration ---------------------------------------------------------
dat_duration <- dat[,.(
  #dat_duration <- subset(dat, FF12!="Oil, Gas, and Coal Extraction and Products")[,.(
  BH = sum(zombiedurationq, na.rm=T)/sum(Dzombie3q, na.rm=T),
  OECD = sum(zombieduration, na.rm=T)/sum(Dzombie3, na.rm=T)
),
by="year"]

dat_duration_long <- melt(dat_duration, id.vars="year", variable.name = "zombieDefinition", value.name="duration")
p <- ggplot(subset(dat_duration_long, year < 2017 & year > 1987), aes(year, duration, colour=zombieDefinition)) + geom_line() + ggtitle("Rising duration low interest coverage firms")
print(p)
write.csv(dat_duration_long, paste0(output_dir, 'zombie_duration.csv'), row.names = F)
# Probability of remaining a zombie and transitions ---------------------------------------
# Zombie Transistion matrix 

#               (T+1)
#------------| Not Zombie | Zombie  | Exit  |
#-------------------------------------------
# Not Zombie |  Pn1       | Pn2     | Pn2   |  
#-------------------------------------------
# Zombie     |  Pz1       | Pz2     | Pz3   |
#-------------------------------------------
# Entry      |  1         | 0(by def*)|  0    |       * firm must be less than 10 years old

### Indicator variables from non-zombie state
dat[,fTA := shift(WC02999,1,type="lead"),by="WC06105"]

# Function to estimate transition probabilities in each year by state
transitionmatrix <- function (dat_ccgroup, DzombieVar) {
  dat_nz <- dat_ccgroup
  dat_nz <- dat_nz[, fDzombieVar := shift(get(DzombieVar),1,type="lead"), by="WC06105"]
  dat_nz[, ntoz := ifelse(fDzombieVar==1 & !is.na(fDzombieVar) & year!=2018 ,1,0)]
  dat_nz[, ntoexit := ifelse(is.na(fTA) & year!=2018 & trueexit==1,1,0)] 
  # exit is if no value for next year and not last year of same 
  dat_nz[, nton := ifelse(ntoz!=1 & ntoexit!=1,1,0)]
  dat_nz <- subset(dat_nz, get(DzombieVar)!=1)
  dat_nz <- subset(dat_nz, !is.na(get(DzombieVar)))
  
  a <- dat_nz[,.(Pntoz = sum(ntoz)/.N,
                 Pntoexit = sum(ntoexit)/.N,
                 Pnton = sum(nton)/.N), by="year"]

  #### Transition from zombie state
  dat_z <- dat_ccgroup
  dat_z <- dat_z[, fDzombieVar := shift(get(DzombieVar),1,type="lead"), by="WC06105"]
  dat_z[, ztoexit := ifelse(is.na(fTA) & year!=2018 & trueexit==1,1,0)] 
  # exit is if no value for next year and not last year of same 
  dat_z[, zton := ifelse(fDzombieVar!=1 & !is.na(fDzombieVar),1,0)]
  dat_z[, ztoz := ifelse(zton!=1 & ztoexit!=1 & year!=2018 ,1,0)]
  dat_z <- subset(dat_z, get(DzombieVar)==1)
  dat_z <- subset(dat_z, !is.na(get(DzombieVar)))
  
  b <- dat_z[,.(Pztoz = sum(ztoz)/.N,
                Pztoexit = sum(ztoexit)/.N,
                Pzton = sum(zton)/.N), by="year"]
 
  transprob <- merge(a,b, by="year")
  return(transprob)
}

# Transitions with tobin's q refined zombie definitions
transprob_fullsample_q <- transitionmatrix(dat_ccgroup=dat, 
                                           DzombieVar="Dzombie3q")

dat_transprob_long <- melt(transprob_fullsample_q, id.vars="year")

dat_transprob_long$year <- dat_transprob_long$year + 1
write.csv(dat_transprob_long, paste0(output_dir, 'transition_probs.csv'), row.names = F)

setkeyv(dat, c("WC06105","year"))
dat[,fTA := shift(WC02999,1,type="lead"),by="WC06105"]

# Transition probability for graph 1

### Transition from non-Zombie state
# Need exit status
transprob_fullsample <- transitionmatrix(dat_ccgroup=dat, DzombieVar="Dzombie3")
transprob_fullsample <- subset(transprob_fullsample, year < 2017 )
transprob_fullsample[, smoothztoz_OECD := (1/3)*(Pztoz + shift(Pztoz, 1) + shift(Pztoz, 2))]
ggplot(transprob_fullsample, aes(year, smoothztoz_OECD)) + geom_line()

transprob_fullsampleq <- transitionmatrix(dat_ccgroup=dat, DzombieVar="Dzombie3q")
transprob_fullsampleq <- subset(transprob_fullsampleq, year < 2017 )
transprob_fullsampleq[, smoothztoz_BH := (1/3)*(Pztoz + shift(Pztoz, 1) + shift(Pztoz, 2))]
ggplot(subset(transprob_fullsampleq, year>1982), aes(year, smoothztoz_BH)) + geom_line()

dat.transprob <- merge(transprob_fullsample[,c("year","smoothztoz_OECD"), with=F], transprob_fullsampleq[,c("year","smoothztoz_BH"), with=F], by="year")
ggplot(subset(dat.transprob, year>1982), aes(year, smoothztoz_BH)) + geom_line() + geom_line(aes(y=smoothztoz_OECD), colour="red")

write.csv(dat.transprob, file="zombie_transition_prob.csv", row.names=F)


# OECD and BH zombie shares and mean duration -----------------------------

dat_duration <- dat[,. (
  BH = sum(zombiedurationq, na.rm=T)/sum(Dzombie3q, na.rm=T),
  OECD = sum(zombieduration, na.rm=T)/sum(Dzombie3, na.rm=T)
),
by="year"]

dat_duration_long <- melt(dat_duration, id.vars="year", variable.name = "zombieDefinition", value.name="duration")
p <- ggplot(subset(dat_duration_long, year < 2019 & year > 1986), aes(year, duration, colour=zombieDefinition)) + geom_line() + ggtitle("Rising duration low interest coverage firms")
print(p)

# Zombie shares 
dat_shares_cc <- dat[,.(OECD = sum(Dzombie3, na.rm=T)/.N,
                        BH = sum(Dzombie3q, na.rm=T)/.N
),
by=c("cc","year")]
setkeyv(dat_shares_cc, c("cc","year"))
write.csv(dat_shares_cc, paste0(output_dir,"countryzombiesharedata.csv"),
          row.names=F)

# ... then average over countries
dat_shares_cc_mean <- dat_shares_cc[,.(
  OECD =mean(OECD, na.rm=T)*100,
  BH =mean(BH, na.rm=T)*100
  
), 
by="year"]

setkeyv(dat_shares_cc_mean, c("year"))
dat_shares_cc_mean_long <- melt(dat_shares_cc_mean, id.vars="year", variable.name = "zombieDefinition", value.name="share")

dat_graph_share_duration <- merge(dat_duration_long, dat_shares_cc_mean_long, by=c("zombieDefinition", "year"))

dat_graph_share_duration[,panels := ifelse(zombieDefinition=="OECD", "Old & unprofitable", "Old, unprofitable\n& low Tobins'q")]

p<- ggplot(subset(dat_graph_share_duration, year<2017), aes(year, duration, colour="duration")) + geom_line() + geom_line(aes(y=share, colour="share")) + 
  scale_colour_manual(name="", values=c(`duration`="black", `share`="red")) + facet_wrap(~panels, scales="free") + ggtitle("Unprofitable firms rise and survive for longer")
print(p)
write.csv(subset(dat_graph_share_duration, year<2019), "Graph 1 average_zombieshare and duration.csv")



# Zombie firm dynamics ----------------------------------------------------

dat[,period2 := ifelse(year<2000,"1.pre2000", "2.post2000")]
dat[,period2 := ifelse(year<2009,"1.pre2010", "2.post2010")]
dat[,period2 := ifelse(year<2000,"1.pre2000", ifelse(year >= 2010, "3.post2010", "2.2000-2009" )) ]
dat[,d2000 := ifelse(year<2000, 0, 1) ]
dat[,d2010 := ifelse(year>2009, 1, 0) ]


fm.ols.lev <- felm(dleverage_w ~ log(fixedassetratio_w) + log(mkt_to_book_w) + 
                     log(realsizeUSD_w) + log(capex_TA_w) + log(RandD_sales_w) + 
                     dividendstatus + Dzombie3q:period2 | SIC2_year + 
                     cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1 & 
                     abs(dleverage_w<0.4) & fixedassetratio_w != 0
                   & mkt_to_book_w != 0 & realsizeUSD_w != 0 &
                     capex_TA_w != 0 & RandD_sales_w != 0 )
summary(fm.ols.lev)
dust(fm.ols.lev) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_1.csv')
extract(fm.ols.lev, include.adjrs=F)

fm.ols.net <- felm(dnetleverage_w ~  log(fixedassetratio_w)  + log(mkt_to_book_w) + 
                     log(realsizeUSD_w) + log(capex_TA_w) + log(RandD_sales_w) + 
                     dividendstatus + Dzombie3q:period2 |  SIC2_year +
                     cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1 &
                     abs(dnetleverage_w<0.4) & fixedassetratio_w != 0
                   & mkt_to_book_w != 0 & realsizeUSD_w != 0 &
                     capex_TA_w != 0 & RandD_sales_w != 0 )
summary(fm.ols.net)
dust(fm.ols.net) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_2.csv')
extract(fm.ols.net, include.adjrs=F)
# fm.ols.cash <- felm(dcashratio_w ~  fixedassetratio_w + mkt_to_book_w + 
#                       log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                       Dzombie3q:period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# 
# fm.ols.equity <- felm(equityissuance_TA_w ~  fixedassetratio_w  + mkt_to_book_w + 
#                         log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                         Dzombie3q:period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))

fm.ols.asset <- felm(assetdisposal_TA_w ~  log(fixedassetratio_w) + log(mkt_to_book_w) + 
                       log(realsizeUSD_w) + log(capex_TA_w) + log(RandD_sales_w) + 
                       dividendstatus + Dzombie3q:period2 |  SIC2_year + 
                       cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1 & 
                       abs(dleverage_w<0.4) & fixedassetratio_w != 0
                     & mkt_to_book_w != 0 & realsizeUSD_w != 0 &
                       capex_TA_w != 0 & RandD_sales_w != 0)
summary(fm.ols.asset)
dust(fm.ols.asset) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_3.csv')
extract(fm.ols.asset, include.adjrs=F)
# Check that zombie firms aren't getting relatively more profitable over time
# fm.ols.ebit <- felm(ebit_TA_w ~  fixedassetratio_w + mkt_to_book_w + 
#                       log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                       Dzombie3q:period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# dust(fm.ols.ebit) %>% 
#   sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
#   sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
#   sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
#                     "P-value") %>%
#   write.csv('reg_4.csv')
# fm.ols.intebit <- felm(interestlessebit_TA_w ~  fixedassetratio_w + mkt_to_book_w + 
#                          log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                          Dzombie3q:period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1)
# summary(fm.ols.intebit)
# 
# fm.ols.int <- felm(interest_TA_w ~  fixedassetratio_w  + mkt_to_book_w + 
#                      log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus + 
#                      Dzombie3q:period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.int)

# fm.ols.int <- felm(interest_subsidy ~ fixedassetratio_w + mkt_to_book_w +
#                       log(realsizeUSD_w) + capex_TA_w + RandD_sales_w + dividendstatus +
#                       Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.int)
# 
# fm.ols.int <- felm(interest_subsidy ~ fixedassetratio_w + mkt_to_book_w + shareoflongtermdebt_w +
#                      log(realsizeUSD_w) + capex_TA_w + RandD_sales_w + dividendstatus +
#                      Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4) & year>1984)
# summary(fm.ols.int)
# 
# fm.ols.int <- felm(interest_subsidy ~ fixedassetratio_w  + mkt_to_book_w + shareoflongtermdebt_w +
#                       log(realsizeUSD_w) + capex_TA_w + RandD_sales_w + dividendstatus +
#                       Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4) & year>1984 & abs(interest_subsidy)<10)
# summary(fm.ols.int)
# 
# fm.fe.int <- felm(interest_subsidy~ fixedassetratio_w + mkt_to_book_w +
#                      log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                      Dzombie3q*period2 | WC06105 + year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1)
# summary(fm.ols.int)
# 

# fm.ols.ebit <- felm(ebit_TA_w ~ fixedassetratio_w + mkt_to_book_w +
#                       log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                       Dzombie3q*period2 | SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.ebit)
# 
# fm.ols.div <- felm(dividends_TA_w ~ fixedassetratio_w + mkt_to_book_w + 
#                      log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                      Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.div)
# 
# fm.ols.capex <- felm(capex_TA_w ~ fixedassetratio_w + mkt_to_book_w +
#                        log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                        Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.capex)
# 
# fm.ols.ltdebt <- felm(shareoflongtermdebt_w ~ fixedassetratio_w + mkt_to_book_w +
#                         log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                         Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.ltdebt)
# 
# fm.ols.dinv <- felm(decreaseininvestments_TA_w ~  fixedassetratio_w + mkt_to_book_w + 
#                        log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus +
#                        Dzombie3q*period2 |  SIC2_year + cc_year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1& abs(dleverage_w<0.4))
# summary(fm.ols.dinv)
# 
# fm.fe.dinv <- felm(decreaseininvestments_TA_w ~  fixedassetratio_w + mkt_to_book_w + 
#                        log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus + 
#                        Dzombie3q*period2 | WC06105 + year | 0 | WC06105 + year, data=dat, subset=ebit_TA_w>-1)
# summary(fm.fe.dinv)
# 
# coeffs <- c(coef(fm.ols.lev)["Dzombie3q"],coef(fm.ols.cash)["Dzombie3q"],
#             coef(fm.ols.asset)["Dzombie3q"]/100,coef(fm.ols.equity)["Dzombie3q"],
#             coef(fm.ols.int)["Dzombie3q"],coef(fm.ols.ebit)["Dzombie3q"],
#             coef(fm.ols.capex)["Dzombie3q"],
#             coef(fm.ols.lev)["Dzombie3q"] + coef(fm.ols.lev)["Dzombie3q:period22.post2000"] ,
#             coef(fm.ols.cash)["Dzombie3q"]+ coef(fm.ols.cash)["Dzombie3q:period22.post2000"],
#             coef(fm.ols.asset)["Dzombie3q"]/100+ coef(fm.ols.asset)["Dzombie3q:period22.post2000"]/100,
#             coef(fm.ols.equity)["Dzombie3q"]+ coef(fm.ols.equity)["Dzombie3q:period22.post2000"],
#             coef(fm.ols.int)["Dzombie3q"]+ coef(fm.ols.int)["Dzombie3q:period22.post2000"],
#             coef(fm.ols.ebit)["Dzombie3q"]+ coef(fm.ols.ebit)["Dzombie3q:period22.post2000"],
#             coef(fm.ols.capex)["Dzombie3q"]+ coef(fm.ols.capex)["Dzombie3q:period22.post2000"])
# variable <- c("1.Dleverage","2.Dcashratio","3.Assetdisposal","4.Equityissuance","2.Interest paid","6.Ebit", "7.Capex",
#               "1.Dleverage","2.Dcashratio","3.Assetdisposal","4.Equityissuance","2.Interest paid","6.Ebit", "7.Capex")
# sigdiff <- c("Yes","Yes","Yes","No","Yes","Yes","Yes","Yes","No","Yes","No","Yes","No","Yes")
# period <- c(rep("1.pre2000",7),rep("2.post2000",7))
# period <- c(rep("1.pre2010",7),rep("2.post2010",7))
# 
# dat_zombie_survival <- data.frame(variable, period, coeffs, sigdiff)
# 
# 
# p <- ggplot(dat_zombie_survival, aes(y=coeffs, x=variable, fill=period, alpha=sigdiff)) + 
#   geom_bar(stat="identity",position=position_dodge()) + scale_alpha_discrete(range = c(0.3, 1)) + ggtitle("Zombie cashflow compare with non-zombies\n (as a share of total assets)")
# print(p)
# 
# # For Claudio's speech
# dat_zombie_survival <- subset(dat_zombie_survival, variable %in%c("1.Dleverage","2.Interest paid","3.Assetdisposal","7.Capex"))
# p <- ggplot(dat_zombie_survival, aes(y=coeffs, x=variable, fill=period)) + 
#   geom_bar(stat="identity",position=position_dodge()) +  ggtitle("Zombies compared with non-zombies\n (as a share of total assets)")
# print(p)



# Potential drivers data prep for graphs ----------------------------------
potential_drivers <- dat[, c('year', 'cc', 'avg_p2b_ratio', 'intr_sr', 'Dzombie3q')] %>%
  group_by(year, cc) %>%
  summarise(avg_p2b_ratio = mean(avg_p2b_ratio),
            intr_sr = mean(intr_sr),
            z_share = mean(Dzombie3q)) %>%
  ungroup()  
write.csv(potential_drivers, 'potential_drivers.csv', row.names = FALSE)


######################################################################################################################
# 4. Effect of low interest rates on zombie share - regression using cross-section (external financial dependence) ----
#   H0: b3<0, ie sectors with more external financial dependence - low rates have greater effect on zshare
#   zombieshare(c,s,t) = a + b1*r(c,t) + b2*ExtFinDep(s) + b3*r(c,t)*ExtFinDep(s) + controls. 
######################################################################################################################
# Basic regressions first
dat_zom <- subset(dat, !is.na(Dzombie3q) & year<2018)
# dat_zom$polrate <- ifelse(dat_zom$cc%in%c("US","DE","CA","GB","JP","AU"), dat_zom$polrate, NA)
a <- dat_zom[,.(Dzombie3q_agg = sum(Dzombie3q, na.rm=T)/.N,
                Dzombie3q_aggTA = sum(Dzombie3q*WC02999, na.rm=T)/sum(WC02999,na.rm=T),
                bis_polrate = mean(bis_polrate, na.rm=T),
                intr_lr = mean(intr_lr, na.rm=T),
                intr_sr = mean(intr_sr, na.rm=T),
                realpolrate = mean(realpolrate, na.rm=T)), by=c("cc","year")]
dat_zom_mean_cc <- a[,.(mean_zshare =mean(Dzombie3q_agg, na.rm=T)*100,
                        median_zshare =median(Dzombie3q_agg, na.rm=T)*100,
                        mean_polrate=mean(bis_polrate, na.rm=T),
                        mean_intr_lr=mean(intr_lr, na.rm=T),
                        mean_intr_sr=mean(intr_sr, na.rm=T),
                        mean_realpolrate=mean(realpolrate, na.rm=T),
                        median_polrate=median(bis_polrate, na.rm=T))
                     , by=c("year")]

fm <- lm(mean_zshare ~ mean_polrate, data=dat_zom_mean_cc)
summary(fm)
fm.IRL <- lm(mean_zshare ~ mean_intr_lr, data=dat_zom_mean_cc)
summary(fm.IRL)
fm.real <- lm(mean_zshare ~ mean_realpolrate, data=dat_zom_mean_cc)
summary(fm.real)


#### With country-sector zshares 
fm.ccsic.1.int <- felm(I(fzshareK_w*100) ~ I(ExtFinDep):I(intr_lr) | cc_year + SIC2 | 0 | cc_year + SIC2_year , data=dat_reg, subset=year>1986 & year < 2019)
print(summary(fm.ccsic.1.int))
extract(fm.ccsic.1.int, include.adjrs=F)

fm.ccsic.2.int <- felm(I(fzshareK_w*100) ~ I(ExtFinDep):I(intr_sr) | cc_year + SIC2 | 0 | cc_year + SIC2_year , data=dat_reg, subset=year>1986 & year < 2019)
print(summary(fm.ccsic.2.int))
extract(fm.ccsic.2.int, include.adjrs=F)

fm.ccsic.1.pol <- felm(I(fzshareK_w*100) ~ I(ExtFinDep):I(bis_polrate) | cc_year + SIC2 | 0 | cc_year + SIC2_year , data=dat_reg, subset=year>1986 & year < 2019)
print(summary(fm.ccsic.1.pol))
extract(fm.ccsic.1.pol, include.adjrs=F)

fm.ccsic.1.bank <- felm(I(fzshareK_w*100) ~ I(ExtFinDep):I(avg_p2b_ratio)  | cc_year + SIC2 | 0 | cc_year + SIC2_year , data=dat_reg, subset=year>1986 & !is.na(bis_polrate))
print(summary(fm.ccsic.1.bank))
extract(fm.ccsic.1.bank , include.adjrs=F)

fm.ccsic.1.bank.2 <- felm(I(fzshareK_w*100) ~ I(ExtFinDep):I(avg_p2b_ratio) + I(ExtFinDep):I(intr_sr) | cc_year + SIC2 | 0 | cc_year + SIC2_year , data=dat_reg, subset=year>1986 & year < 2019)
print(summary(fm.ccsic.1.bank.2))
extract(fm.ccsic.1.bank.2 , include.adjrs=F)

dust(fm.ccsic.1.bank.2) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_ext_fn.csv')


# Firm regressions on effective rate --------------------------------------------

# Regress the rate the firm pays on its debts onto the policy rate and zombie dummy
# Effective rate the firm pays on its debt is aveinterestrate - already limited to 0-100
# Use intr_sr to proxy the policy rate - much better coverage than the BIS central bank rate database
# Important not to use cc_year fixed effects as otherwise matrix becomes indefinite due to rank
fm.ols.effectiverate <- felm(aveinterestrate_w ~  Dzombie3q  + intr_lr + 
                               log(fixedassetratio_w) + log(mkt_to_book_w) + 
                      log(realsizeUSD_w) + log(capex_TA_w) + log(RandD_sales_w) + 
                        dividendstatus | SIC2_year + cc  | 0 | WC06105 + year, data=dat,
                      subset = fixedassetratio_w != 0
                      & mkt_to_book_w != 0 & realsizeUSD_w != 0 &
                        capex_TA_w != 0 & RandD_sales_w != 0)
summary(fm.ols.effectiverate)
extract(fm.ols.effectiverate)
dust(fm.ols.effectiverate) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_5.csv')

# fm.ols.effectiverate.2 <- felm(Dzombie3q ~ aveinterestrate_w + fixedassetratio_w + mkt_to_book_w +
#                                log(realsizeUSD_w) + capex_TA + RandD_sales_w + dividendstatus | SIC2_year  | 0 | WC06105 + year, data=dat)
# summary(fm.ols.effectiverate.2)

# Productivity densities --------------------------------------------------
prod_density <- dat[, c('Dzombie3q', 'labourprod_perworker_w', 'labourprod_w',
                        'capitalprod_w', 'lrealva_wagesgrossprofit')]
write.csv(prod_density, paste0(output_dir,'productivity_density.csv'), row.names = F)

# With no <0 ommitted - still significant negative on z dummy
# fm.ols.labourprod_w <- felm(labourprod_w ~  Dzombie3q + 
#                               log(fixedassetratio_w) + log(mkt_to_book_w) + 
#                               log(realsizeUSD_w) + log(capex_TA_w) + 
#                               log(RandD_sales_w) + dividendstatus | SIC2_year + cc_year  | 0 | WC06105 + year,
#                             data=dat, subset = fixedassetratio_w > 0 &
#                               mkt_to_book_w > 0 &  realsizeUSD_w > 0 &
#                               capex_TA_w > 0 & RandD_sales_w > 0)

fm.ols.labourprod_w <- felm(log(labourprod_w) ~  Dzombie3q + 
                            log(fixedassetratio_w) + log(mkt_to_book_w) + 
                               log(realsizeUSD_w) + log(capex_TA_w) + 
                                 log(RandD_sales_w) + dividendstatus | SIC2_year + cc_year  | 0 | WC06105 + year,
                               data=dat, subset = fixedassetratio_w > 0 &
                               mkt_to_book_w > 0 &  realsizeUSD_w > 0 &
                               capex_TA_w > 0 & RandD_sales_w > 0 & 
                              labourprod_w > 0)
summary(fm.ols.labourprod_w)
extract(fm.ols.labourprod_w)
dust(fm.ols.labourprod_perworker_w) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_6.csv')

# With no <0 ommitted - still significant negative on z dummy
# fm.ols.capitalprod_w <- felm(capitalprod_w ~  Dzombie3q +
#                               log(fixedassetratio_w) + log(mkt_to_book_w) +
#                               log(realsizeUSD_w) + log(capex_TA_w) +
#                               log(RandD_sales_w) + dividendstatus | SIC2_year + cc_year  | 0 | WC06105 + year,
#                             data=dat, subset = fixedassetratio_w > 0 &
#                               mkt_to_book_w > 0 &  realsizeUSD_w > 0 &
#                               capex_TA_w > 0 & RandD_sales_w > 0)

fm.ols.capitalprod_w <- felm(log(capitalprod_w) ~  Dzombie3q + 
                               log(fixedassetratio_w) + log(mkt_to_book_w) + 
                               log(realsizeUSD_w) + log(capex_TA_w) + 
                               log(RandD_sales_w) + dividendstatus | SIC2_year + cc_year  | 0 | WC06105 + year,
                             data=dat, subset = fixedassetratio_w > 0 &
                               mkt_to_book_w > 0 &  realsizeUSD_w > 0 &
                               capex_TA_w > 0 & RandD_sales_w > 0 & 
                               capitalprod_w > 0)
summary(fm.ols.capitalprod_w)
extract(fm.ols.capitalprod_w)
dust(fm.ols.capitalprod_w) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 4) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-stat", 
                    "P-value") %>%
  write.csv('reg_7.csv')

# fm.ols.lrealva_wagesgrossprofit_w <- felm(lrealva_wagesgrossprofit_w ~  Dzombie3q + 
#                                             log(fixedassetratio_w) + log(mkt_to_book_w) + 
#                                             log(realsizeUSD_w) + log(capex_TA_w) + 
#                                             log(RandD_sales_w) + dividendstatus | SIC2_year + cc_year  | 0 | WC06105 + year,
#                                           data=dat, subset = fixedassetratio_w > 0 &
#                                             mkt_to_book_w > 0 &  realsizeUSD_w > 0 &
#                                             capex_TA_w > 0 & RandD_sales_w > 0)
# summary(fm.ols.lrealva_wagesgrossprofit_w)
# TZombie congestion (regressions) -----------------------

##### Capex / fixed capital - Baseline

fm.zshareTA.kq <- felm(log(capex_Kfixed_w) ~ NDzombie3q*zshareTA_w + age + log(realsizeUSD_w) | SIC2_year + cc_year | 0 |SIC2 + cc , dat=dat, exactDOF=TRUE,subset=capex_Kfixed_w>0)
extract(fm.zshareTA.kq)
summary(fm.zshareTA.kq)

#### Growth in labour force
#### With sector year and country year effects - Total asset weighted congestion

fm.zshareTA.lq <- felm(fdemp_w ~ NDzombie3q*zshareTA_w + age + log(realsizeUSD_w) | SIC2_year + cc_year | 0 |SIC2 + cc , dat=dat, exactDOF=TRUE)
extract(fm.zshareTA.lq)
summary(fm.zshareTA.lq)

