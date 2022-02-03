#' @import data.table
#' @import openxlsx

# These methods provide access to non api based datasets. Currently they download the files
# directly from the internet, but should be made more generic. Maybe scraping the site for pages. 
# Doing a depth first search of all html pages under hud and scraping for datasets and putting
# them together.



#' @name hudlithtc
#' @title hudlithtc
#' @description Returns a data frame from https://lihtc.huduser.gov/ 
#' @param key The token given by USPS
#' @returns A data frame built from the file.
#' @export
hudlihtc <- function(file = 1) {
  if(file == 1) {
    # County level 
    return(fread("https://lihtc.huduser.gov/lihtcpub.csv", header = TRUE, stringsAsFactors = FALSE))   
  } else {
    
  }
}

# Neighborhood stabilization program.

#' @name hudnsp
#' @title hudnsp
#' @description Returns a data frame pertaining to the Neighborhood Stabilization Program provided by HUD User or HUD Exchange
#' @param file The specific file needed.
#' @returns A data frame built from file.
#' @export
hudnsp <- function(file = 1) {
  if(file == 1) {
    return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/Neighborhood_Foreclosure_Data.xlsx"))
  } else {
    return(read.xlsx("https://drgr.hud.gov/public/downloads/NSP/NSP%20Close-out_Admin%20Rept02b%20-%20with%20Act%20Start%20and%20End%20dates%20and%20ENV%20Status.xlsx"))
  }
}


# The Department of Housing and Urban Development establishes the rent adjustment
# factors - called Annual Adjustment Factors (AAFs) - on the basis of Consumer 
# Price Index (CPI) data relating to changes in residential rent and utility costs.
# Supports 2019-2021 for now as they are the only ones with excel files. 
# Otherwise need program to parse pdf files.
# Should be normally 1-2 sheets.

#' @name hudadjf
#' @title hudadjf
#' @description Returns a data frame from HUD USER pertaining to the Annual Adjustment Factors dataset.
#' @param year The specific year for the Annual Adjustment Factors to query for.
#' @param sheet The specific sheet in the file.
#' @returns A data frame built from the file.
#' @export
hudadjf <- function(year = 2021, sheet = 1) {
  return(read.xlsx(paste("https://www.huduser.gov/portal/datasets/aaf/", as.character(year), "ScheduleC.xlsx", sep = ""), sheet = sheet))
}


# Difficult Development Areas (DDA) are areas with high land, construction and utility costs relative to the area median income and are based on Fair Market Rents, income limits, the 2010 census counts, and 5-year American Community Survey (ACS) data. Maps of Qualified Census Tracts and Difficult Development Areas are available at: huduser.gov/sadda/sadda_qct.html.
# Supports 2017-2022 for now. Should be 1-2 sheets.

#' @name huddda
#' @title huddda
#' @description Returns a data frame from HUD USER pertaining to the Difficult Development Areas.
#' @param year The specific year for the Difficult Development Areas to query for.
#' @param sheet The specific sheet in the file.
#' @returns A data frame built from the file.
#' @export
huddda <- function(year = 2022 , sheet = 1) {
  return(read.xlsx(paste("https://www.huduser.gov/portal/datasets/qct/", as.character(year), "-DDAs-Data-Used-to-Designate.xlsx", sep = ""), sheet = sheet))
}


# HAF funds are used for qualified expenses that assist homeowners having
# incomes equal to or less than 150 percent of the greater of the area median
# income for their household size, or the area median income for the United States, 
# as determined by the Secretary of Housing and Urban Development. 
# The Homeowner Assistance Fund (HAF) Income Limits are used for determining eligibility for HAF funds.
# Supports 2022 data. Should be 1 sheet.

#' @name hudhaf
#' @title hudhaf
#' @description Returns a data frame from HUD USER pertaining to the Homeowner Assistance Fund Income Limits
#' @param year The specific year for the Homeowner Assistance Fund Income Limits to queryt for.
#' @returns A data frame built from the file.
#' @export
hudhaf <- function(year = 2022) {
  return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/datasets/haf-il/il21_all100_150_HAF.xlsx"))
}


# Rent estimates at the 50th percentile (or median) are calculated for all Fair Market Rent areas. THESE ARE NOT FAIR MARKET RENTS. Under certain conditions, as set forth in the Interim Rule (Federal Register Vol. 65, No. 191, Monday October 2, 2000, pages 58870-58875), these 50th percentile rents can be used to set success rate payment standards.
# Can specify the resolution of the 50th percentile by either "c" for county or "a" for area.
# Currently supports 2016-2022 data.


#' @name hud50p
#' @title hud50p
#' @description Returns a data frame from HUD USER pertaining to the 50th percentile (or median)
#' @param year The specific year for the 50th percentile (or median) to query for.
#' @param res Can choose between county or FMR area resolution for 50% percentile.
#' @returns A data frame built from the file.
#' @export
hud50p <- function(year = 2022, res = "c") {
  if(res == "c") {
    return(read.xlsx(paste("https://www.huduser.gov/portal/datasets/50thper/FY", 2022, "_50_County.xlsx", sep = "")))    
  } else {
    return(read.xlsx(paste("https://www.huduser.gov/portal/datasets/50thper/FY", 2022, "_50_FMRArea.xlsx", sep = ""))) 
  }
}

# The Housing Affordability Data System (HADS) is a set of files derived from the 1985 
# and later national American Housing Survey (AHS) and the 2002 and later Metro AHS. 
# This system categorizes housing units by affordability and households by income, with respect 
# to the Adjusted Median Income, Fair Market Rent (FMR), and poverty income. It also includes 
# housing cost burden for owner and renter households. These files have been the basis for the 
# worst case needs tables since 2001. The data files are available for public use, since they 
# were derived from AHS public use files and the published income limits and FMRs.
# We are providing these files give the community of housing analysts the opportunity to
# use a consistent set of affordability measures.

# Years are recorded every two years (2013,2011,2009). No data past 2013 available.


#' @name hudhads
#' @title hudhads
#' @description Returns a data frame from HUD USER pertaining to the Housing Affordability Data System.
#' @param year The specific year for the Housing Affordability Data System to query for. 
#' Years are recorded every two years (2013,2011,2009). No data past 2013 available.
#' @returns A data frame built from the file.
#' @export
hudhads <- function(year = 2013) {
  # Need to parse out a zip file of txt format. However, it is in CSV format so should be able
  # to easily convert into a dataframe.
}

# The Components of Inventory Change (CINCH) report measures changes in the 
# characteristics of the housing stock of the United States. Using data collected
# from the national American Housing Survey (AHS), conducted every two years, 
# the characteristics of individual housing units are compared across time.
# This comparison allows researchers to see not only changes in the characteristics
# of housing units, but also in the characteristics of occupants. Information 
# is available on the characteristics of units added and removed from the housing stock.
# First-time users of this publication are advised to refer to the Appendices 
# and notes immediately preceding the tables for explanations of the columns 
# and how to interpret the results. The Appendices also contain the definitions
# of all of the characteristics, cautions, and explanations about some of the results.
# Each report has two sections. The first section, labeled "Losses," shows the
# losses to the housing stock in the two-year period using the first year as the 
# base year. In other words, the first section shows the disposition of all the
# units that were present at the beginning of the period. The second section,
# labeled "Gains," shows the gains in the housing stock during the two-year 
# period, using the second year of the period as the base year. That is to say,
# the second section shows the source of all the units that were present at the
# end of the period.

# Provides data from 2001 to 2017. They are in multiples of two. So 2017-2015, 2011-2013
# 2009-2011, 2009 -2007, 2003-2005, 2001-2003


#' @name hudcinch
#' @title hudcinch
#' @description Returns a data frame from HUD USER pertaining to the Components of Inventory Change (CINCH) report.
#' @param year The specific year for the Components of Inventory Change (CINCH) report to query for. Years must be a character type specifying
#' a year range:
#' 1) 2017-2015,
#' 2) 2011-2013,
#' 3) 2009-2011, 
#' 4) 2009 -2007, 
#' 5) 2003-2005, 
#' 6) 2001-2003
#' @returns A data frame built from the file.
#' @export
hudcinch <- function(year = '2017-2015') {
  # Must unzip a zip file and extract the two files inside: 
  # tflcinch_skinny.csv
  # tblcinch_skinny.csv
}



# This report outlines the key findings of the Point-In-Time (PIT) count 
# and Housing Inventory Count (HIC) conducted in January 2020. Specifically,
# this report provides 2020 national, state, and CoC-level PIT and HIC estimates 
# of homelessness, as well as estimates of chronically homeless persons, homeless 
# veterans, and homeless children and youth.

# From 2020 - 2007
# c = Coc
# s = State

# Get veteran PIT
# s = standard
# v = veterans

#' @name hudpit
#' @title hudpit
#' @description Returns a data frame from HUD USER pertaining to the Point-In-Time (PIT) count.
#' @param year The specific year for the Point-In-Time (PIT) count to query for. Supports years 2020 to 2007.
#' @param res The resolution of the dataset. Either Coc or State level data.
#' @param pop Either v for veterans or s for standard population.
#' @returns A data frame built from the file.
#' @export
hudpit <- function(year = 2020, res = "s", pop = "s") {
  if(res == "s") {
    if(pop == "s") {
      years = seq(2020, 2007)
      return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/2007-2020-PIT-Estimates-by-state.xlsx", sheet = match(year, years)))  
    } else if(pop == "v") {
      years = seq(2020, 2011)
      return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/2011-2020-PIT-Veteran-Counts-by-State.xlsx", sheet = match(year, years)))  
    }
  } else if(res == "c") {
    if(pop == "s") {
      years = seq(2020, 2007)
      return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/2007-2020-PIT-Estimates-by-CoC.xlsx", sheet = match(year, years)))  
    } else if(pop == "v") {
      years = seq(2020, 2011)
      return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/2011-2020-PIT-Veteran-Counts-by-CoC.xlsx", sheet = match(year, years)))  
    }
  }
  return(NULL)
}

# This report outlines the key findings of the Point-In-Time (PIT) count 
# and Housing Inventory Count (HIC) conducted in January 2020. Specifically,
# this report provides 2020 national, state, and CoC-level PIT and HIC estimates 
# of homelessness, as well as estimates of chronically homeless persons, homeless 
# veterans, and homeless children and youth.

# From 2020 - 2007
# c = Coc
# s = State

#' @name hudhic
#' @title hudhic
#' @description Returns a data frame from HUD USER pertaining to the Housing Inventory Count (HIC).
#' @param year The specific year for the Housing Inventory Count (HIC) count to query for. Supports years 2020 to 2007.
#' @param res The resolution of the dataset. Either Coc or State level data.
#' @returns A data frame built from the file.
#' @export
hudhic <- function(year = 2020, res = "s") {
  years = seq(2020, 2007)
  if(res == "s") {
    return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/2007-2020-HIC-Counts-by-State.xlsx", sheet = match(year, years))) 
  } else {
    return(read.xlsx("https://www.huduser.gov/portal/sites/default/files/xls/2007-2020-HIC-Counts-by-CoC.xlsx", sheet = match(year, years))) 
  }
}


# HUD has issued new HOME Investment Partnerships Program (HOME) and Housing Trust
# Fund (HTF) Homeownership Value Limits ("95% limits") for 2021 that are effective 
# June 1, 2021. In 24 CFR 92.254(a)(2)(iii) of the Final Rule published on July 24,
# 2013 and effective August 23, 2013, HUD established new homeownership value limits
# for HOME Participating Jurisdictions (PJs). In addition, 24 CFR 93.305(a)(1) of the 
# HTF Interim Rule states that HUD will provide limits for affordable newly constructed
# housing and existing housing based on 95 percent of the median purchase price for the area.
# Supports years 2021-2014
# Both HTF and HOME are in same dataset

#' @name hudhvl
#' @title hudhvl
#' @description Returns a data frame from HUD USER pertaining to the HOME Investment Partnerships Program (HOME) and Housing Trust
#' Fund (HTF).
#' @param year The specific year for the HOME Investment Partnerships Program (HOME) and Housing Trust
#' Fund (HTF) count to query for. Supports years 2021 to 2014.
#' @returns A data frame built from the file.
#' @export
hudhvl <- function(year = 2021) {
  return(read.xlsx("https://www.huduser.gov/portal/datasets/home-datasets/files/FY-", as.character(year), "-HOME-HTF-Homeownership-Sales-Limits.xlsx"))
}

# Multifamily Tax Subsidy Projects (MTSP) Income Limits were developed to meet
# the requirements established by the Housing and Economic Recovery Act of 2008 
# (Public Law 110-289) that allows 2007 and 2008 projects to increase over time.
# The MTSP Income Limits are used to determine qualification levels as well as 
# set maximum rental rates for projects funded with tax credits authorized under
# section 42 of the Internal Revenue Code (the Code) and projects financed with 
# tax exempt housing bonds issued to provide qualified residential rental development
# under section 142 of the Code.


#' @name hudmsil
#' @title hudmsil
#' @description Returns a data frame from HUD USER pertaining to the Multifamily Tax Subsidy Projects (MTSP) Income Limits.
#' @param year The year to query for in Multifamily Tax Subsidy Projects (MTSP) Income Limits. Supports years 2021 to 2014.
#' @returns A data frame built from the file.
#' @export
hudmsil <- function(year = 2021, avg = FALSE) {
  year = substr(as.character(year), nchar(year) - 1, nchar(year))
  if(avg) {
    return(read.xlsx(paste("https://www.huduser.gov/portal/datasets/mtsp/mtsp", year, "/MTSP-Data.xlsx")))
  } else {
    return(read.xlsx(paste("https://www.huduser.gov/portal/datasets/mtsp/mtsp", year, "/MTSP_IncAvg_Data.xlsx")))
  }
}

# The Department of Housing and Urban Development's Office of Housing permits 
# owners/agents to use Utility Allowance Factors (UAFs) for projects subject
# to the requirements found in Housing's Utility Analysis Notice.
# The notice describes when owners/agents of Multifamily Housing 
# properties that receive a utility allowance may use the published
# UAFs to adjust these allowances. The dataset is updated once per year
# and is published with an effective date. The dataset is based on information
# published by the U.S. Energy Information Administration and the Bureau of
# Labor Statistics. Owners/agentps will determine their utility and state 
# specific UAF and apply the published UAF to their existing allowance for 
# the utility allowance they are adjusting.
# Allows years 2022 -2015


#' @name hudmuaf
#' @title hudmuaf
#' @description Returns a data frame from HUD USER pertaining to the Utility Allowance Factors (UAFs) for projects.
#' @param year The specific year for the Utility Allowance Factors (UAFs) for projects. Supports years 2022 to 2015.
#' @returns A data frame built from the file.
#' @export
hudmuaf <- function(year = 2022) {
  year = substr(as.character(year), nchar(year) - 1, nchar(year))
  return(read.xlsx("https://www.huduser.gov/portal/datasets/UtilAllow_FY", as.character(year), ".xlsx"))
}


# Renewal Funding Inflation Factors are used in the allocation of Housing Choice
# Voucher funds among Public Housing Agencies. Prior to FY 2012, these factors
# were called Renewal Funding Annual Adjustment Factors. In the FY 2012 HUD 
# Appropriations bill, Congress changed the name of these factors in recognition
# of HUD's new methodology for calculating the factors, which now take advantage
# of ongoing PD&R research aimed at better understanding the drivers of change 
# in per-unit-costs (PUC) in the voucher program. Renewal Funding Inflation Factors
# have been developed to account for the expected annual change in average PUC in 
# the voucher program using historical program data, coupled with several economic 
# indices used to capture key components of the economic climate which also assist 
# in explaining the changes in PUC.
# Allows years 2021-2017

#' @name hudrfif
#' @title hudrfif
#' @description Returns a data frame from HUD USER pertaining to the Renewal Funding Inflation Factors.
#' @param year The specific year for the Renewal Funding Inflation Factors. Supports years 2021 to 2017.
#' @returns A data frame built from the file.
#' @export
hudrfif <- function(year = 2021) {
  return(read.xlsx("https://www.huduser.gov/portal/datasets/rfif/FY", as.character(year), "/RFIF_", as.character(year), ".xlsx"))
}


# Small Area Fair Market Rents (SAFMRs) are FMRs calculated for
# ZIP Codes within Metropolitan Areas. Small Area FMRs are required to be 
# used to set Section 8 Housing Choice Voucher payment standards in areas
# designated by HUD (available here). Other Housing Agencies operating in 
# non-designated metropolitan areas may opt-in to the use of Small Area FMRs. 
# Furthermore, Small Area FMRs may be used as the basis for setting Exception
# Payment Standards - PHAs may set exception payment standards up to 110 percent of 
# the Small Area FMR. PHAs administering Public Housing units may use Small Area
# FMRs as an alternative to metropolitan area-wide FMRs when calculating Flat Rents.
# Please See HUD's Small Area FMR Final Rule for additional information regarding the
# uses of Small Area FMRs.
# Supports years 2022 to 2016

#' @name hudsfmr
#' @title hudsfmr
#' @description Returns a data frame from HUD USER pertaining to the Small Area Fair Market Rents (SAFMRs).
#' @param year The specific year for the Small Area Fair Market Rents. Supports years 2022 to 2016.
#' @returns A data frame built from the file.
#' @export
hudsfmr <- function(year = 2022) {
  return(read.xlsx("https://www.huduser.gov/portal/datasets/fmr/fmr", as.character(year), "/fy", as.character(year), "_safmrs.xlsx"))
}

# Picture of of Subsidized Households
# Assisted Housing: Local and Global
# Since passage of the U.S. Housing Act of 1937, the federal government
# has provided housing assistance to low-income renters. Most of these housing 
# subsidies were provided under programs administered by the U.S. Department of 
# Housing and Urban Development (HUD) or predecessor agencies. All programs 
# covered in this report provide subsidies that reduce rents for low-income 
# tenants who meet program eligibility requirements. Generally, households 
# pay rent equal to 30 percent of their incomes, after deductions, while the 
# federal government pays the remainder of rent or rental costs. To qualify 
# for a subsidy, an applicant's income must initially fall below a certain income 
# limit. These income limits are HUD-determined, location specific, and vary by 
# household size. Applicants for housing assistance are usually placed on a waiting
# list until a subsidized unit becomes available.
# https://www.huduser.gov/portal/datasets/assthsg.html


#' @name hudash
#' @title hudash
#' @description Returns a data frame from HUD USER pertaining to the Picture of Subsidized Households.
#' @param year The specific year for the Picture of Subsidized Households. Supports years 2021 to 2009.
#' @param res The GEOID or place resolution(2012-2021 are based 2010 census geographies)
#' 1) U.S. Total
#' 2) State
#' 3) Core Based Statistical Areas
#' 4) Public Housing Agency
#' 5) Project
#' 6) Census Tract: AK - MN
#' 7) Census Tract: MO - WY
#' 8) City
#' 9) County
#' 10) Congressional District
#' 11) Zipcode
#' @returns A data frame built from the file.
#' @export
hudash <- function(year = 2021, res = 1) {
  res <- switch(res,
         "US",
         "STATE",
         "CBSA",
         "PHA",
         "PROJECT",
         "TRACT_AK_MN",
         "TRACT_MO_WY",
         "PLACE",
         "COUNTY",
         "CD",
         "Zipcode",
         )
  
  return(read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/", res, "_", as.character(year), ".xlsx"))
}

