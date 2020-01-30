library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Self-Employment by Business Type by County
# Created by Jenna Daly
# On 03/06/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2018,
    table = "S2406"
)

business_type <- data.table()
for (data in acsdata) {
    year <- data@endyear
      total <- acsSum(data, 1, "Total") 
      private_p <- acsSum(data, 2, "Private, Profit") 
      self_own <- acsSum(data, 3, "Self, Own") 
      private_np <- acsSum(data, 4, "Private, Not Profit") 
      govt <- acsSum(data, 5, "Government") 
      self_not_own <- acsSum(data, 6, "Self, Not Own") 

    datafips <- data.table(fips = getACSFips(data))
    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(total),
        estimate(private_p),
        estimate(self_own),
        estimate(private_np),
        estimate(govt),
        estimate(self_not_own)
    )
    
    names(estimates)[grep("(.*Total.*Estimate|.*Estimate.*Total).*Civilian employed population 16 years and over$", names(estimates))] <- "Total"                                                                    
    names(estimates)[grep("(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Civilian employed population 16 years and over$", names(estimates))] <- "Private, Profit"                                             
    names(estimates)[grep("(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Civilian employed population 16 years and over$", names(estimates))] <- "Self-Employed, Incorporated"                              
    names(estimates)[grep("(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Civilian employed population 16 years and over$", names(estimates))] <- "Private, Not-for-profit"                                  
    names(estimates)[grep("(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Civilian employed population 16 years and over$", names(estimates))] <- "Government"                                    
    names(estimates)[grep("(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Civilian employed population 16 years and over$", names(estimates))] <- "Self-Employed, Not Incorporated"
    
    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Business Type",
        variable.factor = F,
        value.name = "Percent",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(total) * 1.645,
        standard.error(private_p) * 1.645,
        standard.error(self_own) * 1.645,
        standard.error(private_np) * 1.645,
        standard.error(govt) * 1.645,
        standard.error(self_not_own) * 1.645
    )
    
    names(moes)[grep("(.*Total.*Estimate|.*Estimate.*Total).*Civilian employed population 16 years and over$", names(moes))] <- "Total"                                                                    
    names(moes)[grep("(.*Employee of private company workers.*Estimate|.*Estimate.*Employee of private company workers).*Civilian employed population 16 years and over$", names(moes))] <- "Private, Profit"                                             
    names(moes)[grep("(.*Self-employed in own incorporated business workers.*Estimate|.*Estimate.*Self-employed in own incorporated business workers).*Civilian employed population 16 years and over$", names(moes))] <- "Self-Employed, Incorporated"                              
    names(moes)[grep("(.*Private not-for-profit wage and salary workers.*Estimate|.*Estimate.*Private not-for-profit wage and salary workers).*Civilian employed population 16 years and over$", names(moes))] <- "Private, Not-for-profit"                                  
    names(moes)[grep("(.*Local, state, and federal government workers.*Estimate|.*Estimate.*Local, state, and federal government workers).*Civilian employed population 16 years and over$", names(moes))] <- "Government"                                    
    names(moes)[grep("(.*Self-employed in own not incorporated business workers and unpaid family workers.*Estimate|.*Estimate.*Self-employed in own not incorporated business workers and unpaid family workers).*Civilian employed population 16 years and over$", names(moes))] <- "Self-Employed, Not Incorporated"
    
    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Business Type",
        variable.factor = F,
        value.name = "Margins of Error",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Business Type`)
    setkey(moes, FIPS, Year, `Business Type`)

    business_type <- rbind(business_type, estimates[moes])
}

business_type <- business_type[business_type$FIPS != "0900100000",]

#Calculate total number for each business type
business_type_long <- gather(business_type, `Measure Type`, Value, 4:5, factor_key = FALSE)
business_type_long$Variable[business_type_long$`Measure Type` == "Percent"] <- "Employment"
business_type_long$Variable[business_type_long$`Measure Type` == "Margins of Error"] <- "Margins of Error"
business_type_long$`Measure Type` <- "Percent"
business_type_long$`Measure Type`[business_type_long$`Business Type` == "Total"] <- "Number"

#Merge in Counties by FIPS (filter out town data)
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp_URL <- datapkg_read(path = county_fips_dp_URL)
counties <- (county_fips_dp_URL$data[[1]])

business_type_fips <- merge(business_type_long, counties, by = "FIPS")

business_type_fips$Year <- paste(business_type_fips$Year-4, business_type_fips$Year, sep="-")

business_type_fips$`Business Type` <- factor(business_type_fips$`Business Type`, 
                                             levels = c("Total",
                                                        "Self-Employed, Incorporated",
                                                        "Self-Employed, Not Incorporated",
                                                        "Government",
                                                        "Private, Profit",
                                                        "Private, Not-for-profit"))

business_type_final <- business_type_fips %>% 
  select(County, FIPS, Year, `Business Type`, `Measure Type`, Variable, Value) %>% 
  arrange(County, Year, `Business Type`, Variable)
    
write.table(
    business_type_final,
    file.path("data", "self-employment-business-county-2018.csv"),
    sep = ",",
    row.names = F,
    col.names = T,
    na = "-6666" 
)
