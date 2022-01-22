#### First Gender Pay Gap Hack Script ####
library(dplyr)

# Open extract charity
extract_charity <- read.csv("../charity-commission-extract/data/extract_charity.csv", stringsAsFactors = FALSE)

# Open gender pay gap data
gpg <- read.csv("../gender-pay-gap/data/UK Gender Pay Gap Data - 2017 to 2018.csv", stringsAsFactors = FALSE)

#### COMBINE BY POSTCODE AND CHARITY NAME
# Combine by postcode
postcode <- sapply(gpg$Address, function(x) strsplit(x, ",")[[1]][length(strsplit(x, ",")[[1]])])
gpg$postcode <- gsub("\n", "", postcode)
extract_charity_pc <- left_join(extract_charity, gpg, by = "postcode")
extract_charity_pc <- extract_charity_pc[!is.na(extract_charity_pc$Address),]

# Filter by charity name
extract_charity_pc$name <- sapply(extract_charity_pc$name, function(x) toupper(x))
extract_charity_pc$EmployerName <- sapply(extract_charity_pc$EmployerName, function(x) toupper(x))
ind1 <- na.omit(match(extract_charity_pc$name, extract_charity_pc$EmployerName))
ind2 <- na.omit(match(extract_charity_pc$EmployerName, extract_charity_pc$name))
charity_postcode <- extract_charity_pc[unique(ind1, ind2),]

#### COMBINE BY COMPANY NUMBER 
# Open extract charity
extract_main_charity <- read.csv("../charity-commission-extract/data/extract_main_charity.csv", stringsAsFactors = FALSE)
extract_main_charity$regno <- as.character(extract_main_charity$regno)
extract_partb <- read.csv("../charity-commission-extract/data/extract_partb.csv", stringsAsFactors = FALSE)

# Combine by charity number to get company number
extract_charity_cn <- left_join(extract_charity, extract_main_charity, by = "regno")
# Remove rows with no company number
extract_charity_cn <- extract_charity_cn[!is.na(extract_charity_cn$coyno),]
extract_charity_cn <- extract_charity_cn[!(extract_charity_cn$coyno == ""),]

# Combine by company number
col_names <- names(extract_charity_cn)
col_names[which(col_names == "coyno")] <- "CompanyNumber"
names(extract_charity_cn) <- col_names
charity_comp <- left_join(extract_charity_cn, gpg, by = "CompanyNumber")
charity_comp <- charity_comp[!is.na(charity_comp$Address),]

# Rbind charity by postcode and charity by company number
comb_names <- names(charity_postcode)
charity_comp2 <- charity_comp[,names(charity_comp) %in% comb_names]
comb_names2 <- names(charity_comp2)
charity_postcode2 <- charity[,names(charity_postcode) %in% comb_names2]
charity <- rbind(charity_postcode2, charity_comp2)

# Save combined data
write.csv(charity, file = "../output_data/gpg_charity_2.csv", row.names = FALSE)
