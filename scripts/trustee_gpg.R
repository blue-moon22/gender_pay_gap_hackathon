library(dplyr)
library(gender)
library(ggplot2)
library(broom)

# Read charity gpg 2
# charities <- read.csv("../output_data/gpg_charity_2.csv", stringsAsFactors = FALSE)
charities <- read.csv("../output_data/gender-pay-gap-with-charity-no.csv", stringsAsFactors = FALSE)

col_names <- names(charities)
col_names[col_names == "CharityNumber"] <- "regno"
names(charities) <- col_names
charities <- charities[!is.na(charities$regno),]

# Read trustees
trustees <- read.csv("../charity-commission-extract/data/extract_trustee.csv", stringsAsFactors = FALSE)

charities$regno <- as.character(charities$regno)
trustees$regno <- as.character(trustees$regno)

trustees <- left_join(trustees, charities, by = "regno")
trustees <- trustees[!is.na(trustees$EmployerName),]

# Convert trusties to lowercase
trustees$trustee <- sapply(trustees$trustee, function(x) tolower(x))

# Get gender
gender <- rep(NA, dim(trustees)[1])
gender[grep("mr ", trustees$trustee)] <- "male"
gender[grep("lord ", trustees$trustee)] <- "male"
gender[grep("lord ", trustees$trustee)] <- "male"
gender[grep("rev ", trustees$trustee)] <- "male"
gender[grep("colonel ", trustees$trustee)] <- "male" # Assumption I think?
gender[grep("lieutenant ", trustees$trustee)] <- "male" # Assumption I think?
gender[grep("viscount ", trustees$trustee)] <- "male" # Assumption I think?
gender[grep("commodore ", trustees$trustee)] <- "male" # Assumption I think?
gender[grep("captain ", trustees$trustee)] <- "male" # Assumption I think?
gender[grep("sir ", trustees$trustee)] <- "male" # Assumption I think?
gender[grep("mrs ", trustees$trustee)] <- "female"
gender[grep("mrs.", trustees$trustee)] <- "female"
gender[grep("miss ", trustees$trustee)] <- "female"
gender[grep("dame ", trustees$trustee)] <- "female"
gender[grep("ms ", trustees$trustee)] <- "female"
gender[grep("lady ", trustees$trustee)] <- "female"

uniq_names <- unique(trustees$trustee[is.na(gender)])
uniq_names

# Doctors
dr <- uniq_names[grep("dr ", uniq_names)]
dr_names <- unique(sapply(dr, function(x) strsplit(x, " ")[[1]][2]))
dr_gender <- gender(c(dr_names), years = 1980, method = "ssa")$gender
dr_names <- gender(c(dr_names), years = 1980, method = "ssa")$name
for(i in 1:length(dr_names)){
  gender[grep(dr_names[i], trustees$trustee)] <- dr_gender[i]
}
unique(trustees$trustee[is.na(gender)])

# Professor
prof <- uniq_names[c(grep("professor ", uniq_names), grep("prof ", uniq_names))]
prof_names <- unique(sapply(prof, function(x) strsplit(x, " ")[[1]][2]))
prof_gender <- gender(c(prof_names), years = 1980, method = "ssa")$gender
prof_names <- gender(c(prof_names), years = 1980, method = "ssa")$name
for(i in 1:length(prof_names)){
  gender[grep(prof_names[i], trustees$trustee)] <- prof_gender[i]
}
unique(trustees$trustee[is.na(gender)])

### For the rest of the names
unique(trustees$trustee[is.na(gender)])
uniq_names2 <- unique(trustees$trustee[is.na(gender)])
names2 <- unique(sapply(uniq_names2, function(x) strsplit(x, " ")[[1]][2]))
names_gender <- gender(c(names2), years = 1980, method = "ssa")$gender
names2 <- gender(c(names2), years = 1980, method = "ssa")$name
for(i in 1:length(names2)){
  gender[grep(names2[i], trustees$trustee)] <- names_gender[i]
}

# Assign gender
trustees$gender <- gender
# Remove if na
trustees <- trustees[!is.na(trustees$gender),]

# Get the size of the trustee board
trustees_size <- trustees %>% 
  group_by(regno, DiffMeanHourlyPercent, DiffMedianHourlyPercent,
           DiffMeanBonusPercent, DiffMedianBonusPercent, 
           MaleLowerQuartile, FemaleLowerQuartile, MaleLowerMiddleQuartile,
           FemaleLowerMiddleQuartile, MaleUpperMiddleQuartile, 
           FemaleUpperMiddleQuartile, MaleTopQuartile,
           FemaleTopQuartile, EmployerSize) %>%
  summarise(board_size = n()) %>%
  data.frame()


# Get the ratio for each charity
count_gender <- trustees %>% 
  group_by(regno, DiffMeanHourlyPercent, DiffMedianHourlyPercent,
           DiffMeanBonusPercent, DiffMedianBonusPercent, 
           MaleLowerQuartile, FemaleLowerQuartile, MaleLowerMiddleQuartile,
           FemaleLowerMiddleQuartile, MaleUpperMiddleQuartile, 
           FemaleUpperMiddleQuartile, MaleTopQuartile,
           FemaleTopQuartile, EmployerSize, gender) %>%
  summarise(n = n()) %>%
  data.frame()
uniq_regno <- unique(count_gender$regno)

# Get unique pay gap for ratio
pay_gap <- rep(NA, length(uniq_regno))
for(i in 1:length(uniq_regno)){
  count_tmp <- count_gender[count_gender$regno == uniq_regno[i],]
  pay_gap[i] <- count_tmp$DiffMeanHourlyPercent[1]
}
trustees_size$pay_gap <- pay_gap

# Get proportion of males
ratio <- rep(NA, length(uniq_regno))
for(i in 1:length(uniq_regno)){
  count_tmp <- count_gender[count_gender$regno == uniq_regno[i],]
  male <- count_tmp$n[count_tmp$gender == "male"]
  total <- sum(count_tmp$n)
  ratio[i] <- male/total *100
}  
trustees_size$ratio <- ratio

# Use only > 3 board size
trustees_size <- trustees_size[trustees_size$board_size > 10,]

# Linear regression for mean hourly gender gap
linearMod <- lm(DiffMeanHourlyPercent ~ ratio, data = trustees_size)
summary(linearMod)

# Linear regression for median hourly gender gap
linearMod_med <- lm(DiffMedianHourlyPercent ~ ratio, data = trustees_size)
summary(linearMod_med)

# Linear regression of board size
linearMod_boardsize <- lm(ratio ~ board_size + EmployerSize, data = trustees_size)
summary(linearMod_boardsize)
# Board size no employer size isnt significantly linearly correlated with ratio nor pay gap

# Plot mean pay gap
predicted_df <- data.frame(pay_gap_pred = predict(linearMod, trustees_size), ratio=trustees_size$ratio)
tiff("../images/mean_pay_gap.tiff", height = 800, width = 1000, res = 150)
ggplot(trustees_size, aes(ratio, DiffMeanHourlyPercent)) +
  geom_point(aes(size = factor(board_size))) +
  geom_point(aes(colour = factor(EmployerSize))) +
  ylab("Pay Gap\n(Mean hourly diff.)") +
  xlab("Percentage Male") +
  scale_size_discrete("Board Size") +
  scale_color_discrete("Employer Size")
  # geom_line(color='red',data = predicted_df, aes(x=ratio, y=pay_gap_pred))
dev.off()

# Plot median pay gap
predicted_med_df <- data.frame(pay_gap_pred = predict(linearMod_med, trustees_size), ratio=trustees_size$ratio)
tiff("../images/median_pay_gap.tiff", height = 800, width = 1000, res = 150)
ggplot(trustees_size, aes(ratio, DiffMedianHourlyPercent)) +
  geom_point(aes(size = factor(board_size))) +
  geom_point(aes(colour = factor(EmployerSize))) +
  ylab("Pay Gap\n(Median hourly diff.)") +
  xlab("Percentage Male") +
  scale_size_discrete("Board Size") +
  scale_color_discrete("Employer Size")
  # geom_line(color='red',data = predicted_med_df, aes(x=ratio, y=pay_gap_pred))
dev.off()
