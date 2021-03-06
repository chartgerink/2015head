setwd(choose.dir())
source("chjh functions.r")

library(BayesFactor)

pdat <- read.csv("FILES_FOR_DRYAD/1. TEXT_MINING/raw_data/p.values.csv", row.names=1)
journal.categories <- read.csv("FILES_FOR_DRYAD/1. TEXT_MINING/raw_data/journal.categories.csv", row.names=1)

# journal.categories$journal.name <- journal.categories$Abbreviation
# pdat <- merge(pdat, journal.categories,by="journal.name")

###################################
# Start selection 
###################################
# Head et al selection 1
# Get rid of papers that did not yield any p values
pdatHEAD <- pdat[!is.na(pdat$p.value), ]

# Head et al selection 2
# Removed due to arguments presented in paper
# Below I just present the count of results and papers
x <- pdatHEAD[which(!pdatHEAD$num.dois==1),]
# Results eliminated
dim(x)[1]
# Number of unique sources eliminated
length(unique(x$file.name))
rm(x)

# Number of results and unique sources in the original dataset
dim(pdatHEAD)[1]
length(unique(pdatHEAD$file.name))

# Head et al selection 3
# "only keep papers for which we have >0 results sections (e.g. reviews and commentaries often have 0 results sections)"
pdatHEAD <- pdatHEAD[which(pdatHEAD$num.results.sections > 0),]

# Head et al selection 4
# "some papers have (legitimately, I've checked) zero authors. Remove these."
pdatHEAD <- pdatHEAD[which(pdatHEAD$num.authors>0),]

# Head et al selection 5
# some journals publish large 'supplements' that contain conference many short conference abstracts
# (with results sections) in one file. Remove these
pdatHEAD <- pdatHEAD[-c(grep("(Suppl)", pdatHEAD$file.name)),]


# we have a few records in this dataset in which the journal.name wasn't extracted properly, we fix that here
pdatHEAD$journal.name <- as.character(pdatHEAD$journal.name)
fixed.names <- as.character(pdatHEAD$folder.name[which(is.na(pdatHEAD$journal.name))])
fixed.names <- gsub("_", " ", fixed.names)
pdatHEAD$journal.name[which(is.na(pdatHEAD$journal.name))] <- fixed.names
pdatHEAD$journal.name <- as.factor(pdatHEAD$journal.name)

# now we add in the FoR categories 
journal.categories$journal.name <- journal.categories$Abbreviation
pdatHEAD <- merge(pdatHEAD, journal.categories,by="journal.name")

# Head et al selection 6
# Adjusted to <=, from <
pdatHEAD <- pdatHEAD[pdatHEAD$p.value <= 0.05, ]

# Head et al selection 7
pdatHEAD <- pdatHEAD[pdatHEAD$operator == "=", ]
###################################
# End selection 
###################################

pdatHEAD.results <- pdatHEAD[pdatHEAD$section == "results", ]
pdatHEAD.abstracts <- pdatHEAD[pdatHEAD$section == "abstract", ]
# Remove excess factor levels from the datasets 
# (there are plenty of empty ones, since we deleted some rows, 
# and the empty levels would otherwise cause trouble later on)
pdatHEADresults <- trim.levels(pdatHEAD.results)
pdatHEADabstracts <- trim.levels(pdatHEAD.abstracts)

###################################
# Start sensitivity reanalysis 
# NOT IN PAPER AFTER REVISIONS
###################################
# Only adjustment from original code is the renaming of objects
# and adjusted the functions to include p <= .05
# Code is thus almost literally from Head et al.
###################################

##### Analysis on the entire dataset (i.e. not split by FoR category)
reps <- 1000
results.bias.test <- bootstrap.binomial.bias.test(pdatHEADresults, reps)
abstract.bias.test <- bootstrap.binomial.bias.test(pdatHEADabstracts, reps)

write.csv(results.bias.test, file="results/chjh.results.combinedata.csv")
write.csv(abstract.bias.test, file="results/chjh.results.combinedata.abstracts.csv")


###### Analyses split by FoR category
results.FoR.test <- bootstrap.FoR.test(pdatHEADresults, reps)
abstract.FoR.test <- bootstrap.FoR.test(pdatHEADabstracts, reps)

write.csv(results.FoR.test, file="results/chjh.results.by.category.csv")
write.csv(abstract.FoR.test, file="results/chjh.results.by.category.abstracts.csv")


# Some additions from CHJH (CI Calculation)
# CI from Head for p-hacking evidence in results section
binom.test(x = 3120, n = (3120 + 2590), alternative = "greater")
# CI Hartgerink for p-hacking evidence in results section
binom.test(x = 7298, n = (7298 + 2692), alternative = "greater")

###################################
# End sensitivity reanalysis 
###################################

###################################
# Start strong reanalysis 
# REANALYSIS PRESENTED IN PAPER
###################################
options(scipen = 5)

sel <- pdatHEAD$p.value > .04 & pdatHEAD$p.value < .05 & !pdatHEAD$p.value == .045

pdf('results/Fig1.pdf', width = 7.5, height = 8.75)
par(mar=c(4, 4, 0, 0), mfrow = c(2, 1))
hist(pdatHEAD$p.value[sel],
     xlim = c(.04, .05),
#      ylim = c(0, 105000),
     breaks = 2,
     main = "",
     xlab = "P-value",
     ylab="Frequency",
     col='white',
     cex.axis=.8,
     las=1,
     cex.lab=.8)

hist(pdatHEAD$p.value,
     xlim = c(0, .05),
     ylim = c(0, 105000),
     breaks = 40,
     main = "",
     xlab = "P-value",
     ylab="Frequency",
     col='white',
     cex.axis=.8,
     las=1,
     cex.lab=.8)
dev.off()

# Number of p-values pre-2010.
x <- table(pdatHEAD$year)
sum(x[1:(which(names(x) == "2010")-1)])

# Creating the caliper test results for the new bin selection
## RESULTS SECTION ONLY
# Create the dataframe to put in
resultsDF_CHJH <- data.frame(discipline = c("All",
                                            "Pharmacology And Pharmaceutical Sciences",
                                            "Medical And Health Sciences",
                                            "Chemistry and geology",
                                            "Neurosciences",
                                            "Multidisciplinary",
                                            "Zoology",
                                            "Biochemistry And Cell Biology",
                                            "Computer sciences",
                                            "Biomedical Engineering",
                                            "Psychology and sociology",
                                            "Animal, veterinary and agricultural science",
                                            "Complementary And Alternative Medicine",
                                            "Public Health And Health Services",
                                            "Informatics, mathematics and physics",
                                            "Education",
                                            "Microbiology",
                                            "Ecology, evolution and earth sciences",
                                            "Biological Sciences",
                                            "Immunology",
                                            "Genetics",
                                            "Physiology",
                                            "Plant Biology",
                                            "Geography, business and economics",
                                            "Dentistry",
                                            "Nutrition And Dietetics",
                                            "Other"),
                             bin1.00125 = NA,
                             bin2.00125 = NA,
                             prop.00125 = NA,
                             pval.00125 = NA,
                             bf10.00125 = NA,
                             bin1.005 = NA,
                             bin2.005 = NA,
                             prop.005 = NA,
                             pval.005 = NA,
                             bf10.005 = NA,
                             bin1.01 = NA,
                             bin2.01 = NA,
                             prop.01 = NA,
                             pval.01 = NA,
                             bf10.01 = NA)

i <- 1
# All disciplines
resultsCHJH <- table(cut(pdatHEAD.results$p.value, breaks = 40))

bin1.00125 <- resultsCHJH[which(names(resultsCHJH) == "(0.0387,0.04]")]
bin2.00125 <- resultsCHJH[which(names(resultsCHJH) == "(0.0488,0.0501]")]

x <- binom.test(x = bin2.00125, n = (bin1.00125 + bin2.00125), alternative = "greater")
x$bf10 <- as.data.frame(proportionBF(y = bin2.00125, N = (bin1.00125 + bin2.00125), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]


resultsDF_CHJH[i,2] <- bin1.00125
resultsDF_CHJH[i,3] <- bin2.00125
resultsDF_CHJH[i,4] <- x$estimate
resultsDF_CHJH[i,5] <- round(x$p.value, 3)
resultsDF_CHJH[i,6] <- x$bf10

resultsCHJH <- table(cut(pdatHEAD.results$p.value, breaks = 10))

bin1.005 <- resultsCHJH[which(names(resultsCHJH) == "(0.035,0.04]")]
bin2.005 <- resultsCHJH[which(names(resultsCHJH) == "(0.045,0.0501]")]

x <- binom.test(x = bin2.005, n = (bin1.005 + bin2.005), alternative = "greater")
x$bf10 <- as.data.frame(proportionBF(y = bin2.005, N = (bin1.005 + bin2.005), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]


resultsDF_CHJH[i,7] <- bin1.005
resultsDF_CHJH[i,8] <- bin2.005
resultsDF_CHJH[i,9] <- x$estimate
resultsDF_CHJH[i,10] <- round(x$p.value, 3)
resultsDF_CHJH[i,11] <- x$bf10

resultsCHJH <- table(cut(pdatHEAD.results$p.value, breaks = 5))

bin1.01 <- resultsCHJH[which(names(resultsCHJH) == "(0.03,0.04]")]
bin2.01 <- resultsCHJH[which(names(resultsCHJH) == "(0.04,0.0501]")]

x <- binom.test(x = bin2.01, n = (bin1.01 + bin2.01), alternative = "greater")
x$bf10 <- as.data.frame(proportionBF(y = bin2.01, N = (bin1.01 + bin2.01), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]


resultsDF_CHJH[i,12] <- bin1.01
resultsDF_CHJH[i,13] <- bin2.01
resultsDF_CHJH[i,14] <- x$estimate
resultsDF_CHJH[i,15] <- round(x$p.value, 3)
resultsDF_CHJH[i,16] <- x$bf10

i <- i + 1

# Per discpline
for(d in resultsDF_CHJH$discipline[-1]){
  resultsCHJH <- table(cut(pdatHEAD.results$p.value[as.character(pdatHEAD.results$Category) == d], breaks = 40))
  
  bin1.00125 <- resultsCHJH[which(names(resultsCHJH) == "(0.0387,0.04]")]
  bin2.00125 <- resultsCHJH[which(names(resultsCHJH) == "(0.0488,0.0501]")]
  
  x <- binom.test(x = bin2.00125, n = (bin1.00125 + bin2.00125), alternative = "greater")
  x$bf10 <- as.data.frame(proportionBF(y = bin2.00125, N = (bin1.00125 + bin2.00125), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]
  
  
  resultsDF_CHJH[i,2] <- bin1.00125
  resultsDF_CHJH[i,3] <- bin2.00125
  resultsDF_CHJH[i,4] <- x$estimate
  resultsDF_CHJH[i,5] <- round(x$p.value, 3)
  resultsDF_CHJH[i,6] <- x$bf10
  
  resultsCHJH <- table(cut(pdatHEAD.results$p.value[as.character(pdatHEAD.results$Category) == d], breaks = 10))
  
  bin1.005 <- resultsCHJH[which(names(resultsCHJH) == "(0.035,0.04]")]
  bin2.005 <- resultsCHJH[which(names(resultsCHJH) == "(0.045,0.0501]")]
  
  x <- binom.test(x = bin2.005, n = (bin1.005 + bin2.005), alternative = "greater")
  x$bf10 <- as.data.frame(proportionBF(y = bin2.005, N = (bin1.005 + bin2.005), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]
  
  
  resultsDF_CHJH[i,7] <- bin1.005
  resultsDF_CHJH[i,8] <- bin2.005
  resultsDF_CHJH[i,9] <- x$estimate
  resultsDF_CHJH[i,10] <- round(x$p.value, 3)
  resultsDF_CHJH[i,11] <- x$bf10
  
  resultsCHJH <- table(cut(pdatHEAD.results$p.value[as.character(pdatHEAD.results$Category) == d], breaks = 5))
  
  bin1.01 <- resultsCHJH[which(names(resultsCHJH) == "(0.03,0.04]")]
  bin2.01 <- resultsCHJH[which(names(resultsCHJH) == "(0.04,0.0501]")]
  
  x <- binom.test(x = bin2.01, n = (bin1.01 + bin2.01), alternative = "greater")
  x$bf10 <- as.data.frame(proportionBF(y = bin2.01, N = (bin1.01 + bin2.01), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]
  
  
  resultsDF_CHJH[i,12] <- bin1.01
  resultsDF_CHJH[i,13] <- bin2.01
  resultsDF_CHJH[i,14] <- x$estimate
  resultsDF_CHJH[i,15] <- round(x$p.value, 3)
  resultsDF_CHJH[i,16] <- x$bf10
  
  i <- i + 1
}

## ABSTRACTS SECTION ONLY
# Create the dataframe to put in
abstractsDF_CHJH <- data.frame(discipline = c("All",
                                              "Pharmacology And Pharmaceutical Sciences",
                                              "Medical And Health Sciences",
                                              "Chemistry and geology",
                                              "Neurosciences",
                                              "Multidisciplinary",
                                              "Zoology",
                                              "Biochemistry And Cell Biology",
                                              "Computer sciences",
                                              "Biomedical Engineering",
                                              "Psychology and sociology",
                                              "Animal, veterinary and agricultural science",
                                              "Complementary And Alternative Medicine",
                                              "Public Health And Health Services",
                                              "Informatics, mathematics and physics",
                                              "Education",
                                              "Microbiology",
                                              "Ecology, evolution and earth sciences",
                                              "Biological Sciences",
                                              "Immunology",
                                              "Genetics",
                                              "Physiology",
                                              "Plant Biology",
                                              "Geography, business and economics",
                                              "Dentistry",
                                              "Nutrition And Dietetics",
                                              "Other"),
                               bin1.00125 = NA,
                               bin2.00125 = NA,
                               prop.00125 = NA,
                               pval.00125 = NA,
                               bf10.00125 = NA,
                               bin1.005 = NA,
                               bin2.005 = NA,
                               prop.005 = NA,
                               pval.005 = NA,
                               bf10.005 = NA,
                               bin1.01 = NA,
                               bin2.01 = NA,
                               prop.01 = NA,
                               pval.01 = NA,
                               bf10.01 = NA)

i <- 1
# All disciplines
abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value, breaks = 40))

bin1.00125 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0387,0.04]")]
bin2.00125 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0488,0.0501]")]

x <- binom.test(x = bin2.00125, n = (bin1.00125 + bin2.00125), alternative = "greater")
x$bf10 <- as.data.frame(proportionBF(y = bin2.00125, N = (bin1.00125 + bin2.00125), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]


abstractsDF_CHJH[i,2] <- bin1.00125
abstractsDF_CHJH[i,3] <- bin2.00125
abstractsDF_CHJH[i,4] <- x$estimate
abstractsDF_CHJH[i,5] <- round(x$p.value, 3)
abstractsDF_CHJH[i,6] <- x$bf10

abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value, breaks = 10))

bin1.005 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.035,0.04]")]
bin2.005 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.045,0.0501]")]

x <- binom.test(x = bin2.005, n = (bin1.005 + bin2.005), alternative = "greater")
x$bf10 <- as.data.frame(proportionBF(y = bin2.005, N = (bin1.005 + bin2.005), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]


abstractsDF_CHJH[i,7] <- bin1.005
abstractsDF_CHJH[i,8] <- bin2.005
abstractsDF_CHJH[i,9] <- x$estimate
abstractsDF_CHJH[i,10] <- round(x$p.value, 3)
abstractsDF_CHJH[i,11] <- x$bf10

abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value, breaks = 5))

bin1.01 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.03,0.04]")]
bin2.01 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.04,0.0501]")]

x <- binom.test(x = bin2.01, n = (bin1.01 + bin2.01), alternative = "greater")
x$bf10 <- as.data.frame(proportionBF(y = bin2.01, N = (bin1.01 + bin2.01), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]


abstractsDF_CHJH[i,12] <- bin1.01
abstractsDF_CHJH[i,13] <- bin2.01
abstractsDF_CHJH[i,14] <- x$estimate
abstractsDF_CHJH[i,15] <- round(x$p.value, 3)
abstractsDF_CHJH[i,16] <- x$bf10

i <- i + 1

# Per discpline
for(d in abstractsDF_CHJH$discipline[-1]){
  abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value[as.character(pdatHEAD.abstracts$Category) == d], breaks = 40))
  
  bin1.00125 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0387,0.04]")]
  bin2.00125 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0488,0.0501]")]
  
  x <- binom.test(x = bin2.00125, n = (bin1.00125 + bin2.00125), alternative = "greater")
  x$bf10 <- as.data.frame(proportionBF(y = bin2.00125, N = (bin1.00125 + bin2.00125), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]
  
  
  abstractsDF_CHJH[i,2] <- bin1.00125
  abstractsDF_CHJH[i,3] <- bin2.00125
  abstractsDF_CHJH[i,4] <- x$estimate
  abstractsDF_CHJH[i,5] <- round(x$p.value, 3)
  abstractsDF_CHJH[i,6] <- x$bf10
  
  abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value[as.character(pdatHEAD.abstracts$Category) == d], breaks = 10))
  
  bin1.005 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.035,0.04]")]
  bin2.005 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.045,0.0501]")]
  
  x <- binom.test(x = bin2.005, n = (bin1.005 + bin2.005), alternative = "greater")
  x$bf10 <- as.data.frame(proportionBF(y = bin2.005, N = (bin1.005 + bin2.005), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]
  
  
  abstractsDF_CHJH[i,7] <- bin1.005
  abstractsDF_CHJH[i,8] <- bin2.005
  abstractsDF_CHJH[i,9] <- x$estimate
  abstractsDF_CHJH[i,10] <- round(x$p.value, 3)
  abstractsDF_CHJH[i,11] <- x$bf10
  
  abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value[as.character(pdatHEAD.abstracts$Category) == d], breaks = 5))
  
  bin1.01 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.03,0.04]")]
  bin2.01 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.04,0.0501]")]
  
  x <- binom.test(x = bin2.01, n = (bin1.01 + bin2.01), alternative = "greater")
  x$bf10 <- as.data.frame(proportionBF(y = bin2.01, N = (bin1.01 + bin2.01), p = .5, nullInterval = c(0, .5), rscale = 1))$bf[2]
  
  
  abstractsDF_CHJH[i,12] <- bin1.01
  abstractsDF_CHJH[i,13] <- bin2.01
  abstractsDF_CHJH[i,14] <- x$estimate
  abstractsDF_CHJH[i,15] <- round(x$p.value, 3)
  abstractsDF_CHJH[i,16] <- x$bf10
  
  i <- i + 1
}

write.table(resultsDF_CHJH, 'results/reanalysis results section_CHJH.csv', sep = ";", dec = ".", row.names = FALSE)
write.table(abstractsDF_CHJH, 'results/reanalysis abstracts section_CHJH.csv', sep = ";", dec = ".", row.names = FALSE)

###################################
# End strong reanalysis 
###################################

bin2 <- sum(pdatHEAD.results$p.value > .045 & pdatHEAD.results$p.value <= .05)
bin1 <- sum(pdatHEAD.results$p.value >= .04 & pdatHEAD.results$p.value < .045)

binom.test(x = bin2, n = (bin1 + bin2), alternative = "greater")
proportionBF(y = bin2, N = (bin1 + bin2), p = .5, nullInterval = c(0, .5), rscale = 1)
