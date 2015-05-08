setwd("C:/Users/chjh/Dropbox/projects/2015head")
source("chjh functions.r")

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
###################################
options(scipen = 5)

pdf('Fig1.pdf')
par(mar=c(4, 4, 0, 0))
hist(pdatHEAD$p.value,
     xlim = c(0, .05),
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
           bin1 = NA,
           bin2 = NA,
           pval = NA)

i <- 1
# All disciplines
resultsCHJH <- table(cut(pdatHEAD.results$p.value, breaks = 40))

bin1 <- resultsCHJH[which(names(resultsCHJH) == "(0.0387,0.04]")]
bin2 <- resultsCHJH[which(names(resultsCHJH) == "(0.0488,0.0501]")]

x <- binom.test(x = bin2, n = (bin1 + bin2), alternative = "greater")

resultsDF_CHJH[i,2] <- bin2
resultsDF_CHJH[i,3] <- bin1
resultsDF_CHJH[i,4] <- round(x$p.value, 3)

i <- i + 1

# Per discpline
for(d in resultsDF_CHJH$discipline[-1]){
  resultsCHJH <- table(cut(pdatHEAD.results$p.value[as.character(pdatHEAD.results$Category) == d], breaks = 40))
  
  bin1 <- resultsCHJH[which(names(resultsCHJH) == "(0.0387,0.04]")]
  bin2 <- resultsCHJH[which(names(resultsCHJH) == "(0.0488,0.0501]")]
  
  x <- binom.test(x = bin2, n = (bin1 + bin2), alternative = "greater")
  
  resultsDF_CHJH[i,2] <- bin2
  resultsDF_CHJH[i,3] <- bin1
  resultsDF_CHJH[i,4] <- round(x$p.value, 3)
  
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
                               bin1 = NA,
                               bin2 = NA,
                               pval = NA)

i <- 1
# All disciplines
abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value, breaks = 40))

bin1 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0387,0.04]")]
bin2 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0488,0.0501]")]

x <- binom.test(x = bin2, n = (bin1 + bin2), alternative = "greater")

abstractsDF_CHJH[i,2] <- bin2
abstractsDF_CHJH[i,3] <- bin1
abstractsDF_CHJH[i,4] <- round(x$p.value, 3)

i <- i + 1

# Per discpline
for(d in abstractsDF_CHJH$discipline[-1]){
  abstractsCHJH <- table(cut(pdatHEAD.abstracts$p.value[as.character(pdatHEAD.abstracts$Category) == d], breaks = 40))
  
  bin1 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0387,0.04]")]
  bin2 <- abstractsCHJH[which(names(abstractsCHJH) == "(0.0488,0.0501]")]
  
  x <- binom.test(x = bin2, n = (bin1 + bin2), alternative = "greater")
  
  abstractsDF_CHJH[i,2] <- ifelse(is.na(bin2), 0, bin2)
  abstractsDF_CHJH[i,3] <- ifelse(is.na(bin1), 0, bin1)
  abstractsDF_CHJH[i,4] <- round(x$p.value, 3)
  
  i <- i + 1
}

write.csv(resultsDF_CHJH, 'results/strong re results_CHJH.csv')
write.csv(abstractsDF_CHJH, 'results/strong re abstracts_CHJH.csv')

###################################
# End strong reanalysis 
###################################