# ********  NaiveBayes ************

install.packages('mlbench')
library('e1071')
library('mlbench')
# installing and loading catools package
install.packages("caTools")
library(caTools)
library(ggplot2) 
library(e1071)
# Library for spliting dataset
library(dplyr)


# Import Data from CSV file
cruise_data <- read.csv(file = 'Downloads/Cruise Director Analysis.csv')
print(cruise_data)

# Drop unnecessary ID column 
cruise_data = subset(cruise_data, select = -c(ID) )
print(cruise_data)
dim(cruise_data)

# Plot graph
ggplot(cruise_data, aes(x=reorder(PurchasedPackage, PurchasedPackage, function(x)-length(x)))) +
  geom_bar(fill='steelblue', width = 0.40) +
  labs(x='Is Package Purchased')

#cruise_data <- na.omit(cruise_data)

#cruise_data <- rename(cruise_data, c("Port of Embarkation"="PortOfEmbarkation", "Economic Class"="EconomicClass", "No of Siblings or Spouses on Board"="NoOfSiblingsOrSpousesOnBoard" ,
#                                    "NoOfParentsOrChildrenOnBoard"="three", "PassengerFare"="three", "ChildFare?"="ChildFare")) 

cruise_data <- na.omit(cruise_data)
dim(cruise_data)


cruise_data[cruise_data$Age <= 5, "age_group"] <- "0-5"
cruise_data[cruise_data$Age > 5 & cruise_data$Age <= 10, "age_group"] <- "6-10"
cruise_data[cruise_data$Age > 10 & cruise_data$Age <= 15, "age_group"] <- "11-15"
cruise_data[cruise_data$Age > 15 & cruise_data$Age <= 20, "age_group"] <- "16-20"
cruise_data[cruise_data$Age > 20 & cruise_data$Age <= 25, "age_group"] <- "21-25"
cruise_data[cruise_data$Age > 25 & cruise_data$Age <= 30, "age_group"] <- "26-30"
cruise_data[cruise_data$Age > 30 & cruise_data$Age <= 35, "age_group"] <- "31-35"
cruise_data[cruise_data$Age > 35 & cruise_data$Age <= 40, "age_group"] <- "36-40"
cruise_data[cruise_data$Age > 40 & cruise_data$Age <= 45, "age_group"] <- "41-45"
cruise_data[cruise_data$Age > 45 & cruise_data$Age <= 50, "age_group"] <- "46-50"
cruise_data[cruise_data$Age > 50 & cruise_data$Age <= 55, "age_group"] <- "51-55"
cruise_data[cruise_data$Age > 55 & cruise_data$Age <= 60, "age_group"] <- "56-60"
cruise_data[cruise_data$Age > 60, "age_group"] <- "> 60"


cruise_data = subset(cruise_data, select = -c(Age) )
# Splitting data into train and test dataset
ind = sample.split(Y = cruise_data$PurchasedPackage, SplitRatio = 0.8)

#subsetting into Train data
train_cruise_data = cruise_data[ind,]

#subsetting into Test data
test_cruise_data = cruise_data[!ind,]
dim(train_cruise_data)
dim(test_cruise_data)

# Print count Yes and No values of PurchasedPackage
table(cruise_data$PurchasedPackage)

table(train_cruise_data$PurchasedPackage)

table(test_cruise_data$PurchasedPackage)

# Fitting Naive Bayes Model
# to training dataset
NBclassfier <- naiveBayes(PurchasedPackage ~ ., data = train_cruise_data)
NBclassfier

# Predicting on test data
NBresult <- predict(NBclassfier, test_cruise_data)
NBresult

# Create a table to store : training and test from prediction result 
prop.table(table(NBresult))

# Installing Libraries required for evaluation of naive bayes 
library(lattice)
library(ggplot2)
install.packages('gmodels')
install.packages('caret')
library('caret')
library(gmodels)

# Confusion matrix to check accuracy
# Generate Statistical report and  Confusion Matrix using Caret library which will give us True Positive, 
#False Positive, True Negative, True Positive counts which will help to evaluate model performance

NBConfusion_matrix<-confusionMatrix(NBresult, as.factor(test_cruise_data$PurchasedPackage))

str(NBConfusion_matrix)
NBConfusion_matrix

tab_matrix<-table(NBresult,test_cruise_data$PurchasedPackage)
tab_matrix


# Plot the Confusion Matrix

test_cruise_data$PurchasedPackage <- NBresult$PurchasedPackage
ggplot(test_cruise_data, aes(PurchasedPackage, NBresult, color = PurchasedPackage)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs. Actual from Cruise Director Analysis dataset", 
       y="Predicted", 
       x="Actual")


passengers_with_dependents_gte_4_perchased = dim(filter(cruise_data, cruise_data$No.of.Parents.or.Children.on.Board >= 4 
                                                        & cruise_data$ChildFare. == "N" & cruise_data$PurchasedPackage == "Yes"))[1]
total_passengers_with_dependents_gte_4 = dim(filter(cruise_data, cruise_data$No.of.Parents.or.Children.on.Board >= 4 
                                                    & cruise_data$ChildFare. == "N"))[1]
(passengers_with_dependents_gte_4_perchased/total_passengers_with_dependents_gte_4) *100

passengers_with_dependents_lt_4_perchased = dim(filter(cruise_data, cruise_data$No.of.Parents.or.Children.on.Board < 4 
                                                       & cruise_data$ChildFare. == "Y" & cruise_data$PurchasedPackage == "Yes"))[1]
total_passengers_with_dependents_lt_4 = dim(filter(cruise_data, cruise_data$No.of.Parents.or.Children.on.Board < 4  
                                                   & cruise_data$ChildFare. == "Y"))[1]
(passengers_with_dependents_lt_4_perchased/total_passengers_with_dependents_lt_4) *100



# percentage of female having age less than 18 and having upper or middle class who purchased the packages
female_passengers_age_gte_50_perchased = dim(filter(cruise_data, cruise_data$Sex == "Female" & cruise_data$Age >= 50 
                                                        & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle") 
                                                    & cruise_data$PurchasedPackage == "Yes"))[1]
total_female_passengers_age_gte_50 = dim(filter(cruise_data, cruise_data$Sex == "Female" & cruise_data$Age >= 50 
                                                    & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle")))[1]
(female_passengers_age_gte_50_perchased/total_female_passengers_age_gte_50) *100

# percentage of female which in Upper & Middle class having age greater than 50 who purchased the packages considering all females
female_passengers_age_gte_50_perchased = dim(filter(cruise_data, cruise_data$Sex == "Female" & cruise_data$Age >= 50  
                                                    & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle") 
                                                    & cruise_data$PurchasedPackage == "Yes"))[1]
total_female_passengers = dim(filter(cruise_data, cruise_data$Sex == "Female"
                                                & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle")))[1]
(female_passengers_age_gte_50_perchased/total_female_passengers) *100

# percentage of female having age less than 18 and having upper or middle class who purchased the packages
female_passengers_age_lte_17_perchased = dim(filter(cruise_data, cruise_data$Sex == "Female" & cruise_data$Age <= 17 
                                                    & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle") 
                                                    & cruise_data$PurchasedPackage == "Yes"))[1]
total_female_passengers_age_lte_17 = dim(filter(cruise_data, cruise_data$Sex == "Female" & cruise_data$Age <= 17 
                                                & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle")))[1]
(female_passengers_age_lte_17_perchased/total_female_passengers_age_lte_17) *100

# percentage of female which in Upper & Middle class having age less than 17 who purchased the packages considering all females
female_passengers_age_lte_17_perchased = dim(filter(cruise_data, cruise_data$Sex == "Female" & cruise_data$Age <= 17 
                                                    & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle") 
                                                    & cruise_data$PurchasedPackage == "Yes"))[1]
total_female_passengers = dim(filter(cruise_data, cruise_data$Sex == "Female"
                                     & (cruise_data$Economic.Class == "Upper" | cruise_data$Economic.Class == "Middle")))[1]
(female_passengers_age_lte_17_perchased/total_female_passengers) *100
