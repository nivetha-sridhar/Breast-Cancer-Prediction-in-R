my_packages <- c("tidyverse", "xlsx", "skimr","ggplot2","caTools")
lapply(my_packages, require, character.only = TRUE)

#Data Loading and Pre-Processing

#Malignant -> 0
#Benign    -> 1

breast_cancer_dataset <- readxl::read_excel("D:/breast-cancer-wisconsin dataset.xlsx")

breast_cancer_dataset <- data.frame(breast_cancer_dataset)

View(breast_cancer_dataset)


attach(breast_cancer_dataset) #-> Search path

breast_cancer_dataset[1]

dim(breast_cancer_dataset)

sum(is.na(breast_cancer_dataset)) #prints sum of null values
summary(is.na(breast_cancer_dataset)) #->changes


colnames(breast_cancer_dataset)[colSums(is.na(breast_cancer_dataset))>0] # prints all column names only with null values

#count total missing values in each column of data frame 
as.data.frame(colSums(is.na(breast_cancer_dataset))) #-> clear output #->changes
sapply(breast_cancer_dataset,function(x) sum(is.na(x)))

#prints the datatype of all the columns with column name
sapply(breast_cancer_dataset,function(x) typeof(x))

#basic operation -> changing the type of some columns
breast_cancer_dataset$diagnosis <- as.integer(breast_cancer_dataset$diagnosis)

#stats
summary(breast_cancer_dataset)
breast_cancer_dataset %>% skim()
glimpse(breast_cancer_dataset)

#print the no.of benign and no.of malignant cases
table(diagnosis)

#finding mean of all the columns using group_by  -> This helps us to identify the difference btw malignant cases and 
# benign cases -> here we observe that the mean values of all the columns for malignant  are greater than benign
# this helps us in the model differenciation

breast_cancer_dataset%>%group_by(diagnosis)%>%summarise_all("mean")


#Splitting train and test
Split_data <- sample.split(radius_mean,SplitRatio = 0.8)
train_data <- subset(breast_cancer_dataset , Split_data == TRUE)
test_data <- subset(breast_cancer_dataset ,Split_data == FALSE)

dim(train_data)
dim(test_data)

#LINEAR Regression
lm_mod1 <- lm(diagnosis~radius_mean,train_data)
lm_mod1
summary(lm_mod1)
plot(lm_mod1)

prediction <- predict(lm_mod,test_data)
prediction

test_data$prediction = prediction
View(test_data)

ggplot(test_data,aes(x = texture_mean)) +geom_point(aes(y = diagnosis),color = "red") +geom_point(aes(y = prediction),color = "blue")
