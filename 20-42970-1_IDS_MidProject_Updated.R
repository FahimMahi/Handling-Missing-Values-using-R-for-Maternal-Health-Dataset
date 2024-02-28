Dataset <- read.csv("D:/Semester/10) Spring 2023/Introduction To Data Science/Project/Dataset_midterm.csv",header=TRUE,sep=",")
Dataset

names(Dataset)
summary(Dataset)
str(Dataset)
sum(is.na(Dataset))

mode(Dataset$id)
mode(Dataset$Age)
mode(Dataset$weight.kg.)
mode(Dataset$Delivery_number)
mode(Dataset$Delivery_time)
mode(Dataset$Blood)
mode(Dataset$Heart)
mode(Dataset$Caesarian)

meanHeart = mean(Dataset$Heart)
print(meanHeart)

medianHeart = median(Dataset$Heart)
print(medianHeart)

names(sort(table(Dataset$weight.kg.)))
names(sort(table(Dataset$Blood)))
names(sort(table(Dataset$Age)))
names(sort(table(Dataset$Delivery_number)))
names(sort(table(Dataset$Delivery_time)))
names(sort(table(Dataset$Caesarian)))
names(sort(table(Dataset$Heart)))

sdweight= sd(Dataset$weight.kg.)
print(sdweight)
sdAge = sd(Dataset$Age)
print(sdAge)
sdDelivery_number= sd(Dataset$Delivery_number)
print(sdDelivery_number)
sdDelivery_time = sd(Dataset$Delivery_time)
print(sdDelivery_time)
sdHeart = sd(Dataset$Heart)
print(sdHeart)
sdCaesarian =sd(Dataset$Caesarian)
print(sdCaesarian)


idRange=max(Dataset$id, na.rm=TRUE)-min(Dataset$id, na.rm=TRUE)
print(idRange)
AgeRange=max(Dataset$Age, na.rm=TRUE)-min(Dataset$Age, na.rm=TRUE)
print(AgeRange)
weightRange=max(Dataset$weight.kg., na.rm=TRUE)-min(Dataset$weight.kg., na.rm=TRUE)
print(weightRange)
Delivery_numberRange=max(Dataset$Delivery_number, na.rm=TRUE)-min(Dataset$Delivery_number, na.rm=TRUE)
print(Delivery_numberRange)
Delivery_timeRange=max(Dataset$Delivery_time, na.rm=TRUE)-min(Dataset$Delivery_time, na.rm=TRUE)
print(Delivery_timeRange)
CaesarianRange=max(Dataset$Caesarian, na.rm=TRUE)-min(Dataset$Caesarian, na.rm=TRUE)
print(CaesarianRange)
HeartRange=max(Dataset$Heart, na.rm=TRUE)-min(Dataset$Heart, na.rm=TRUE)
print(HeartRange)

Dataset$Blood <- factor(Dataset$Blood,
                        levels = c("high","normal","low"),
                        labels = c(1,2,3))
Dataset


#Dataset$Blood <- as.numeric(as.character(Dataset$Blood))
#Dataset$Blood <- as.numeric(Dataset$Blood)
#Dataset

colSums(is.na(Dataset))

which(is.na(Dataset$Age))
which(is.na(Dataset$weight.kg.))
which(is.na(Dataset$Delivery_number))
which(is.na(Dataset$Delivery_time))
which(is.na(Dataset$Blood))
which(is.na(Dataset$Caesarian))

Dataset$missing_value= is.na(Dataset$Age) | is.na(Dataset$weight.kg.) | is.na(Dataset$Delivery_number) | is.na(Dataset$Delivery_time) | is.na(Dataset$Blood)  | is.na(Dataset$Caesarian) 
Dataset

Data= filter(Dataset,Dataset$missing_value !=FALSE)

plot(Dataset$Age,Dataset$missing_value)
plot(Dataset$weight.kg.,Dataset$missing_value)
plot(Dataset$Delivery_number,Dataset$missing_value)
plot(Dataset$Delivery_time,Dataset$missing_value)
plot(Dataset$Blood,Dataset$missing_value)
plot(Dataset$Caesarian,Dataset$missing_value)

#install.packages("matrixStats")
library(matrixStats)
#install.packages("dplyr")
library(dplyr)


remove_missingvalue<- na.omit(Dataset)
remove_missingvalue

# Replacing with Mean
meanvalueAge <-mean(Dataset$Age,na.rm=TRUE)
meanvalueAge
Dataset[is.na(Dataset$Age), "Age"] <-meanvalueAge
Dataset

meanvalueWeight <-mean(Dataset$weight.kg.,na.rm=TRUE)
meanvalueWeight
Dataset[is.na(Dataset$weight.kg.), "weight.kg."] <-meanvalueWeight
Dataset

meanvalueDelivery_number <-mean(Dataset$Delivery_number,na.rm=TRUE)
meanvalueDelivery_number
Dataset[is.na(Dataset$Delivery_number), "Delivery_number"] <-meanvalueDelivery_number
Dataset

meanvalueDelivery_time <-mean(Dataset$Delivery_time,na.rm=TRUE)
meanvalueDelivery_time
Dataset[is.na(Dataset$Delivery_time), "Delivery_time"] <-meanvalueDelivery_time
Dataset

meanvalueCaesarian <-mean(Dataset$Caesarian,na.rm=TRUE)
meanvalueCaesarian
Dataset[is.na(Dataset$Caesarian), "Caesarian"] <-meanvalueCaesarian
Dataset

#meanvalueBlood <-mean(Dataset$Blood,na.rm=TRUE)
#meanvalueBlood
#Dataset[is.na(Dataset$Blood), "Blood"] <-meanvalueBlood
#Dataset

##Mean Blood
# Replace "NA" with NA in the 'Blood' column
Dataset$Blood[Dataset$Blood == "NA"] <- NA

# Convert the 'Blood' column to character
Dataset$Blood <- as.character(Dataset$Blood)

# Calculate the mean excluding NAs
meanvalueBlood <- mean(as.numeric(Dataset$Blood), na.rm = TRUE)

# Replace missing values in 'Blood' column with the mean
Dataset$Blood[is.na(Dataset$Blood)] <- as.character(meanvalueBlood)
Dataset




# Replacing with Median
medianvalueAge <-median(Dataset$Age,na.rm=TRUE)
medianvalueAge
Dataset[is.na(Dataset$Age), "Age"] <-medianvalueAge
Dataset

medianvalueweight <-median(Dataset$weight.kg.,na.rm=TRUE)
medianvalueweight
Dataset[is.na(Dataset$weight.kg.), "weight.kg."] <-medianvalueweight
Dataset

medianvalueDelivery_number <-median(Dataset$Delivery_number,na.rm=TRUE)
medianvalueDelivery_number
Dataset[is.na(Dataset$Delivery_number), "Delivery_number"] <-medianvalueDelivery_number
Dataset

medianvalueDelivery_time <-median(Dataset$Delivery_time,na.rm=TRUE)
medianvalueDelivery_time
Dataset[is.na(Dataset$Delivery_time), "Delivery_time"] <-medianvalueDelivery_time
Dataset

medianvalueCaesarian <-median(Dataset$Caesarian,na.rm=TRUE)
medianvalueCaesarian
Dataset[is.na(Dataset$Caesarian), "Caesarian"] <-medianvalueCaesarian
Dataset

##Median Blood
# Replace "NA" with NA in the 'Blood' column
Dataset$Blood[Dataset$Blood == "NA"] <- NA

# Convert the 'Blood' column to numeric
Dataset$Blood <- as.numeric(Dataset$Blood)

# Calculate the median excluding NAs
medianvalueBlood <- median(Dataset$Blood, na.rm = TRUE)

# Replace missing values in 'Blood' column with the median
Dataset$Blood[is.na(Dataset$Blood)] <- medianvalueBlood
Dataset





# Replacing with Mode
modeAge=names(sort(table(Dataset$Age)))[1]
modeAge
Dataset[is.na(Dataset$Age), "Age"] <-modeAge
Dataset

modeweight=names(sort(table(Dataset$weight.kg.)))[1]
modeweight
Dataset[is.na(Dataset$weight.kg.), "weight.kg."] <-modeweight
Dataset

modeDelivery_number=names(sort(table(Dataset$Delivery_number)))[1]
modeDelivery_number
Dataset[is.na(Dataset$Delivery_number), "Delivery_number"] <-modeDelivery_number
Dataset

modeDelivery_time=names(sort(table(Dataset$Delivery_time)))[1]
modeDelivery_time
Dataset[is.na(Dataset$Delivery_time), "Delivery_time"] <-modeDelivery_time
Dataset

modeCaesarian=names(sort(table(Dataset$Caesarian)))[1]
modeCaesarian
Dataset[is.na(Dataset$Caesarian), "Caesarian"] <-modeCaesarian
Dataset



#########Mode Blood
# Replace "NA" with NA in the 'Blood' column
Dataset$Blood[Dataset$Blood == "NA"] <- NA

# Convert the 'Blood' column to numeric
Dataset$Blood <- as.numeric(Dataset$Blood)

# Function to calculate the mode
calculate_mode <- function(x) {
  tbl <- table(x)
  modes <- tbl[tbl == max(tbl)]
  as.numeric(names(modes))
}

# Calculate the mode excluding NAs
modeBlood <- calculate_mode(Dataset$Blood[!is.na(Dataset$Blood)])

# Replace missing values in 'Blood' column with the mode
Dataset$Blood[is.na(Dataset$Blood)] <- modeBlood
Dataset







#Data Visualization
# Create bar plot
barplot(table(Dataset$Age), main = "Bar Plot of Age", xlab = "Age", ylab = "Frequency", col = "red")
barplot(table(Dataset$weight.kg.), main = "Bar Plot of Weight(kg)", xlab = "Weight(kg)", ylab = "Frequency", col = "blue")
barplot(table(Dataset$Delivery_number), main = "Bar Plot of Delivery Number", xlab = "Delivery Number", ylab = "Frequency", col = "lightblue")
barplot(table(Dataset$Delivery_time), main = "Bar Plot of Delivery Time", xlab = "Delivery Time", ylab = "Frequency", col = "purple")
barplot(table(Dataset$Blood), main = "Bar Plot of Blood", xlab = "Blood", ylab = "Frequency", col = "magenta")
barplot(table(Dataset$Heart), main = "Bar Plot of Heart", xlab = "Heart", ylab = "Frequency", col = "lightgreen")
barplot(table(Dataset$Caesarian), main = "Bar Plot of Caesarian", xlab = "Caesarian", ylab = "Frequency", col = "yellow")

# Create box plot
boxplot(Dataset$Heart, main = "Box Plot for Heart", ylab = "Heart")
boxplot(Dataset$Blood, main = "Box Plot for Blood", ylab = "Blood")
boxplot(Dataset$weight.kg., main = "Box Plot for weight.kg.", ylab = "weight.kg.")

boxplot(Blood ~ weight.kg., Dataset = Dataset, main = "Box Plot for Blood by weight.kg.", ylab = "weight.kg.")
boxplot(Blood ~ Heart, Dataset = Dataset, main = "Box Plot for Blood by Heart", ylab = "Heart")


# Create histogram
hist(Dataset$Age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", col = "lightblue")
hist(Dataset$Blood, main = "Histogram of Blood", xlab = "Blood", ylab = "Frequency", col = "darkblue")
hist(Dataset$Heart, main = "Histogram of Heart", xlab = "Heart", ylab = "Frequency", col = "darkblue")

