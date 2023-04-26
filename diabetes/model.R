library(RCurl) # for downloading the flight CSV file
library(randomForest)
library(caret)

diabetes <- read.csv("diabetes.csv")
diabetes <- na.omit(diabetes)
diabetes$Diabetes_012 <- factor(diabetes$Diabetes_012, levels = c(0, 1, 2), labels = c("No diabetes", "Borderline diabetes", "Diabetes"))
data <- diabetes[sample(nrow(diabetes), 20000, replace = FALSE), ]
set.seed(123) # for reproducibility
TrainingIndex <- createDataPartition(data$Diabetes_012, p = 0.8, list = FALSE)
TrainingSet <- data[TrainingIndex,] # Training Set
TestingSet <- data[-TrainingIndex,]
write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

model_formula <- as.formula("Diabetes_012 ~ HighBP + HighChol + HeartDiseaseorAttack + PhysActivity + Fruits + Veggies + HvyAlcoholConsump+GenHlth + PhysHlth + Sex + Age")
model <- randomForest(model_formula, data = TrainingSet)

# Save model to RDS file
saveRDS(model, "model.rds")
 