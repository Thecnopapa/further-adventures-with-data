library(tidyverse)
library(GGally)
library(caret)

#### Data setup ################################################################

data <- read.csv("Data/brca_data_w_subtypes.csv") # Import dataset


patients <- na.omit(data) # Remove NAs (there are none)
patients <- select(patients, -PR.Status, -ER.Status, -HER2.Final.Status) # Remove incomplete columns
patients$vital.status <- as.factor(patients$vital.status) # Convert to a factor (0 or 1)
patients$histological.type <- as.factor(patients$histological.type)# Convert to a a factor (idk if necessary)


patients.lobular <- patients[ patients$histological.type == "infiltrating lobular carcinoma"] # Filter only lobular cancers
patients.ductal <- patients[ patients$histological.type == "infiltrating ductal carcinoma"] # Filter only ductal cancers


patients.mu <- select(patients, vital.status, histological.type, contains("mu_")) # Filter only mu columns
patients.rs <- select(patients, vital.status, histological.type, contains("rs_")) # Filter only rs columns
patients.pp <- select(patients, vital.status, histological.type, contains("pp_")) # Filter only pp columns
patients.cn <- select(patients, vital.status, histological.type, contains("cn_")) # Filter only cn columns


#### PCA #######################################################################

dataset <- patients# Change to any dataset to analyse (e.g. patients.pp)

pca <- dataset %>% # Get data
  select(-vital.status, -histological.type) %>% # Remove columns
  prcomp(scale. = T) # Run PCA
pca # Show PCA

pca_labelled <- data.frame(pca$x, vital.status = dataset$vital.status) # Label PCA
pca_labelled

pca_labelled %>% ggplot(aes(x = PC1, y = PC2, colour = vital.status)) + # Plot PC1 vs PC2
  geom_point() +
  geom_smooth()

select(pca_labelled, PC1:PC6, vital.status) %>% ggpairs(aes(colour = vital.status)) # Pot the first 6 PCs



#### LDA #######################################################################

ids <- createDataPartition(y = patients$vital.status, p = 0.75, list = F) # Split data into 2

train <- patients %>% slice(ids) # 75% to train
test <- patients %>% slice(-ids) # 25 % to test

# To use same train/test dataset:
# train <- patients
# test <- train


lda <- train %>% # LDA with train dataset
  select(-vital.status, -histological.type) %>% # Remove columns
  MASS::lda(grouping = train$vital.status) # MASS has a different select() function
lda$scaling # show LDA (I guess)


plda <- test %>% 
  select(-vital.status, -histological.type) %>% # Remove same columns as before
  predict(object = lda) # Obtain predictions
plda # Show predictions

table(plda[["class"]],test$vital.status) # Make a table

mat <- confusionMatrix(plda$class, test$vital.status) # Get statistics info
mat # Show results

lda_labelled <- data.frame(plda$x, # Make a table with results
                           vital.status = test$vital.status,
                           class = plda$class)

 
lda_labelled %>% ggpairs(aes(colour = vital.status)) # plot graphs


################################################################################













