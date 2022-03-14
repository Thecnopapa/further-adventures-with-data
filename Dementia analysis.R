library(tidyverse)
library(janitor)
library(GGally)
#library(caret)
#library()

getwd()

patients <-
  read.csv("Data/oasis_longitudinal.csv")
View(patients)
patients <- clean_names(patients)
patients$cdr_num <- patients$cdr
patients$cdr <- factor(patients$cdr)

summary(patients$cdr)


ggplot(patients, aes(y = age, x = group)) +
  geom_boxplot() +
  theme_classic()

ggplot(patients, aes(y = n_wbv, x = age, color = group)) +
  geom_smooth(method = lm) +
  theme_classic()

# normalised brain volume is related to dementia status
# demented have lower normalise brain volume than non demented

ggplot(patients, aes(x = cdr, y = age)) +
  
  geom_boxplot() +
  theme_classic()

ggplot(na.omit (patients[c("cdr", "mmse")]), aes(y = mmse, x = cdr)) +
  # geom_smooth(method = lm) +
  geom_boxplot() +
  theme_classic()

ggplot(patients, aes(y = mmse, x = cdr)) +
  # geom_smooth(method = lm) +
  geom_boxplot() +
  theme_classic()

ggplot(na.omit (patients[c("cdr", "m_f")]), aes(x = m_f, fill = cdr)) +
  # geom_smooth(method = lm) +
  geom_bar() +
  theme_classic()



by_id <- group_by(patients, subject_id)


delta <- summarise(
  by_id,
  cdr_change = last(cdr_num) - first(cdr_num),
  mmse_change = last(mmse) - first(mmse),
  age_change = last(age) - first(age),
  etiv_change = last(e_tiv) - first(e_tiv),
  nwbv_change = last(n_wbv) - first(n_wbv)
)

delta %>% 
  select(-subject_id) %>%
  ggpairs()


by_id <- group_by (merge(by_id, delta), subject_id)

ggplot(delta, aes(x = age_change, y = mmse_change)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(delta, aes(x = age_change, y = nwbv_change)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(delta, aes(x = age_change, y = etiv_change)) +
  geom_point() +
  geom_smooth(method = lm)

#By group


ggplot(by_id, aes(x = age_change, y = mmse_change, color = cdr)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(by_id, aes(x = age_change, y = nwbv_change, color = cdr)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(by_id, aes(x = age_change, y = etiv_change, color = cdr)) +
  geom_point() +
  geom_smooth(method = lm)


patients %>% filter(is.na(ses))

patients_noses <- patients %>% select(- ses, - hand, -cdr_num, -asf) %>% filter(!is.na(mmse))
summary(patients_noses)

patients_noses %>% 
  select(-subject_id, -mri_id) %>%
  ggpairs(aes(colour = cdr))

patients_noses$cdr[patients_noses$cdr == 2] <- 1

patients_noses$cdr <- droplevels(patients_noses$cdr)
levels(patients_noses$cdr)


############ PCA ###############################################################

pca <- patients_noses %>% 
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  prcomp(scale. = T)
pca

pca_labelled <- data.frame(pca$x, cdr = patients_noses$cdr)
pca_labelled

pca_labelled %>% ggplot(aes(x = PC1, y = PC2, colour = cdr)) + 
  geom_point() +
  geom_smooth()

pca_labelled %>% ggpairs(aes(colour = cdr))


############ LDA ###############################################################

lda.test <- function(dataset){

lda <- dataset %>% 
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  MASS::lda(grouping = dataset$cdr) ###### MASS has a different select() function
lda$scaling


plda <- dataset %>% 
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  predict(object = lda)

plda
table(plda[["class"]],dataset$cdr)

mat <- confusionMatrix(plda$class, dataset$cdr)

mat
}
lda.test(patients_noses)
lda_labelled <- data.frame(plda$x,
                          cdr = patients_noses$cdr,
                          class = plda$class)

lda_labelled %>% ggplot(aes(x = LD1, y = LD2, colour = cdr)) + 
 geom_point() +
 geom_smooth()

lda_labelled %>% ggpairs(aes(colour = cdr))

############ LDA trained #######################################################


ids <- createDataPartition(y = patients_noses$cdr, p = 0.75, list = F)
str(ids)

train <- patients_noses %>% slice(ids)
test <- patients_noses %>% slice(-ids)

lda.test(train)
#lda.test(test)



  






lda.test()









