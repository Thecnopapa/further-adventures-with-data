library(ggplot2)
library(tidyverse)
library(janitor)

patients <-
  read.csv("~/Further adventures with data/Data/oasis_longitudinal.csv")
View(patients)
patients <- clean_names(patients)
patients$cdr_num <- patients$cdr
patients$cdr <- factor(patients$cdr)




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
