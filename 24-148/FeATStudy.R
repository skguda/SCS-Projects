install.packages(c("readxl", "dplyr", "tidyr"))
library(readxl)
library(dplyr)
library(tidyr)

#Merging datasets
merged_data <- abcd_p_demo %>%
  left_join(mri_y_smr_vol_aseg, by = c("src_subject_id", "eventname")) %>%
  left_join(ph_y_anthro, by = c("src_subject_id", "eventname")) %>%
  left_join(ph_y_bld, by = c("src_subject_id", "eventname"))  

head(merged_data)
colnames(merged_data)
summary(merged_data)


# Check for missing values
colSums(is.na(merged_data))


key_vars <- c("src_subject_id", "eventname", "biospec_blood_ferritin", "smri_vol_scs_hpuslh", "smri_vol_scs_hpusrh", "anthroweightcalc", "anthroheightcalc", "biospec_blood_wbc_count", "biospec_blood_hemoglobin")

subset_data <- merged_data %>%
  select(all_of(key_vars))

str(subset_data$biospec_blood_ferritin)

subset_data$biospec_blood_ferritin <- as.numeric(subset_data$biospec_blood_ferritin)
subset_data$biospec_blood_wbc_count <- as.numeric(subset_data$biospec_blood_wbc_count)
subset_data$anthroheightcalc <- as.numeric(subset_data$anthroheightcalc)
subset_data$anthroweightcalc <- as.numeric(subset_data$anthroweightcalc)


cleaned_subset <- subset_data %>%
  filter(!is.na(biospec_blood_ferritin) & 
           !is.na(smri_vol_scs_hpuslh) & 
           !is.na(smri_vol_scs_hpusrh) & 
           !is.na(anthroweightcalc) & 
           !is.na(anthroheightcalc) & 
           !is.na(biospec_blood_hemoglobin) & 
           !is.na(biospec_blood_wbc_count))

analysis_data <- cleaned_subset %>%
  mutate(
    total_hippocampal_volume = smri_vol_scs_hpuslh + smri_vol_scs_hpusrh,
    BMI = (anthroweightcalc / (anthroheightcalc * anthroheightcalc)) * 703
  )

# Histogram for BMI
ggplot(analysis_data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "pink", alpha = 0.7) +
  labs(title = "Distribution of BMI", x = "BMI", y = "Count")


# Histogram for total hippocampal volume
ggplot(analysis_data, aes(x = total_hippocampal_volume)) +
  geom_histogram(binwidth = 100, fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Total Hippocampal Volume", x = "Volume", y = "Count")

# Histogram for total Ft
ggplot(analysis_data, aes(x = biospec_blood_ferritin)) +
  geom_histogram(binwidth = 5, fill = "orange", alpha = 0.7) +
  labs(title = "Distribution of Ferritin", x = "Ferritin", y = "Count")

# Histogram for left hippocampal volume
ggplot(analysis_data, aes(x = smri_vol_scs_hpuslh)) +
  geom_histogram(binwidth = 100, fill = "gold", alpha = 0.7) +
  labs(title = "Distribution of Left Hippocampal Volume", x = "Volume", y = "Count")

# Histogram for right hippocampal volume
ggplot(analysis_data, aes(x = smri_vol_scs_hpusrh)) +
  geom_histogram(binwidth = 100, fill = "turquoise", alpha = 0.7) +
  labs(title = "Distribution of Right Hippocampal Volume", x = "Volume", y = "Count")

# Histogram for WBC
ggplot(analysis_data, aes(x = biospec_blood_wbc_count)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of WBC", x = "WBC", y = "Count")


# Scatter plot of BMI vs total hippocampal volume
ggplot(analysis_data, aes(x = BMI, y = total_hippocampal_volume)) +
  geom_point(alpha = 0.5) +
  labs(title = "BMI vs Total Hippocampal Volume", x = "BMI", y = "Total Hippocampal Volume")

library(car)

# Q-Q plot for total ferritin
qqPlot(analysis_data$biospec_blood_ferritin, main = "Q-Q Plot for Total Ferritin")

# Q-Q plot for total WBC
qqPlot(analysis_data$biospec_blood_wbc_count, main = "Q-Q Plot for total wbc")

# Q-Q plot for total hemoglobin
qqPlot(analysis_data$biospec_blood_hemoglobin, main = "Q-Q Plot for hemoglobin")

# Q-Q plot for BMI
qqPlot(analysis_data$BMI, main = "Q-Q Plot for BMI")

# Q-Q plot for total hippocampal volume
qqPlot(analysis_data$total_hippocampal_volume, main = "Q-Q Plot for Total Hippocampal Volume")

# Q-Q plot for left hippocampus
qqPlot(analysis_data$smri_vol_scs_hpuslh, main = "Q-Q Plot for left Hippocampal Volume")

# Q-Q plot for right hippocampus
qqPlot(analysis_data$smri_vol_scs_hpusrh, main = "Q-Q Plot for right Hippocampal Volume")


# Shapiro-Wilk test for normality
shapiro.test(analysis_data$biospec_blood_ferritin)
shapiro.test(analysis_data$biospec_blood_wbc_count)
shapiro.test(analysis_data$biospec_blood_hemoglobin)
shapiro.test(analysis_data$BMI)
shapiro.test(analysis_data$total_hippocampal_volume)
shapiro.test(analysis_data$smri_vol_scs_hpuslh)
shapiro.test(analysis_data$smri_vol_scs_hpusrh)


# Count participants per year
year_counts <- analysis_data %>%
  group_by(eventname) %>%
  summarise(count = n())

print("Participant counts by year:")
print(year_counts)

# Count unique IDs
unique_ids <- analysis_data %>%
  distinct(src_subject_id) %>%
  nrow()

print(paste("Number of unique participant IDs:", unique_ids))

participants_both_years <- analysis_data %>%
  filter(eventname %in% c("2_year_follow_up_y_arm_1", "4_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  filter(n() == 2) %>%
  summarise()

num_participants <- nrow(participants_both_years)

print(paste("Number of participants with data from both year 2 and year 4:", num_participants))

year2_data <- analysis_data %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(src_subject_id, biospec_blood_ferritin, biospec_blood_wbc_count, biospec_blood_hemoglobin)

year4_data <- analysis_data %>%
  filter(eventname == "4_year_follow_up_y_arm_1") %>%
  select(src_subject_id, smri_vol_scs_hpuslh, smri_vol_scs_hpusrh, anthroweightcalc, anthroheightcalc)

longitudinal_data <- year2_data %>%
  inner_join(year4_data, by = "src_subject_id")

summary(analysis_data)
nrow(analysis_data)
