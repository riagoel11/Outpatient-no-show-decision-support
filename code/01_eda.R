# ============================================================================
# Section 00 - SET UP
# ============================================================================
# Data manipulation
library(tidyverse)
library(lubridate)
# Data visualisation
library(ggplot2)
library(scales)
#library(viridis)
# Statistical analysis
library(psych)
library(rstatix)
# Correlation plots
library(corrplot)
# Tables
library(knitr)
library(gt)
# Themes
library(hrbrthemes)

# ============================================================================
# Colour theme
# ============================================================================
# Colors
dissertation_colors <- c(
  primary = "#1B3A5C",
  secondary = "#2E6F95",
  accent = "#F2A541",
  background = "#F7F9FB",
  text = "#1F1F1F"
)
# Theme
theme_dissertation <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#F7F9FB", color = NA),
    panel.background = element_rect(fill = "#F7F9FB", color = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = "#1B3A5C", face = "bold"),
    plot.subtitle = element_text(color = "#2E6F95"),
    axis.title = element_text(color = "#1F1F1F"),
    axis.text = element_text(color = "#1F1F1F"),
    legend.title = element_text(color = "#1B3A5C", face = "bold")
  )
# Discrete color/fill scales
scale_fill_dissertation <- function(...) scale_fill_manual(values = unname(dissertation_colors), ...)
scale_color_dissertation <- function(...) scale_color_manual(values = unname(dissertation_colors), ...)

# ============================================================================
# Section 01 - Data Import and Missingness
# ============================================================================
data <- read.csv("KaggleV2-May-2016.csv")
head(data)
str(data)
dim(data)
names(data)
colSums(is.na(data))
#Quick summary
summary(data)
# ---- Table 1: Numeric ----
num_vars <- c("Age")
numeric_summary <- data %>%
  summarise(across(all_of(num_vars),
    list(
      n = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Q1 = ~quantile(., 0.25, na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      Q3 = ~quantile(., 0.75, na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("Variable", "Statistic"), names_sep = "__") %>%
  pivot_wider(names_from = Statistic, values_from = value)
gt_numeric <- numeric_summary %>% # AI was used in an assistive manner to prepare the tables
  gt() %>%
  tab_header(
    title = md("**Table 1. Summary of Numeric Variables**"),
    subtitle = "Brazilian Medical Appointment No-Shows (N = 110,527)"
  ) %>%
  fmt_number(columns = c(Mean, SD, Min, Q1, Median, Q3, Max), decimals = 1) %>%
  fmt_number(columns = n, decimals = 0, use_seps = TRUE) %>%
  cols_label(n = "N") %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = dissertation_colors[["primary"]], weight = "bold"),
    locations = cells_body(columns = Variable)
  ) %>%
  tab_options(
    table.background.color = dissertation_colors[["background"]],
    table.font.color = dissertation_colors[["text"]],
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dissertation_colors[["primary"]],
    table.border.top.color = dissertation_colors[["primary"]],
    table.border.bottom.color = dissertation_colors[["primary"]],
    data_row.padding = px(6)
  )
# ---- Table 2: Categorical ----
cat_vars <- c("Gender", "Scholarship", "Hipertension", "Diabetes",
  "Alcoholism", "Handcap", "SMS_received", "No.show")
categorical_summary <- lapply(cat_vars, function(v) {
  data %>%
    count(.data[[v]]) %>%
    mutate(Variable = v,
      Level = as.character(.data[[v]]),
      Percent = n / sum(n) * 100) %>%
    select(Variable, Level, Count = n, Percent)
}) %>% bind_rows()
gt_categorical <- categorical_summary %>%
  group_by(Variable) %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Distribution of Categorical Variables**"),
    subtitle = "Counts and within-variable percentages"
  ) %>%
  fmt_number(columns = Count, decimals = 0, use_seps = TRUE) %>%
  fmt_number(columns = Percent, decimals = 1) %>%
  cols_label(Percent = "%") %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["secondary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.background.color = dissertation_colors[["background"]],
    table.font.color = dissertation_colors[["text"]],
    heading.title.font.size = 16,
    data_row.padding = px(5)
  )
gt_numeric
gt_categorical
# ----------------------------------------------------------------------------
# DUPLICATE CHECKS
# ----------------------------------------------------------------------------
# ---- 1. COMPLETE ROW DUPLICATES ----
# Every column identical across two or more rows and this would indicate the
# same record was accidentally entered twice
n_complete_dupes <- sum(duplicated(data))
cat("Complete row duplicates:", n_complete_dupes,
  "(", round(n_complete_dupes / nrow(data) * 100, 3), "% of rows)\n")
# if any exist
if (n_complete_dupes > 0) {
  complete_dupe_rows <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
  print(complete_dupe_rows)
}
# ---- 2. APPOINTMENT DUPLICATES ----
# AppointmentID should be unique per appointment by design.
n_appointment_dupes <- sum(duplicated(data$AppointmentID))
cat("Appointment ID duplicates:", n_appointment_dupes,
  "(", round(n_appointment_dupes / nrow(data) * 100, 3), "% of rows)\n")
if (n_appointment_dupes > 0) {
  dupe_appointment_ids <-
    data$AppointmentID[duplicated(data$AppointmentID)]
  appointment_dupe_rows <- data %>% filter(AppointmentID %in% dupe_appointment_ids) %>%
    arrange(AppointmentID)
  print(appointment_dupe_rows)
}
# ---- 3. PATIENT DUPLICATES ----
# PatientId repeating is *Expected*
n_unique_patients <- n_distinct(data$PatientId)
n_repeat_patients <- data %>% count(PatientId) %>% filter(n > 1) %>% nrow()
cat("Unique patients:", n_unique_patients, "\n")
cat("Patients with more than one appointment:", n_repeat_patients,
  "(", round(n_repeat_patients / n_unique_patients * 100, 1), "% of unique patients)\n")
# ------4.Same patient, same PatientId AND same AppointmentDay-------
patient_same_day_dupes <- data %>%
  count(PatientId, AppointmentDay) %>%
  filter(n > 1)
cat("Patient-days with more than one appointment record:",
  nrow(patient_same_day_dupes),
  "(", round(nrow(patient_same_day_dupes) / nrow(data) * 100, 3), "% of rows)\n")
if (nrow(patient_same_day_dupes) > 0) {
  print(patient_same_day_dupes %>% arrange(desc(n)))}
# ----------------------------------------------------------------------------
# Prepare for Lead Time Calculation
# ----------------------------------------------------------------------------
# Lead time is engineered as a key predictor based on previous literature
# ( Samorani & LaGanga, 2015; Deina et al., 2024).
##Check date columns
head(data[c("ScheduledDay", "AppointmentDay")])
# Check data types
class(data$ScheduledDay)
class(data$AppointmentDay)
# Converting to date format
data$ScheduledDay <- as.Date(data$ScheduledDay)
data$AppointmentDay <- as.Date(data$AppointmentDay)
# Calculate Lead time in days
data$LeadTime <- as.numeric(data$AppointmentDay - data$ScheduledDay)
# ----------------------------------------------------------------------------
# INVALID RECORDS — summary table
# ----------------------------------------------------------------------------
invalid_records <- data.frame(
  Issue = c("Negative Age (-1)", "Negative Lead Time"),
  Count = c(
    sum(data$Age == -1, na.rm = TRUE),
    sum(data$LeadTime < 0, na.rm = TRUE)
  ),
  Action = c("Removed", "Removed")
)
# ---- Remove invalid records ----
n_before <- nrow(data)
data <- data %>%
  filter(Age >= 0, # drops the 1 negative age
    LeadTime >= 0) # drops the 5 negative lead times
n_after <- nrow(data)
cat("Removed", n_before - n_after, "invalid records (",
  n_before, "->", n_after, ")\n")
print(invalid_records)
gt_invalid <- invalid_records %>%
  gt() %>%
  tab_header(title = md("**Table 3. Invalid Records Removed During Data Quality Checks**")) %>%
  cols_align(align = "center", columns = Count) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.background.color = dissertation_colors[["background"]],
    table.font.color = dissertation_colors[["text"]]
  )
gt_invalid

# ============================================================================
# Section 2 - UNDERSTAND THE DATA
# ============================================================================
# ----------------------------------------------------------------------------
# PATIENT CHARACTERISTICS
# ----------------------------------------------------------------------------
patient_freq <- data %>%
  count(PatientId, name = "N_Appointments")
patient_characteristics <- data.frame(
  Metric = c("Unique patients", "Total appointments",
    "Mean appointments per patient", "Median appointments per patient",
    "Maximum appointments (single patient)", "Patients with only 1 appointment (%)"),
  Value = c(
    n_distinct(data$PatientId),
    nrow(data),
    round(mean(patient_freq$N_Appointments), 2),
    median(patient_freq$N_Appointments),
    max(patient_freq$N_Appointments),
    round(mean(patient_freq$N_Appointments == 1) * 100, 1)
  )
)
print(patient_characteristics)
gt_patient_char <- patient_characteristics %>%
  gt() %>%
  tab_header(title = md("**Table 2.5. Patient Characteristics**")) %>%
  cols_align(align = "right", columns = Value) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = dissertation_colors[["primary"]], weight = "bold"),
    locations = cells_body(columns = Metric)
  ) %>%
  tab_options(
    table.background.color = dissertation_colors[["background"]],
    table.font.color = dissertation_colors[["text"]],
    data_row.padding = px(6)
  )
gt_patient_char

# ============================================================================
# Section 2 - UNDERSTAND THE TARGET VARIABLE
# ============================================================================
table(data$No.show) # Counts of each class
prop.table(table(data$No.show)) * 100 # Percentages
# noshow summary table
no_show_summary <- data %>%
  group_by(No.show) %>%
  summarise(Count = n(), Percentage = n() / nrow(data) * 100)
print(no_show_summary)
# Visualize: Bar chart of no-show distribution
ggplot(data, aes(x = factor(No.show), fill = factor(No.show))) +
  geom_bar() +
  labs(title = "Figure 1. Distribution of appointment attendance, showing a 20.2% no-show rate",
    x = "No-show Status (0 = Attended, 1 = No-show)",
    y = "Frequency") +
  scale_fill_manual(values = c("No" = dissertation_colors[["primary"]],
    "Yes" = dissertation_colors[["accent"]])) +
  theme_dissertation +
  theme(legend.position = "none")

# ============================================================================
# Section 3 - FEATURE ENGINEERING
# ============================================================================
# ---- Target variable ----
data$No_show_numeric <- ifelse(data$No.show == "Yes", 1, 0)
# ---- Ensure dates are Date type (needed for 3.1, 3.4, 3.5) ----
data$ScheduledDay <- as.Date(data$ScheduledDay)
data$AppointmentDay <- as.Date(data$AppointmentDay)
# ---- 3.1 Lead time ----
data$LeadTime <- as.numeric(data$AppointmentDay - data$ScheduledDay)
# Lead-time bands
data$LeadTimeBand <- cut(data$LeadTime,
  breaks = c(-1, 0, 3, 7, 14, 30, 1000),
  labels = c("Same day", "1-3 days", "4-7 days",
    "8-14 days", "15-30 days", "30+ days"))
# ---- 3.2 Age groups ----
data$AgeGroup <- cut(data$Age,
  breaks = c(-1, 12, 18, 35, 50, 65, 120),
  labels = c("Child (0-12)", "Teen (13-18)", "Young Adult (19-35)",
    "Middle Age (36-50)", "Older Adult (51-65)", "Senior (65+)"))
# ---- 3.3 Comorbidity count ----
data$ComorbidityCount <- data$Diabetes + data$Hipertension +
  data$Alcoholism
# ---- Handicap binary flag (any disability, given severe sparsity of levels 1-4) ----
data$Handicap_Any <- ifelse(data$Handcap > 0, 1, 0)
# ---- 3.4 Visit frequency (merged back onto every row, not a separate object) ----
patient_freq <- data %>%
  count(PatientId, name = "N_Appointments")
data <- data %>%
  left_join(patient_freq, by = "PatientId") %>%
  mutate(VisitBand = case_when(
    N_Appointments == 1 ~ "1 visit",
    N_Appointments %in% 2:3 ~ "2-3 visits",
    N_Appointments %in% 4:9 ~ "4-9 visits",
    TRUE ~ "10+ visits"
  ))
# ---- 3.5 Same-day appointments ----
data$SameDay <- ifelse(data$LeadTime == 0, "Same-day", "Booked in advance")
# ---- 3.6 Supporting time variables ----
data$AppointmentMonth <- format(data$AppointmentDay, "%Y-%m")
data$AppointmentDOW <- factor(weekdays(data$AppointmentDay),
  levels = c("Monday","Tuesday","Wednesday",
    "Thursday","Friday","Saturday","Sunday"))
# ---- 3.7 Risk Score Baseline ----
data$RiskScore <- with(data,
  ifelse(LeadTime > 14, 1, 0) +
    ifelse(Scholarship == 1, 1, 0) +
    ifelse(Age < 30, 1, 0))

# ============================================================================
# Section 4 - EXPLORATORY DATAANALYSIS
# ============================================================================
# -----------------------------------------------------------------------------
# 4.1 Outcome Prevalence
# -----------------------------------------------------------------------------
table(data$No.show)
prop.table(table(data$No.show)) * 100
no_show_summary <- data %>% group_by(No.show) %>%
  summarise(Count = n(), Percentage = n() / nrow(data) * 100)
print(no_show_summary)
ggplot(no_show_summary, aes(x = "", y = Percentage, fill = No.show)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(No.show, ": ", round(Percentage,1), "%")),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_dissertation() +
  labs(title = "Roughly 1 in 5 appointments end in a no-show", x = NULL, y = NULL) +
  theme_dissertation + theme(legend.position = "none", axis.text.y = element_blank())
# -----------------------------------------------------------------------------
# 4.2 Lead Time (Expected to be an important variale)
# -----------------------------------------------------------------------------
summary(data$LeadTime); sd(data$LeadTime, na.rm = TRUE)
ggplot(data, aes(x = factor(No_show_numeric), y = LeadTime, fill = factor(No_show_numeric))) +
  geom_boxplot() +
  scale_fill_dissertation() +
  labs(title = "Lead Time by Appointment Outcome", x = "No-show Status",
    y = "Days between Scheduling and Appointment") +
  theme_dissertation + theme(legend.position = "none")
cor.test(data$LeadTime, data$No_show_numeric, method = "pearson") # r = 0.186
cor.test(data$LeadTime, data$No_show_numeric, method = "spearman") # rho = 0.282
# Lead-time bands -
leadtime_band_summary <- data %>%
  filter(!is.na(LeadTimeBand)) %>%
  group_by(LeadTimeBand) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100)
print(leadtime_band_summary)
ggplot(leadtime_band_summary, aes(x = LeadTimeBand, y = NoShow_Rate)) +
  geom_col(fill = dissertation_colors["primary"]) +
  geom_text(aes(label = paste0(round(NoShow_Rate,1), "%")), vjust = -0.5, size = 3.8, fontface = "bold",
    color = dissertation_colors["text"]) +
  labs(title = "No-show rate by lead-time band",
    x = "Lead Time Band", y = "No-show Rate (%)") +
  theme_dissertation
# Same-day vs booked-in-advance (supports the lead-time-band story)
data %>% group_by(SameDay) %>% summarise(N = n(), NoShow_Rate = mean(No_show_numeric)*100)
chisq.test(table(data$SameDay, data$No_show_numeric))
# ---- 4.3 Age ----
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = dissertation_colors["primary"], color = "white") +
  labs(title = "Distribution of Patient Age (descriptive)", x = "Age (years)", y = "Frequency") +
  theme_dissertation
cor.test(data$Age, data$No_show_numeric, method = "pearson")
age_group_summary <- data %>% filter(!is.na(AgeGroup)) %>%
  group_by(AgeGroup) %>% summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100)
print(age_group_summary)
ggplot(age_group_summary, aes(x = AgeGroup, y = NoShow_Rate)) +
  geom_segment(aes(xend = AgeGroup, y = 0, yend = NoShow_Rate), color = "grey70", linewidth = 1) +
  geom_point(size = 5, color = dissertation_colors["primary"]) +
  geom_text(aes(label = paste0(round(NoShow_Rate,1), "%")), vjust = -1.5, size = 3.6) +
  labs(title = "Figure 3: No-show rate declines steadily with age",
    x = NULL, y = "No-show Rate (%)") +
  theme_dissertation
# ---- 4.4 Gender ----
table(data$Gender)
gender_table <- table(data$Gender, data$No_show_numeric)
chisq.test(gender_table)
ggplot(data, aes(x = "", y = after_stat(count), fill = Gender)) +
  geom_bar(width = 0.5, position = "fill") +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  scale_fill_dissertation() +
  labs(title = "Gender split (65% female / 35% male) - not associated with no-show", x=NULL, y=NULL) +
  theme_dissertation
# ---- 4.5 Chronic Conditions ----
baseline_rate <- mean(data$No_show_numeric) * 100
comorbidity_combined <- data %>%
  summarise(
    Alcoholism = list(c(N = sum(Alcoholism==1), Rate = mean(No_show_numeric[Alcoholism==1])*100)),
    Diabetes = list(c(N = sum(Diabetes==1), Rate = mean(No_show_numeric[Diabetes==1])*100)),
    Hipertension = list(c(N = sum(Hipertension==1), Rate = mean(No_show_numeric[Hipertension==1])*100))
  ) %>%
  pivot_longer(everything(), names_to = "Condition", values_to = "vals") %>%
  mutate(N = map_dbl(vals, "N"), NoShow_Rate = map_dbl(vals, "Rate")) %>% select(-vals)
# the calculation was suggest by AI but ran and crosschecked thoroughy by author
ggplot(comorbidity_combined, aes(x = N, y = NoShow_Rate, size = N, color = Condition)) +
  geom_hline(yintercept = baseline_rate, linetype = "dashed", color = "grey50",
    linewidth = 0.6) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = Condition), vjust = -1.8, size = 4, fontface = "bold",
    show.legend = FALSE) +
  geom_text(aes(label = paste0(round(NoShow_Rate,1), "%")), vjust = 2, size = 3.3, show.legend = FALSE, color = "grey20") +
  annotate("text", x = max(comorbidity_combined$N)*0.95, y = baseline_rate +
      0.5,
    label = paste0("Overall rate: ", round(baseline_rate,1), "%"),
    hjust = 1, size = 3, color = "grey50", fontface = "italic") +
  scale_size_continuous(range = c(10, 30), guide = "none") +
  scale_color_dissertation() +
  scale_x_continuous(labels = comma, expand = expansion(mult = 0.25)) +
  labs(title = "Chronic conditions are common, but linked to fewer no-shows",
    x = "Number of patients with condition", y = "No-show rate among that group (%)") +
  theme_dissertation + theme(legend.position = "none")
chisq.test(table(data$Diabetes, data$No_show_numeric))
chisq.test(table(data$Hipertension, data$No_show_numeric))
chisq.test(table(data$Alcoholism, data$No_show_numeric))
Handcap_summary <- data %>% group_by(Handcap) %>% summarise(Count = n())
ggplot(Handcap_summary, aes(x = factor(Handcap), y = Count)) +
  geom_segment(aes(xend = factor(Handcap), y = 1, yend = Count), color = "grey70", linewidth = 1) +
  geom_point(size = 5, color = dissertation_colors["accent"]) +
  geom_text(aes(label = comma(Count)), vjust = -1.2, size = 3.3) +
  scale_y_log10(labels = comma) +
  labs(title = "Handicap coding is extremely sparse beyond category 0",
    x = "Handicap level", y = "Number of patients (log scale)") +
  theme_dissertation
chisq.test(table(data$Handcap, data$No_show_numeric))
comorbidity_dose <- data %>% group_by(ComorbidityCount) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100)
print(comorbidity_dose)
# ---- 4.6 Scholarship ----
table(data$Scholarship)
scholarship_summary <- data %>% group_by(Scholarship) %>%
  summarise(Count = n(), Percentage = n() / nrow(data) * 100)
print(scholarship_summary)
ggplot(scholarship_summary, aes(x = "", y = Percentage, fill = factor(Scholarship))) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(ifelse(Scholarship==1,"Enrolled","Not enrolled"), ": ", round(Percentage,1),"%")),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_dissertation() +
  labs(title = "1 in 10 patients are enrolled in the welfare scheme", x = NULL, y = NULL) +
  theme_dissertation + theme(legend.position = "none", axis.text.y = element_blank())
chisq.test(table(data$Scholarship, data$No_show_numeric))
# ---- 4.7 SMS Reminders ----
table(data$SMS_received)
chisq.test(table(data$SMS_received, data$No_show_numeric))
data %>% group_by(SMS_received) %>%
  summarise(Mean_LeadTime = mean(LeadTime, na.rm=TRUE),
    Median_LeadTime = median(LeadTime, na.rm=TRUE))
ggplot(data, aes(x = factor(SMS_received, labels = c("No SMS","SMS received")), y = LeadTime, fill = factor(SMS_received))) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA) +
  scale_fill_dissertation() +
  labs(title = "SMS reminders were sent almost exclusively to long-lead-time bookings",
    subtitle = "This confounds any apparent link between SMS and no-show risk",
    x = NULL, y = "Lead time (days)") +
  theme_dissertation + theme(legend.position = "none") + coord_cartesian(ylim = c(0,100))
t.test(LeadTime ~ SMS_received, data = data)
# ---- 4.8 Neighbourhood ----
n_distinct(data$Neighbourhood)
neighbourhood_summary <- data %>% group_by(Neighbourhood) %>%
  summarise(Count = n(), Percentage = n()/nrow(data)*100) %>%
  arrange(desc(Count))
top10 <- neighbourhood_summary %>% slice_max(Count, n = 10)
neighbourhood_noshow <- data %>% filter(Neighbourhood %in% top10$Neighbourhood) %>%
  group_by(Neighbourhood) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric,
    na.rm=TRUE)*100) %>%
  arrange(desc(NoShow_Rate))
print(neighbourhood_noshow)
top10_full <- top10 %>% left_join(neighbourhood_noshow, by = "Neighbourhood")
ggplot(top10_full, aes(x = reorder(Neighbourhood, Count), y = Count, color = NoShow_Rate)) +
  geom_segment(aes(xend = Neighbourhood, y = 0, yend = Count), color = "grey80", linewidth = 1) +
  geom_point(size = 5) +
  scale_color_gradient(low = dissertation_colors["primary"], high = dissertation_colors["accent"], name = "No-show %") +
  coord_flip() +
  labs(title = "Appointment volume and no-show rate: top 10 neighbourhoods",
    subtitle = "Descriptive only - not central to the research contribution",
    x = NULL, y = "Number of appointments") +
  theme_dissertation
# ---- 4.9 Temporal Patterns ----
month_summary <- data %>% group_by(AppointmentMonth) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100) %>%
  arrange(AppointmentMonth)
print(month_summary)
ggplot(month_summary, aes(x = AppointmentMonth, y = NoShow_Rate, group = 1)) +
  geom_line(color = dissertation_colors["secondary"], linewidth = 1.1) +
  geom_point(color = dissertation_colors["secondary"], size = 3) +
  geom_text(aes(label = paste0(round(NoShow_Rate,1),"%")), vjust = -1, size = 3.2) +
  labs(title = "No-show rate by appointment month",
    subtitle = "No meaningful seasonal pattern - dataset spans only 3 months",
    x = NULL, y = "No-show rate (%)") +
  theme_dissertation
dow_summary <- data %>% filter(!is.na(AppointmentDOW)) %>%
  group_by(AppointmentDOW) %>% summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100)
print(dow_summary)
dow_summary_clean <- dow_summary %>% filter(N >= 1000)
ggplot(dow_summary_clean, aes(x = AppointmentDOW, y = NoShow_Rate)) +
  geom_segment(aes(xend = AppointmentDOW, y = 0, yend = NoShow_Rate),
    color = "grey70", linewidth = 1) +
  geom_point(size = 5, color = dissertation_colors["secondary"]) +
  geom_text(aes(label = paste0(round(NoShow_Rate,1), "%")), vjust = -1.5, size = 3.8, fontface = "bold") +
  labs(title = "No-show rate by day of the week",
    subtitle = "Monday-Friday only; Saturday (N=39) and Sunday (N=0) excluded as unreliable/non-operating",
    x = NULL, y = "No-show rate (%)") +
  theme_dissertation + theme(panel.grid.major.x = element_blank())
weekday_data <- data %>% filter(AppointmentDOW %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
chisq.test(table(weekday_data$AppointmentDOW,
  weekday_data$No_show_numeric))
# ---- 4.10 Repeat Patients ----
freq_summary <- patient_freq %>%
  mutate(Visits = case_when(N_Appointments==1 ~ "1 visit", N_Appointments
    %in% 2:3 ~ "2-3 visits",
    N_Appointments %in% 4:9 ~ "4-9 visits", TRUE ~ "10+ visits")) %>%
  count(Visits)
print(freq_summary)
ggplot(patient_freq, aes(x = N_Appointments)) +
  stat_ecdf(geom = "step", color = dissertation_colors["primary"], linewidth = 1) +
  labs(title = "Cumulative Distribution of Appointments per Patient",
    x = "Number of Appointments", y = "Cumulative Proportion of Patients") +
  theme_dissertation
patient_visit_noshow <- data %>%
  group_by(VisitBand) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100)
print(patient_visit_noshow)

# =============================================================================
# SECTION 05 - INTERACTION AND OPERATIONAL INSIGHTS
# =============================================================================
# ---- 5.1 Scholarship x Age ----
age_welfare_summary <- data %>%
  filter(!is.na(AgeGroup)) %>%
  group_by(AgeGroup, Scholarship) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100, .groups = "drop") %>%
  mutate(Welfare = ifelse(Scholarship == 1, "Enrolled", "Not enrolled"))
print(age_welfare_summary, n = Inf)
slope_data <- age_welfare_summary %>% filter(N >= 100)
ggplot(slope_data, aes(x = Welfare, y = NoShow_Rate, group = AgeGroup)) +
  geom_line(color = "grey70", linewidth = 0.8) +
  geom_point(aes(color = Welfare), size = 4) +
  geom_text(aes(label = paste0(round(NoShow_Rate,1),"%")),
    hjust = ifelse(slope_data$Welfare == "Enrolled", 1.4, -0.4), size = 3.2) +
  facet_wrap(~AgeGroup, nrow = 1) +
  scale_color_dissertation() +
  labs(title = "The welfare no-show gap holds across most age groups",
    subtitle = "Senior (65+) reverses direction, but N=251 enrolled - likely a small-sample artefact",
    x = NULL, y = "No-show rate (%)") +
  theme_dissertation + theme(legend.position = "none")
senior_table <- data %>% filter(AgeGroup == "Senior (65+)") %>%
  select(Scholarship, No_show_numeric) %>% table()
chisq.test(senior_table)
# ---- 5.2 SMS x Lead Time ----
sms_within_leadtime <- data %>%
  filter(!is.na(LeadTimeBand)) %>%
  group_by(LeadTimeBand, SMS_received) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100, .groups = "drop")
print(sms_within_leadtime, n = Inf)
# ---- 5.3 Composite Risk Score ----
#Here the code was debugged using Chat GPT to prepare the risk summary
risk_summary <- data %>% filter(!is.na(RiskScore)) %>%
  group_by(RiskScore) %>% summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100)
scale_factor <- max(risk_summary$NoShow_Rate) / max(risk_summary$N)
ggplot(risk_summary, aes(x = factor(RiskScore))) +
  geom_col(aes(y = N * scale_factor), fill = "grey80", width = 0.6) +
  geom_line(aes(y = NoShow_Rate, group = 1), color = dissertation_colors["accent"], linewidth = 1.2) +
  geom_point(aes(y = NoShow_Rate), color = dissertation_colors["accent"], size = 3) +
  geom_text(aes(y = NoShow_Rate, label = paste0(round(NoShow_Rate,1),
    "%")),
    vjust = -1, color = dissertation_colors["text"], fontface = "bold") +
  scale_y_continuous(name = "No-show Rate (%)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Number of Patients (N)")) +
  labs(title = "No-show Rate and Patient Volume by Composite Risk Score",
    x = "Number of Risk Factors Present") +
  theme_dissertation +
  theme(axis.title.y.right = element_text(color = "grey40"),
    axis.title.y.left = element_text(color = dissertation_colors["accent"]))
profile_summary <- data %>% filter(!is.na(Age)) %>%
  mutate(Flag_LongLead = ifelse(LeadTime > 15, 1, 0),
    Flag_Welfare = ifelse(Scholarship == 1, 1, 0),
    Flag_Young = ifelse(Age < 30, 1, 0),
    Flag_NoHyperten = ifelse(Hipertension == 0, 1, 0)) %>%
  group_by(Flag_LongLead, Flag_Welfare, Flag_Young, Flag_NoHyperten) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100, .groups = "drop") %>%
  filter(N >= 100) %>% arrange(desc(NoShow_Rate))
print(profile_summary, n = Inf)
# ---- 5.4 Exploratory Operational Targeting ----
data_ranked <- data %>% filter(!is.na(RiskScore)) %>%
  arrange(desc(RiskScore))
top10pct_n <- round(0.10 * nrow(data_ranked))
top10pct <- data_ranked %>% slice(1:top10pct_n)
total_noshows <- sum(data_ranked$No_show_numeric)
captured_noshows <- sum(top10pct$No_show_numeric)
capture_rate <- captured_noshows / total_noshows * 100
cat("Targeting the top 10% by risk score captures", round(capture_rate,1),
  "% of all no-shows in the dataset.\n")

# =============================================================================
# SECTION 06 - STATISTICAL SUMMARY
# Effect sizes alongside significance and a forest plot of univariate odds ratios
# =============================================================================
# -----------------------------------------------------------------------------
# 6.1 Effect sizes for categorical predictors (Cramér's V)
# -----------------------------------------------------------------------------
# Cramér's V ranges 0-1: ~0.1 small, ~0.3 medium, ~0.5 large
get_cramers_v <- function(var, data) {
  tbl <- table(data[[var]], data$No_show_numeric)
  v <- cramer_v(tbl)
  chi <- chisq.test(tbl)
  data.frame(Variable = var, ChiSq = round(chi$statistic, 2),
    p_value = format.pval(chi$p.value, digits = 3),
    CramersV = round(v, 4))
}
categorical_vars <- c("Gender", "Diabetes", "Hipertension", "Alcoholism",
  "Handcap", "Scholarship", "SMS_received")
effect_size_summary <- do.call(rbind, lapply(categorical_vars, get_cramers_v,
  data = data))
effect_size_summary <- effect_size_summary[order(-
    effect_size_summary$CramersV), ]
print(effect_size_summary)
kable(effect_size_summary,
  caption = "Table 6.1: Effect Sizes for Categorical Predictors (Cramér's V)",
  align = "lccc")
# -----------------------------------------------------------------------------
# 6.2 Effect size for Lead Time (odds ratio via univariate logistic regression)
# -----------------------------------------------------------------------------
# Odds ratio per 10-day increase in lead time, for interpretability
data$LeadTime10 <- data$LeadTime / 10
leadtime_model <- glm(No_show_numeric ~ LeadTime10, data = data, family = binomial)
leadtime_or <- exp(coef(leadtime_model)[2])
leadtime_ci <- exp(confint.default(leadtime_model)[2, ])
leadtime_effect <- data.frame(
  Variable = "Lead Time (per 10 days)",
  OddsRatio = round(leadtime_or, 3),
  CI_Lower = round(leadtime_ci[1], 3),
  CI_Upper = round(leadtime_ci[2], 3)
)
print(leadtime_effect)
kable(leadtime_effect,
  caption = "Table 6.2: Effect Size for Lead Time (Univariate Logistic Regression)",
  align = "lccc")

# =============================================================================
# 6.3 Forest plot
# =============================================================================
# ---- Building the univariate odds ratios (one predictor at a time) ----
get_or <- function(var, data) {
  formula <- as.formula(paste("No_show_numeric ~", var))
  model <- glm(formula, data = data, family = binomial)
  est <- exp(coef(model)[2])
  ci <- exp(confint.default(model)[2, ])
  data.frame(Variable = var, OR = est, Lower = ci[1], Upper = ci[2])
}
binary_vars <- c("Scholarship", "Hipertension", "Diabetes", "Alcoholism",
  "SMS_received")
forest_data <- do.call(rbind, lapply(binary_vars, get_or, data = data))
data$LeadTime10 <- data$LeadTime / 10
data$Age10 <- data$Age / 10
data$Gender_Male <- ifelse(data$Gender == "M", 1, 0)
# The code structure for the forest plot was refined using CHAT GPT
#The statistical methodology, variable selection and
# interpretation were the author's own decisions.
forest_data <- rbind(
  forest_data,
  get_or("LeadTime10", data) %>% mutate(Variable = "Lead time (per 10 days)"),
  get_or("Age10", data) %>% mutate(Variable = "Age (per 10 years)"),
  get_or("Gender_Male", data) %>% mutate(Variable = "Gender (Male vs Female)")
)
forest_data <- forest_data %>%
  mutate(Variable = recode(Variable,
    Scholarship = "Welfare enrolled",
    Hipertension = "Hypertension",
    Diabetes = "Diabetes",
    Alcoholism = "Alcoholism",
    SMS_received = "SMS received")) %>%
  arrange(OR)
print(forest_data)
kable(forest_data, caption = "Table 6.3: Univariate Odds Ratios with 95% Confidence Intervals",
  align = "lccc", digits = 3)
# ---- simple forest plot ----
ggplot(forest_data, aes(x = OR, y = reorder(Variable, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = "grey50") +
  geom_point(size = 3.5, color = dissertation_colors["primary"]) +
  scale_x_log10() +
  labs(title = "Univariate Predictors of No-show: Odds Ratios with 95% CI",
    x = "Odds Ratio (Log Scale)", y = NULL) +
  theme_dissertation

# =============================================================================
# 6.4 Resolving the SMS confound: no-show rate by lead-time band, split by SMS
# =============================================================================
sms_within_leadtime <- data %>%
  filter(!is.na(LeadTimeBand)) %>%
  group_by(LeadTimeBand, SMS_received) %>%
  summarise(N = n(), NoShow_Rate = mean(No_show_numeric) * 100, .groups = "drop")
print(sms_within_leadtime, n = Inf)
sms_within_leadtime <- sms_within_leadtime %>%
  mutate(SMS_label = ifelse(SMS_received == 1, "SMS Received", "No SMS"))
ggplot(sms_within_leadtime, aes(x = LeadTimeBand, y = NoShow_Rate,
  color = SMS_label, group = SMS_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("No SMS" = "grey50", "SMS Received" = as.character(dissertation_colors["accent"]))) +
  labs(title = "No-show rate by lead-time band, split by SMS receipt",
    x = "Lead Time Band", y = "No-show Rate (%)", color = NULL) +
  theme_dissertation
