#*-----------------------------------------------------*
#*Name: Applicant Perception Survey
#*Author:  Dagoberto Estevez-Ordonez, MD
#*Purpose: Applicant Perceptions of Post-Interview Communication during the 2022-2023 Neurosurgery Recruitment Cycle: a survey study
#*-----------------------------------------------------*
# Load required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(DescTools)
library(tibble)
library(scales)
library(forcats)
library(stringr)  # for str_wrap()
library(writexl)

#***Modeled after: https://stackoverflow.com/questions/73621412/ggplot-stacked-barplot-with-uncertainty/76114204#76114204

# Define file paths
coded_data_path <- "/codedDatabase.csv"
labels_data_path <- "/Database.csv"
questionDictionaryPath <- "/QuestionDictionary.csv"

# Check if files exist before reading
if (!file.exists(coded_data_path)) stop("Coded data file does not exist.")
if (!file.exists(labels_data_path)) stop("Labels data file does not exist.")

# Importing CSV files
coded_data <- read.csv(coded_data_path)
labels_data <- read.csv(labels_data_path)
questionDictionary <- read.csv(questionDictionaryPath)

# Remove observations with NA in 'Finished' column
coded_data_no_na <- coded_data[complete.cases(coded_data[, "Finished"]), ]
labels_data_no_na <- labels_data[complete.cases(labels_data[, "Finished"]), ]

# Define columns to drop
columns_to_drop <- c("StartDate", "EndDate", "IPAddress", "Progress", 
                     "Duration..in.seconds.", "RecordedDate", "RecipientLastName",
                     "RecipientFirstName", "RecipientEmail", "ExternalReference",
                     "LocationLatitude", "LocationLongitude", "DistributionChannel", 
                     "UserLanguage", "Q_RecaptchaScore", "Q1_1", "Q2_1", "Q14_1", 
                     "Q20_1", "Q29")

# Define columns to keep
columns_to_keep <- setdiff(names(labels_data_no_na), columns_to_drop)

# Subset only the columns to keep
labels_data_no_na <- subset(labels_data_no_na, select = columns_to_keep)

#creating variable with all variable names
vars <- c("status", "finished", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q15", "q16", "q17", "q18", "q19", "q20", "q21", "q22", "q23", "q24", "q25", "q26", "q27", "q28")

#Changing first word in list above to upper case 
vars <- paste0(toupper(substr(vars, 1, 1)), substr(vars, 2, nchar(vars)))

# Loop over each variable
for (var in vars) {
  
  # Create a temporary data frame to store the current variable's codes and corresponding labels
  temp <- merge(coded_data_no_na[c("ResponseId", var)], labels_data_no_na[c("ResponseId", var)], by = "ResponseId", suffixes = c("_coded", "_label"))
  
  # Check if the merge operation returned any rows
  if (nrow(temp) > 0) {
    
    # Exclude NA values
    temp <- temp[!is.na(temp[[paste0(var, "_coded")]]), ]
    
    # Create a named vector where names are codes and values are labels
    label_mapping <- setNames(as.character(temp[[paste0(var, "_label")]]), temp[[paste0(var, "_coded")]])
    
    # Apply the label mapping to the coded data
    coded_data_no_na[[var]] <- factor(coded_data_no_na[[var]], levels = names(label_mapping), labels = label_mapping)
    
  } else {
    warning(paste("No matching labels found for variable", var))
  }
}

##Now preparing to merge as a QA
#changing labels in preparation for merge 
#Testing code: 
#labels_dataNoNa <- labels_dataNoNa %>% 
#  rename(Q3_label = Q3)

for (var in vars) {
  labels_data_no_na <- labels_data_no_na %>% 
    rename(!!paste0(var, "_label") := !!as.name(var))
}


##Creating vector with corresponding variables with label
label_var <- paste0(vars, "_label")

#Merging Databases
merged_data <- full_join(coded_data_no_na, labels_data_no_na, by = "ResponseId")

##EXAMPLE CODE for Q3
##Wilson interval with continuity correction
#q3 = table(coded_data_no_na$Q3)
#q3_ci <- MultinomCI(q3, conf.level = 0.95, method = "wilson")
#q3_ci <- as.data.frame(q3_ci)
#q3_ci <- cbind(Response = rownames(q3_ci), q3_ci)
#rownames(q3_ci) <- NULL
#q3_ci <- cbind(Question = rep("Q3", nrow(q3_ci)), q3_ci)


#q3_ci$Response <- as.factor(q3_ci$Response)
#q3_ci$Question <- as.factor(q3_ci$Question)
#q3_ci$est <- as.array(q3_ci$est)
#q3_ci$lwr.ci <- as.array(q3_ci$lwr.ci)
#q3_ci$upr.ci <- as.array(q3_ci$upr.ci)

#q3_ci_grouped <- q3_ci %>%
#  group_by(Question, Response)

# Define question list
question_list <- c("Q3", "Q4", paste0("Q", 7:13), "Q15", paste0("Q", 17:28))
question_list2 <- c(paste0("Q", 5:6), "Q16")

# Function to apply based on test code above
#The function generates a variable _ci_grouped for a question that creates a data frame->
 #with confidence intervals applying the MultinomCI function with Wilson intervals to ->
 #questions with more than to categories.This data frame is ready for a figure
calc_ci <- function(question) {
  # Frequency table
  q_table = table(coded_data_no_na[[question]])
  
  # Calculate confidence intervals
  ci <- MultinomCI(q_table, conf.level = 0.95, method = "wilson")
  ci <- as.data.frame(ci)
  
  # Add response and question columns
  ci <- cbind(Response = rownames(ci), ci)
  rownames(ci) <- NULL
  ci <- cbind(Question = rep(question, nrow(ci)), ci)
  
  # Convert data types
  ci$Question <- as.factor(ci$Question)
  ci$est <- as.array(ci$est)
  ci$lwr.ci <- as.array(ci$lwr.ci)
  ci$upr.ci <- as.array(ci$upr.ci)
  
  # Group by Question and Response
  ci_grouped <- ci %>% group_by(Question, Response)
  
  return(ci_grouped)
}

# Apply function to each question in question_list
for (question in question_list) {
  assign(paste0(question, "_ci_grouped"), calc_ci(question), envir = .GlobalEnv)
}

# Apply function to each question in question_list2
for (question in question_list2) {
  assign(paste0(question, "_ci_grouped"), calc_ci(question), envir = .GlobalEnv)
}



###Testing the plots for question 3 only
p <- ggplot(Q3_ci_grouped) +
  geom_bar(aes(x=Response, y=est, fill=Response), stat="identity", color="black") +
  geom_errorbar(aes(x=Response, y=est, ymin=lwr.ci, ymax=upr.ci, color="95% CI"), width=0.3, alpha=0.9, size=0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = "", values = "#828282") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title="Question #3", x="", y="Response [%]") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_blank())

print(p)



# Create a loop for generating plots
#Plots are ordered by response frequency and saved into report folder as png files 600dpi
for (question in question_list) {
  # Get the grouped data frame for the question
  df <- get(paste0(question, "_ci_grouped"))
  
  # Reorder the levels of 'Response' based on 'est'
  df$Response <- with(df, fct_reorder(Response, est))
  
  # Get the question text
  if (question %in% colnames(questionDictionary)) {
    question_text <- questionDictionary[[question]][1]
  } else {
    question_text <- paste("Question", question)
  }
  
  # Wrap the title into multiple lines if it's too long
  question_text <- str_wrap(question_text, width = 80)  # adjust 'width' as needed
  
  # Plotting
  p <- ggplot(df) +
    geom_bar(aes(x=Response, y=est, fill=Response), stat="identity", color="black") +
    geom_errorbar(aes(x=Response, y=est, ymin=lwr.ci, ymax=upr.ci, color="95% CI"), width=0.3, alpha=0.9, size=0.8) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(name = "", values = "#828282") +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title=question_text, x="", y="Response [%]") +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank())
  
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("/Users/dago_estevez/Desktop/Dago/PhD/Clinical Research/Recruitment Survey/reports/", question, ".png"), plot = p, width = 10, height = 7, dpi = 600)
  
}


##Preparing plots for yes or no question: 

##test code for a later loop
# Prepare the data for visualization by setting the x and y values
Q5_calculated <- Q5_ci_grouped %>%
  summarize(x = as.numeric(Question),
            x_adjusted = c(x - 0.25, x - 0.25, x - 0.083, x + 0.083, x + 0.25, x + 0.25),
            y = c(0, est, ifelse(Response == 'Yes', lwr.ci, upr.ci),
                  ifelse(Response == 'Yes', upr.ci, lwr.ci), est, 0))

# Create the stacked area plot.
Q5_plot <- ggplot(Q5_calculated, aes(x_adjusted, y, fill = Response)) + 
  geom_area(position = 'stack', color="black") +
  geom_line(aes(y = 0), color = "black") +  # Add a line at y = 0
  geom_hline(yintercept = ifelse(Q5_ci_grouped$Response == "Yes", Q5_ci_grouped$upr.ci, NA), linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = ifelse(Q5_ci_grouped$Response == "Yes", Q5_ci_grouped$lwr.ci, NA), linetype = "dashed", color = "darkgrey") +
  scale_x_continuous(breaks = 1:3, labels = levels(Q5_ci_grouped$Response)) +
  labs(title="Question #3", x="", y="Response [%]") +
  coord_flip() +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_minimal(base_size = 16)

Q5_plot


# Create a loop for generating calculated data frames
for (question in question_list2) {
  # Get the grouped data frame for the question
  df <- get(paste0(question, "_ci_grouped"))
  
  # Perform the calculation
  calculated_df <- df %>%
    summarize(x = as.numeric(Question),
              x_adjusted = c(x - 0.25, x - 0.25, x - 0.083, x + 0.083, x + 0.25, x + 0.25),
              y = c(0, est, ifelse(Response == 'Yes', lwr.ci, upr.ci),
                    ifelse(Response == 'Yes', upr.ci, lwr.ci), est, 0))
  
  # Assign the calculated data frame to a new variable
  assign(paste0(question, "_calculated"), calculated_df, envir = .GlobalEnv)
}

# Create a loop for generating plots: No percentage
for (question in question_list2) {
  # Get the calculated data frame for the question
  df <- get(paste0(question, "_calculated"))
  
  # Get the grouped data frame for the question
  grouped_df <- get(paste0(question, "_ci_grouped"))
  
  # Get the question text
  if (question %in% colnames(questionDictionary)) {
    question_text <- questionDictionary[[question]][1]
  } else {
    question_text <- paste("Question", question)
  }
  
  # Wrap the title into multiple lines if it's too long
  question_text <- str_wrap(question_text, width = 80)  # adjust 'width' as needed
  
  # Create the stacked area plot
  p <- ggplot(df, aes(x_adjusted, y, fill = Response)) + 
    geom_area(position = 'stack', color="black") +
    geom_line(aes(y = 0), color = "black") +  # Add a line at y = 0
    geom_hline(aes(yintercept = ifelse(Response == "Yes", upr.ci, NA), linetype = "95% CI"), color = "darkgrey", data = grouped_df) +
    geom_hline(aes(yintercept = ifelse(Response == "Yes", lwr.ci, NA)), linetype = "dashed", color = "darkgrey", data = grouped_df) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = 1:length(levels(grouped_df$Response)), labels = levels(grouped_df$Response)) +
    labs(title=question_text, x="", y="Response [%]") +
    coord_flip() +
    scale_fill_brewer(palette = 'Pastel1') +
    theme_minimal(base_size = 16) +
    scale_linetype_manual(name = "", values = c("dashed", "solid"), labels = c("95% CI", "")) +
    theme(legend.position = "bottom")
  
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("/Users/dago_estevez/Desktop/Dago/PhD/Clinical Research/Recruitment Survey/reports/", question, "_plot.png"), plot = p, width = 10, height = 7, dpi = 600)
}

# Create a loop for generating plots: Two percentages displayed
for (question in question_list2) {
  # Get the calculated data frame for the question
  df <- get(paste0(question, "_calculated"))
  
  # Get the grouped data frame for the question
  grouped_df <- get(paste0(question, "_ci_grouped"))
  
  # Generate labels for each question
  labels <- grouped_df %>%
    summarize(x = as.numeric(Question),
              x_adjusted = ifelse(Response == 'Yes', x + 0.083, x - 0.083),
              y = ifelse(Response == 'Yes', est / 2, 1 - est / 2),
              label = sprintf("%.1f%%\n", est * 100))
  
  # Get the question text
  if (question %in% colnames(questionDictionary)) {
    question_text <- questionDictionary[[question]][1]
  } else {
    question_text <- paste("Question", question)
  }
  
  # Wrap the title into multiple lines if it's too long
  question_text <- str_wrap(question_text, width = 80)  # adjust 'width' as needed
  
  # Create the stacked area plot
  p <- ggplot(df, aes(x_adjusted, y, fill = Response)) + 
    geom_area(position = 'stack', color="black") +
    geom_line(aes(y = 0), color = "black") +  # Add a line at y = 0
    geom_hline(aes(yintercept = ifelse(Response == "Yes", upr.ci, NA), linetype = "dashed"), color = "darkgrey", data = grouped_df) +
    geom_hline(aes(yintercept = ifelse(Response == "Yes", lwr.ci, NA), linetype = "dashed"), color = "darkgrey", data = grouped_df) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = 1:length(levels(grouped_df$Response)), labels = levels(grouped_df$Response)) +
    labs(title=question_text, x="", y="Response [%]") +
    coord_flip() +
    scale_fill_brewer(palette = 'Pastel1') +
    theme_minimal(base_size = 16) +
    scale_linetype_manual(name = "", values = c("dashed", "solid"), labels = c("95% CI", "")) +
    theme(legend.position = "bottom") +
    geom_text(data = labels, aes(x = x_adjusted, y = y, label = label), size = 3, hjust = "center", color = "black")  # Add labels
  
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("/Users/dago_estevez/Desktop/Dago/PhD/Clinical Research/Recruitment Survey/reports/", question, "_plot.png"), plot = p, width = 10, height = 7, dpi = 600)
}

##Creating database with CIs (Wilson)
combined_dataframe <- bind_rows(Q3_ci_grouped, Q4_ci_grouped, Q5_ci_grouped, Q6_ci_grouped,
                                Q7_ci_grouped, Q8_ci_grouped, Q9_ci_grouped, Q10_ci_grouped,
                                Q11_ci_grouped, Q12_ci_grouped, Q13_ci_grouped,
                                Q15_ci_grouped, Q16_ci_grouped, Q17_ci_grouped,
                                Q18_ci_grouped, Q19_ci_grouped, Q20_ci_grouped,
                                Q21_ci_grouped, Q22_ci_grouped, Q23_ci_grouped,
                                Q24_ci_grouped, Q25_ci_grouped, Q26_ci_grouped,
                                Q27_ci_grouped, Q28_ci_grouped)

##Exporting to Excel
write_xlsx(combined_dataframe, "/Users/dago_estevez/Desktop/Dago/PhD/Clinical Research/Recruitment Survey/reports/confidenceIntervals.xlsx")


##QA Testing Q5, 6 and 16 with BinomCI
#Q5
table(coded_data_no_na$Q5)
BinomCI(64, (64+12), conf.level = 0.95, method = c("wilson"))
BinomCI(12, (64+12), conf.level = 0.95, method = c("wilson"))

#Q6
table(coded_data_no_na$Q6)
BinomCI(44, (44+32), conf.level = 0.95, method = c("wilson"))
BinomCI(32, (44+32), conf.level = 0.95, method = c("wilson"))

#Q16
table(coded_data_no_na$Q16)
BinomCI(67, (67+5), conf.level = 0.95, method = c("wilson"))
BinomCI(5, (67+5), conf.level = 0.95, method = c("wilson"))

