df <- read.csv('/Users/emmanuelmensah/Desktop/Course Materials/Second Year/Fall Semester/Intro to IR/Dataset/GTD/cleaned_dataset.csv')
library(dplyr)
head(df)
str(df)

######################### Cleaning My Data to Prepare them for Model

# Recoding 'onset' from categorical (unssuccessful, successful, no onset) into a binary variable (onset and no onset)
df$onset_binary <- ifelse(df$onset == 0, 0, 1)
######################## Converting all categorical and binary data as factor
df$regime_type <- as.factor(df$regime_type)
df$con_onset <- as.factor(df$con_onset)
df$first_attack <- as.factor(df$first_attack)

#Renaming the onset variable to terrorism onset for clarity
df <- df %>%
  rename(terrorism_onset = onset_binary)
# Converting the renamed variable to a factor
df$terrorism_onset <- as.factor(df$terrorism_onset)
str(df)
head(df)
#####Further Cleaning of my data ########################
# Example for fixing terrorism_onset
df$terrorism_onset <- factor(df$terrorism_onset, levels = c(0, 1), labels = c("0", "1"))
# Fix con_onset levels
df$con_onset <- factor(df$con_onset, levels = c(0, 1), labels = c("0", "1"))
# Example if regime_type has numerical values, convert to factors with specific labels
df$regime_type <- factor(df$regime_type, levels = c(0, 1, 2, 3), labels = c("Closed Autocracy", "Elec. Autocracy", "Elec. Democracy", "Lib Democracy"))
# Fix first_attack levels
df$first_attack <- factor(df$first_attack, levels = c(0, 1), labels = c("0", "1"))


# Checking levels for specific variables
levels(df$con_onset)
levels(df$regime_type)
levels(df$first_attack)
levels(df$terrorism_onset)

##########Converting dataset into panel. This was not used due to error I kept receiving for
# working with panel data structure with country and year as factors ############
#library(plm)
#df_panel <- pdata.frame(df, index = c("country", "year"))
#summary(df_panel)

#######################Generating Summary Statistics for Variables to be used in the Model ######
library(dplyr)
library(knitr)
library(kableExtra)

# Selecting the variables of interest
selected_vars <- df %>% select(con_onset, regime_type, gdp_per_capita, hdi, unemp_rate, terrorism_onset)

# Function to generate summary statistics
summary_stats <- function(x) {
  if (is.factor(x)) {
    # For categorical variables
    freq_table <- table(x)
    prop_table <- prop.table(freq_table)
    data.frame(
      Variable = class(x),
      n = length(x),
      Mean = NA,
      SD = NA,
      Min = NA,
      Max = NA,
      Categories = paste(names(freq_table), collapse = ", "),
      Frequencies = paste(freq_table, collapse = ", "),
      Proportions = paste(round(prop_table, 2), collapse = ", ")
    )
  } else {
    # For numeric variables
    data.frame(
      Variable = class(x),
      n = length(x),
      Mean = round(mean(x, na.rm = TRUE), 2),
      SD = round(sd(x, na.rm = TRUE), 2),
      Min = round(min(x, na.rm = TRUE), 2),
      Max = round(max(x, na.rm = TRUE), 2),
      Categories = NA,
      Frequencies = NA,
      Proportions = NA
    )
  }
}

# Applying the summary function to each variable
summary_list <- lapply(selected_vars, summary_stats)

# Combining the results into a single data frame
summary_df <- do.call(rbind, summary_list)
rownames(summary_df) <- names(selected_vars)

# Generating HTML table
kable(summary_df, format = "html", caption = "Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  footnote(general = "For categorical variables: n, categories, frequencies, and proportions are reported. For numeric variables: n, mean, standard deviation, minimum, and maximum are reported. NA indicates not applicable.")



#####Running Logistic Regression not as panel data but as data frame without country and year dummies to test my model
l_model1 <- glm(terrorism_onset ~ regime_type + gdp_per_capita + hdi + unemp_rate, 
                 family = binomial(link = "logit"), data = df)


# Generate predicted probabilities for regime_type
regime_effect <- ggpredict(l_model1, terms = "regime_type")

# Dot-and-whisker plot for predicted probabilities
library(ggplot2)
plot_regime_effect_dot <- ggplot(regime_effect, aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "black") +  # Plot the predicted probabilities as dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Predicted Probability of Terrorism Onset by Regime Type",
    x = "Regime Type",
    y = "Predicted Probability of Terrorism Onset"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot_regime_effect_dot)



####Another Logistic Regression with conflitc variable included: 
l_model2 <- glm(terrorism_onset ~ regime_type + con_onset + gdp_per_capita + hdi + unemp_rate, 
                family = binomial(link = "logit"), data = df)

regime_effect1 <- ggpredict(l_model1, terms = "regime_type")
print(regime_effect1)

# Dot-and-whisker plot for predicted probabilities
library(ggplot2)
plot_regime_effect_dot1 <- ggplot(regime_effect1, aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "black") +  # Plot the predicted probabilities as dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Predicted Probability of Terrorism Onset by Regime Type",
    x = "Regime Type",
    y = "Predicted Probability of Terrorism Onset"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot_regime_effect_dot1)



##########Creating Country-Year Dummies Using One_Hot Encoding; 
# One-hot encoding with model.matrix()
df_onehot <- model.matrix(~ country + year - 1, data = df)  # -1 to remove intercept
df_onehot <- as.data.frame(df_onehot)
df_panel_encoded <- cbind(df, df_onehot)

# View the first few rows and columns of the updated dataset
head(df_onehot[, 1:20])
                        

################Generating a Logistic Model with Country and Year Dummies
fe_model_onehot <- glm(terrorism_onset ~ regime_type + gdp_per_capita + hdi + unemp_rate + 
                   country + year, 
                 family = binomial(link = "logit"), data = df_onehot)

summary(fe_model_onehot)
# Get predicted probabilities from the fixed effects logistic regression model
# Generate predicted probabilities for the 'regime_type' variable
regime_effect_onehot <- ggpredict(fe_model_onehot, terms = "regime_type")

library(ggeffects)

regime_effect_onehot <- ggpredict(model, terms = "regime_type", type = "fixed")

levels(df$country)
levels(model$xlevels$country)




# Print the predicted probabilities and their confidence intervals
print(regime_effect_onehot)

###########Visualizing results for the on_hot_encoded fixed effects; 
plot_regime_effect_onehot <- ggplot(regime_effect_onehot, aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "black") +  # Plot the predicted probabilities as dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Predicted Probability of Terrorism Onset by Regime Type",
    x = "Regime Type",
    y = "Predicted Probability of Terrorism Onset"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot_regime_effect_onehot)



########Running Another model including conflict_onset and excluding unemployment due to colinearity

fe_model_onehot2 <- glm(terrorism_onset ~ regime_type + gdp_per_capita + hdi + con_onset + 
                         country + year, 
                       family = binomial(link = "logit"), data = df_onehot)


# Get predicted probabilities from the fixed effects logistic regression model
# Generate predicted probabilities for the 'regime_type' variable
regime_effect_onehot2 <- ggpredict(fe_model_onehot2, terms = "regime_type")

# Print the predicted probabilities and their confidence intervals
print(regime_effect_onehot2)

###########Visualizing results for the on_hot_encoded fixed effects; 
plot_regime_effect_onehot2 <- ggplot(regime_effect_onehot2, aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "black") +  # Plot the predicted probabilities as dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Predicted Probability of Terrorism Onset by Regime Type",
    x = "Regime Type",
    y = "Predicted Probability of Terrorism Onset"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot_regime_effect_onehot2)

########################3rd Model: Including All Variables
fe_model_onehot3 <- glm(terrorism_onset ~ regime_type + gdp_per_capita + hdi + con_onset + unemp_rate +
                          country + year, 
                        family = binomial(link = "logit"), data = df_panel_encoded)


# Get predicted probabilities from the fixed effects logistic regression model
# Generate predicted probabilities for the 'regime_type' variable
regime_effect_onehot3 <- ggpredict(fe_model_onehot3, terms = "regime_type")

# Print the predicted probabilities and their confidence intervals
print(regime_effect_onehot3)

###########Visualizing results for the on_hot_encoded fixed effects; 
plot_regime_effect_onehot3 <- ggplot(regime_effect_onehot3, aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "black") +  # Plot the predicted probabilities as dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Predicted Probability of Terrorism Onset by Regime Type",
    x = "Regime Type",
    y = "Predicted Probability of Terrorism Onset"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot_regime_effect_onehot3)


################### 4th Model with only 3 predictors: 
fe_model_onehot4 <- glm(terrorism_onset ~ regime_type + gdp_per_capita + hdi +
                          country + year, 
                        family = binomial(link = "logit"), data = df_panel_encoded)


# Get predicted probabilities from the fixed effects logistic regression model
# Generate predicted probabilities for the 'regime_type' variable
regime_effect_onehot4 <- ggpredict(fe_model_onehot4, terms = "regime_type")

# Print the predicted probabilities and their confidence intervals
print(regime_effect_onehot4)

###########Visualizing results for the on_hot_encoded fixed effects; 
plot_regime_effect_onehot4 <- ggplot(regime_effect_onehot4, aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "black") +  # Plot the predicted probabilities as dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Predicted Probability of Terrorism Onset by Regime Type",
    x = "Regime Type",
    y = "Predicted Probability of Terrorism Onset"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(plot_regime_effect_onehot4)


###################Model Comparison ###################
library(stargazer)

stargazer(
  fe_model_onehot, fe_model_onehot2, fe_model_onehot3, fe_model_onehot4,
  type = "text",  # Change to "html" or "latex" for other formats
  title = "Comparison of Logistic Regression Models",
  dep.var.labels = "Terrorism Onset",
  covariate.labels = c("Regime Type (Electoral Autocracy)", 
                       "Regime Type (Electoral Democracy)", 
                       "Regime Type (Liberal Democracy)", 
                       "GDP Per Capita", "HDI", "Unemployment Rate"),
  out = "model_comparison_table.html"  # Save as a file if needed
)

# Generating the stargazer table while omitting country and year dummies
stargazer(
  fe_model_onehot, fe_model_onehot2, fe_model_onehot3, fe_model_onehot4,
  type = "text",  # Change to "html" or "latex" for other formats
  title = "Comparison of Logistic Regression Models",
  dep.var.labels = "Terrorism Onset",
  covariate.labels = c("Regime Type (Electoral Autocracy)", 
                       "Regime Type (Electoral Democracy)", 
                       "Regime Type (Liberal Democracy)", 
                       "GDP Per Capita", "HDI", "Unemployment Rate"),
  omit = c("country", "year"),  # Exclude variables containing "country" or "year"
  omit.labels = c("Country Dummies", "Year Dummies"),  # Add a note for omitted variables
  out = "model_comparison_table.html"  # Save as a file if needed
)
library(stargazer)

# Generating the stargazer table while omitting country and year dummies
stargazer(
  fe_model_onehot, fe_model_onehot2, fe_model_onehot3, fe_model_onehot4,
  type = "latex",  # Change to "html" or "latex" for other formats
  title = "Comparison of Logistic Regression Models",
  dep.var.labels = "Terrorism Onset",
  covariate.labels = c("Regime Type (Electoral Autocracy)", 
                       "Regime Type (Electoral Democracy)", 
                       "Regime Type (Liberal Democracy)", 
                       "GDP Per Capita", "HDI", "Unemployment Rate"),
  omit = c("country", "year"),  # Exclude variables containing "country" or "year"
  omit.labels = c("Country Dummies", "Year Dummies"),  # Add a note for omitted variables
  out = "model_comparison_table.tex"  # Save as a file if needed
)


print(df_panel_encoded)

#########Model Comparison #############
AIC(fe_model_onehot, fe_model_onehot2, fe_model_onehot3, fe_model_onehot4)
BIC(fe_model_onehot, fe_model_onehot2, fe_model_onehot3, fe_model_onehot4)

library(knitr)

##Model comparison statistics
comparison <- data.frame(
  Model = c("Model 1 (fe_model_onehot)", 
            "Model 2 (fe_model_onehot2)", 
            "Model 3 (fe_model_onehot3)", 
            "Model 4 (fe_model_onehot4)"),
  AIC = c(AIC(fe_model_onehot), 
          AIC(fe_model_onehot2), 
          AIC(fe_model_onehot3), 
          AIC(fe_model_onehot4)),
  BIC = c(BIC(fe_model_onehot), 
          BIC(fe_model_onehot2), 
          BIC(fe_model_onehot3), 
          BIC(fe_model_onehot4)),
  LogLikelihood = c(logLik(fe_model_onehot), 
                    logLik(fe_model_onehot2), 
                    logLik(fe_model_onehot3), 
                    logLik(fe_model_onehot4)),
  df = c(attr(logLik(fe_model_onehot), "df"),
         attr(logLik(fe_model_onehot2), "df"),
         attr(logLik(fe_model_onehot3), "df"),
         attr(logLik(fe_model_onehot4), "df"))
)

# Visualising Scores for East Comparison
kable(comparison, 
      caption = "Model Comparison Statistics", 
      format = "html")  # Change to "latex" for LaTeX output


############## Performing Pseudo-R Squared ############
# Function to calculate McFadden's Pseudo R-squared
calculate_mcfadden_r2 <- function(model) {
  log_lik_model <- logLik(model)
  log_lik_null <- logLik(update(model, . ~ 1))  # Null model with no predictors
  r2 <- 1 - (log_lik_model / log_lik_null)
  return(r2)
}

# Calculating McFadden's Pseudo R-squared for each model
r2_model1 <- calculate_mcfadden_r2(fe_model_onehot)
r2_model2 <- calculate_mcfadden_r2(fe_model_onehot2)
r2_model3 <- calculate_mcfadden_r2(fe_model_onehot3)
r2_model4 <- calculate_mcfadden_r2(fe_model_onehot4)

# Store results in a data frame for comparison
r2_comparison <- data.frame(
  Model = c("Model 1 (fe_model_onehot)", 
            "Model 2 (fe_model_onehot2)", 
            "Model 3 (fe_model_onehot3)", 
            "Model 4 (fe_model_onehot4)"),
  McFadden_R2 = c(r2_model1, r2_model2, r2_model3, r2_model4)
)

# Display the comparison
print(r2_comparison)

###########Checking for Multicolinearity between predictors of Model 2
library(car)
vif_values <- vif(fe_model_onehot2)
print(vif_values)






