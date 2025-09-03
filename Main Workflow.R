# CHINA VS USA GENE PRESENCE/ABSENCE PREDICTION --------------------------
source("utils.R")
source("model_train_test.R")

set.seed(123)
output_dir <- here::here("Core_Accessory_Results_China_USA")
dir.create(output_dir, showWarnings = FALSE)

two_countries <- c("China","USA")

# Load + preprocess
data <- load_data(two_countries)
processed_data <- preprocess_data(data)
clean2orig <- setNames(levels(processed_data$country), levels(processed_data$country_clean))

# Stratified split: 55% per country
train_data <- processed_data %>%
  group_by(country_clean) %>%
  slice_sample(prop = 0.55) %>%
  ungroup()

test_data <- processed_data %>%
  filter(!Isolate %in% train_data$Isolate)

train_data$country <- factor(train_data$country, levels = two_countries)
test_data$country  <- factor(test_data$country, levels = two_countries)

# Train + evaluate
model <- train_model(train_data)
results <- evaluate_model(model, test_data, clean2orig, output_dir)

# Save outputs
save_outputs(results, processed_data, train_data, test_data, two_countries, output_dir)

message("Analysis complete! Results saved to: ", normalizePath(output_dir))