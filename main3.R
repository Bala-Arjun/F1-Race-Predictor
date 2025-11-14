library(tidyverse)
library(randomForest)
library(lubridate)

# --------------------------
# 1. Load CSV files
# --------------------------
results <- read_csv("data/results.csv")
races <- read_csv("data/races.csv")
drivers <- read_csv("data/drivers.csv")
constructors <- read_csv("data/constructors.csv")
circuits <- read_csv("data/circuits.csv")

# --------------------------
# Helper: pick first existing column from candidates
# --------------------------
pick_col <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

# --------------------------
# 2. Merge datasets
# --------------------------
df_full <- results %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(circuits, by = "circuitId")

# Identify correct columns
driver_ref_col <- pick_col(df_full, c("driverRef"))
constructor_ref_col <- pick_col(df_full, c("constructorRef"))
circuit_name_col <- pick_col(df_full, c(
  "circuitName",      # Kaggle
  "name.y",           # usually circuits after merge
  "name",             # fallback
  "circuitRef"
))
position_col <- pick_col(df_full, c("positionOrder", "position"))
year_col <- pick_col(df_full, c("year"))

# Build cleaned df
df <- df_full %>%
  mutate(
    driver = .data[[driver_ref_col]],
    constructor = .data[[constructor_ref_col]],
    circuit = .data[[circuit_name_col]],
    position = as.numeric(.data[[position_col]]),
    year = as.integer(.data[[year_col]])
  ) %>%
  select(driver, constructor, circuit, position, year) %>%
  filter(!is.na(position) & position > 0)

# --------------------------
# Debug: show circuits found
# --------------------------
cat("Circuits detected in dataset:\n")
print(head(unique(df$circuit), 20))

# --------------------------
# 3. Performance embeddings
# --------------------------
driver_rating <- df %>%
  group_by(driver) %>%
  summarise(driver_perf = mean(position), .groups = "drop")

constructor_rating <- df %>%
  group_by(constructor) %>%
  summarise(const_perf = mean(position), .groups = "drop")

circuit_rating <- df %>%
  group_by(circuit) %>%
  summarise(circuit_perf = mean(position), .groups = "drop")

df <- df %>%
  left_join(driver_rating, by = "driver") %>%
  left_join(constructor_rating, by = "constructor") %>%
  left_join(circuit_rating, by = "circuit")

train_df <- df %>%
  select(position, year, driver_perf, const_perf, circuit_perf) %>%
  filter(!is.na(driver_perf) & !is.na(const_perf) & !is.na(circuit_perf))

# --------------------------
# 4. Train model
# --------------------------
set.seed(42)
model <- randomForest(
  position ~ year + driver_perf + const_perf + circuit_perf,
  data = train_df,
  ntree = 300
)

print(model)

# --------------------------
# Name matching helper
# --------------------------
find_match <- function(query, choices) {
  q <- tolower(query)
  choices_low <- tolower(choices)
  
  # exact
  exact <- choices[choices_low == q]
  if (length(exact)) return(exact[1])
  
  # contains
  cont <- choices[grepl(q, choices_low, fixed = TRUE)]
  if (length(cont)) return(cont[1])
  
  # startsWith
  start <- choices[startsWith(choices_low, q)]
  if (length(start)) return(start[1])
  
  # fuzzy
  fuzzy <- agrep(q, choices_low, max.distance = 0.2, value = TRUE)
  if (length(fuzzy)) return(fuzzy[1])
  
  return(NA_character_)
}

# --------------------------
# 5. Prediction function
# --------------------------
predict_finish <- function(driver_in, constructor_in, circuit_in, year_in) {
  
  driver_match <- find_match(driver_in, driver_rating$driver)
  constructor_match <- find_match(constructor_in, constructor_rating$constructor)
  circuit_match <- find_match(circuit_in, circuit_rating$circuit)
  
  if (is.na(driver_match)) {
    cat("Driver not found:", driver_in, "\nTry one of:\n")
    print(head(driver_rating$driver, 15))
    return(NULL)
  }
  
  if (is.na(constructor_match)) {
    cat("Constructor not found:", constructor_in, "\nTry one of:\n")
    print(head(constructor_rating$constructor, 15))
    return(NULL)
  }
  
  if (is.na(circuit_match)) {
    cat("Circuit not found:", circuit_in, "\nTry one of:\n")
    print(head(circuit_rating$circuit, 15))
    return(NULL)
  }
  
  dp <- driver_rating$driver_perf[driver_rating$driver == driver_match]
  cp <- constructor_rating$const_perf[constructor_rating$constructor == constructor_match]
  sp <- circuit_rating$circuit_perf[circuit_rating$circuit == circuit_match]
  
  new_data <- tibble(
    year = year_in,
    driver_perf = dp,
    const_perf = cp,
    circuit_perf = sp
  )
  
  pred <- predict(model, new_data)
  pred_round <- max(1, round(pred))
  
  cat("\n===== Prediction Result =====\n")
  cat("Driver:", driver_match, "\n")
  cat("Constructor:", constructor_match, "\n")
  cat("Circuit:", circuit_match, "\n")
  cat("Year:", year_in, "\n")
  cat("----------------------------\n")
  cat("Predicted Position:", paste0("P", pred_round), "\n")
  cat("Raw model output:", pred, "\n")
  cat("============================\n\n")
  
  return(pred_round)
}

# --------------------------
# Example
# --------------------------
# predict_finish("max", "red bull", "monza", 2022)
