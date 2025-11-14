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
# Helper: pick first existing column name from a list
# --------------------------
pick_col <- function(df, candidates){
  which_exist <- candidates[candidates %in% names(df)]
  if(length(which_exist) == 0) return(NA_character_)
  return(which_exist[1])
}

# --------------------------
# 2. Merge datasets (robust selection of columns)
# --------------------------
df_full <- results %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(circuits, by = "circuitId")

# Likely column names in Kaggle data:
# drivers: driverRef
# constructors: constructorRef or name
# circuits: name
# races: year, round, raceName

# Find the right columns robustly
driver_ref_col <- pick_col(df_full, c("driverRef", "driver_ref", "driver"))
constructor_ref_col <- pick_col(df_full, c("constructorRef", "constructor_ref", "constructor", "name.x", "name"))
circuit_name_col <- pick_col(df_full, c("name.y", "name", "circuitRef", "circuit_name", "raceName"))
position_col <- pick_col(df_full, c("positionOrder", "position", "positionText"))
year_col <- pick_col(df_full, c("year"))

# Basic check
if(is.na(driver_ref_col) || is.na(constructor_ref_col) || is.na(circuit_name_col) || is.na(position_col) || is.na(year_col)){
  stop("Couldn't find expected columns. Available columns: ", paste(names(df_full), collapse = ", "))
}

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
# 3. Create performance embeddings (numeric ratings)
# --------------------------
driver_rating <- df %>%
  group_by(driver) %>%
  summarise(driver_perf = mean(position, na.rm = TRUE), .groups = "drop")

constructor_rating <- df %>%
  group_by(constructor) %>%
  summarise(const_perf = mean(position, na.rm = TRUE), .groups = "drop")

circuit_rating <- df %>%
  group_by(circuit) %>%
  summarise(circuit_perf = mean(position, na.rm = TRUE), .groups = "drop")

# Merge embeddings back to df
df <- df %>%
  left_join(driver_rating, by = "driver") %>%
  left_join(constructor_rating, by = "constructor") %>%
  left_join(circuit_rating, by = "circuit")

# Final training dataset
train_df <- df %>%
  select(position, year, driver_perf, const_perf, circuit_perf) %>%
  filter(!is.na(driver_perf) & !is.na(const_perf) & !is.na(circuit_perf))

# --------------------------
# 4. Train Model
# --------------------------
set.seed(42)
model <- randomForest(
  position ~ year + driver_perf + const_perf + circuit_perf,
  data = train_df,
  ntree = 300,
  importance = TRUE
)
print(model)

# --------------------------
# Helper: flexible name matcher
# --------------------------
find_match <- function(query, choices){
  if(is.null(query) || is.na(query) || query == "") return(NA_character_)
  q <- tolower(trimws(query))
  choices_low <- tolower(choices)
  
  # 1) exact
  exact <- choices[choices_low == q]
  if(length(exact)) return(exact[1])
  
  # 2) substring anywhere
  substr_match <- choices[grepl(q, choices_low, fixed = TRUE)]
  if(length(substr_match)) return(substr_match[1])
  
  # 3) startsWith
  starts <- choices[startsWith(choices_low, q)]
  if(length(starts)) return(starts[1])
  
  # 4) fuzzy agrep (small distance)
  ag <- agrep(q, choices_low, max.distance = 0.2, value = TRUE)
  if(length(ag)) return(ag[1])
  
  return(NA_character_)
}

# --------------------------
# 5. Robust prediction function
# --------------------------
predict_finish <- function(driver_in, constructor_in, circuit_in, year_in){
  # find actual names in dataset
  driver_match <- find_match(driver_in, driver_rating$driver)
  constructor_match <- find_match(constructor_in, constructor_rating$constructor)
  circuit_match <- find_match(circuit_in, circuit_rating$circuit)
  
  # If not found, show helpful messages and some suggestions
  if(is.na(driver_match)){
    cat("Driver not found for: '", driver_in, "'.\n", sep = "")
    cat("Try one of these (top 12):\n")
    print(head(driver_rating$driver, 12))
    return(invisible(NULL))
  }
  if(is.na(constructor_match)){
    cat("Constructor not found for: '", constructor_in, "'.\n", sep = "")
    cat("Try one of these (top 12):\n")
    print(head(constructor_rating$constructor, 12))
    return(invisible(NULL))
  }
  if(is.na(circuit_match)){
    cat("Circuit not found for: '", circuit_in, "'.\n", sep = "")
    cat("Try one of these (top 12):\n")
    print(head(circuit_rating$circuit, 12))
    return(invisible(NULL))
  }
  
  # Get embedding values (fall back to overall mean if missing)
  dp <- driver_rating$driver_perf[driver_rating$driver == driver_match]
  if(length(dp) == 0 || is.na(dp)) dp <- mean(driver_rating$driver_perf, na.rm = TRUE)
  
  cp <- constructor_rating$const_perf[constructor_rating$constructor == constructor_match]
  if(length(cp) == 0 || is.na(cp)) cp <- mean(constructor_rating$const_perf, na.rm = TRUE)
  
  sp <- circuit_rating$circuit_perf[circuit_rating$circuit == circuit_match]
  if(length(sp) == 0 || is.na(sp)) sp <- mean(circuit_rating$circuit_perf, na.rm = TRUE)
  
  # Build newdata and predict
  new_data <- tibble(
    year = as.integer(year_in),
    driver_perf = as.numeric(dp),
    const_perf = as.numeric(cp),
    circuit_perf = as.numeric(sp)
  )
  
  pred_raw <- predict(model, newdata = new_data)
  pred_round <- pmax(1, round(pred_raw))
  
  # Nicely formatted output
  out <- list(
    driver_input = driver_in,
    driver_match = driver_match,
    constructor_input = constructor_in,
    constructor_match = constructor_match,
    circuit_input = circuit_in,
    circuit_match = circuit_match,
    year = year_in,
    predicted_position_raw = as.numeric(pred_raw),
    predicted_position = as.integer(pred_round)
  )
  
  cat(sprintf("Prediction for %s (%s) at %s in %d:\n", driver_match, driver_in, circuit_match, year_in))
  cat(sprintf(" → Predicted finishing position (rounded): P%d\n", out$predicted_position))
  cat(sprintf(" → Raw predicted position (decimal): %.3f\n", out$predicted_position_raw))
  
  return(invisible(out))
}

# --------------------------
# 6. Plots (display)
# --------------------------
# Barplot: Driver wins (top 20)
p1 <- df %>%
  filter(position == 1) %>%
  count(driver) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(driver, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Driver Wins", x = "Driver", y = "Wins")

print(p1)

# Pie chart: Constructor win share (top 10, grouped "Others")
constructor_wins <- df %>%
  filter(position == 1) %>%
  count(constructor, name = "wins") %>%
  arrange(desc(wins)) %>%
  mutate(constructor = as.character(constructor))

top10 <- constructor_wins %>% slice_head(n = 10)
others <- constructor_wins %>% slice(-(1:10)) %>% summarise(constructor = "Others", wins = sum(wins))
pie_df <- bind_rows(top10, others)

p2 <- ggplot(pie_df, aes(x = "", y = wins, fill = constructor)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  labs(title = "Constructor Win Share (Top 10 + Others)")

print(p2)

# --------------------------
# Example: flexible short-name usage
# --------------------------
# Now this will work with short/lowercase inputs:
# predict_finish("hamilton", "mercedes", "silverstone", 2023)
