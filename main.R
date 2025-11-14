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
# 2. Merge datasets
# --------------------------

df <- results %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(circuits, by = "circuitId")

df <- df %>%
  select(
    driverRef, constructorRef, name.y, positionOrder,
    year
  ) %>%
  rename(
    driver = driverRef,
    constructor = constructorRef,
    circuit = name.y,
    position = positionOrder
  ) %>%
  filter(position > 0)

# --------------------------
# 3. Create performance embeddings
# --------------------------

# Driver rating
driver_rating <- df %>%
  group_by(driver) %>%
  summarise(driver_perf = mean(position, na.rm = TRUE))

# Constructor rating
constructor_rating <- df %>%
  group_by(constructor) %>%
  summarise(const_perf = mean(position, na.rm = TRUE))

# Circuit rating
circuit_rating <- df %>%
  group_by(circuit) %>%
  summarise(circuit_perf = mean(position, na.rm = TRUE))

# Merge embeddings
df <- df %>%
  left_join(driver_rating, by = "driver") %>%
  left_join(constructor_rating, by = "constructor") %>%
  left_join(circuit_rating, by = "circuit")

# Final training dataset
train_df <- df %>%
  select(position, year, driver_perf, const_perf, circuit_perf)

# --------------------------
# 4. Train Model
# --------------------------

model <- randomForest(
  position ~ year + driver_perf + const_perf + circuit_perf,
  data = train_df,
  ntree = 300,
  importance = TRUE
)

print(model)

# --------------------------
# 5. Prediction Function
# --------------------------

predict_finish <- function(driver, constructor, circuit, year) {
  
  dp <- driver_rating$driver_perf[driver_rating$driver == driver]
  cp <- constructor_rating$const_perf[constructor_rating$constructor == constructor]
  sp <- circuit_rating$circuit_perf[circuit_rating$circuit == circuit]
  
  new_data <- tibble(
    year = year,
    driver_perf = dp,
    const_perf = cp,
    circuit_perf = sp
  )
  
  predict(model, new_data)
}

# Example:
print(predict_finish("hamilton", "mercedes", "silverstone", 2023))

# --------------------------
# 6. Plots
# --------------------------

# Barplot: Driver wins
df %>%
  filter(position == 1) %>%
  count(driver) %>%
  ggplot(aes(x = reorder(driver, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Driver Wins", x = "Driver", y = "Wins")

# Pie chart: Constructor wins
df %>%
  filter(position == 1) %>%
  count(constructor) %>%
  ggplot(aes(x = "", y = n, fill = constructor)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  labs(title = "Constructor Win Share")

predict_finish("hamilton", "mercedes", "silverstone", 2023)
