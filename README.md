# 🏎️ F1 Race Predictor

A machine learning project that predicts Formula 1 race finishing positions using historical F1 data and Random Forest algorithm.

## 📋 Overview

This project analyzes historical Formula 1 racing data to predict race outcomes based on driver performance, constructor (team) performance, and circuit characteristics. The model uses performance embeddings and a Random Forest classifier to make predictions.

## ✨ Features

- **Machine Learning Model**: Random Forest algorithm with 300 decision trees
- **Performance Embeddings**: Custom rating system for drivers, constructors, and circuits
- **Historical Data Analysis**: Processes data from multiple F1 seasons
- **Prediction Function**: Easy-to-use function to predict finishing positions
- **Data Visualizations**: 
  - Driver wins bar chart
  - Constructor win share pie chart

## 📊 Dataset

The project uses F1 historical data from CSV files:
- `results.csv` - Race results data
- `races.csv` - Race information
- `drivers.csv` - Driver details
- `constructors.csv` - Team/constructor information
- `circuits.csv` - Circuit details

## 🛠️ Requirements

- R (version 3.6 or higher)
- Required R packages:
  ```r
  install.packages("tidyverse")
  install.packages("randomForest")
  install.packages("lubridate")
  ```

## 🚀 Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/Bala-Arjun/F1-Race-Predictor.git
   cd F1-Race-Predictor
   ```

2. Install required R packages:
   ```r
   install.packages(c("tidyverse", "randomForest", "lubridate"))
   ```

3. Ensure your data files are in the `data/` directory

## 💻 Usage

### Running the Model

```r
# Load the script
source("main.R")

# Make a prediction
predict_finish("hamilton", "mercedes", "silverstone", 2023)
```

### Prediction Function Parameters

- `driver`: Driver reference name (e.g., "hamilton", "verstappen")
- `constructor`: Team/constructor name (e.g., "mercedes", "red_bull")
- `circuit`: Circuit name (e.g., "silverstone", "monza")
- `year`: Year of the race

### Example Predictions

```r
# Predict Lewis Hamilton's finish at Silverstone with Mercedes
predict_finish("hamilton", "mercedes", "silverstone", 2023)

# Predict Max Verstappen's finish at Monza with Red Bull
predict_finish("verstappen", "red_bull", "monza", 2023)
```

## 🧠 Model Details

The prediction model works in the following steps:

1. **Data Merging**: Combines results, races, drivers, constructors, and circuits data
2. **Feature Engineering**: Creates performance embeddings:
   - Driver performance rating (average finishing position)
   - Constructor performance rating
   - Circuit performance rating
3. **Model Training**: Uses Random Forest with:
   - 300 decision trees
   - Features: year, driver performance, constructor performance, circuit performance
   - Target: finishing position
4. **Prediction**: Generates finishing position predictions based on input parameters

## 📈 Visualizations

The project generates two key visualizations:

1. **Driver Wins Bar Chart**: Shows the number of wins for each driver
2. **Constructor Win Share Pie Chart**: Displays the distribution of wins among constructors

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes:

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License

This project is open source and available under the [MIT License](LICENSE).

## 🙏 Acknowledgments

- Formula 1 historical data
- R community for excellent data science packages
- Random Forest algorithm developers

## 📧 Contact

For questions or feedback, please open an issue on GitHub.

---

**Note**: This is an educational project for learning machine learning and data analysis techniques.
