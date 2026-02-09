# Real-Time Analytics & Visualization Dashboard (R Shiny)

An interactive **R Shiny–based analytics dashboard** that enables users to upload datasets, explore data visually, compute summary statistics, analyze correlations, export results, and monitor live stock data—all through an intuitive web interface.

---

##  Key Features

*  **Data Upload & Preview**

    * Upload CSV files
    * Custom delimiter & quote options
    * Adjustable row preview

*  **Interactive Visualizations**

    * Scatter plots, histograms, and distributions
    * Dynamic X/Y variable selection
    * Color mapping & themes

*  **Analytics**

    * Summary statistics
    * Correlation matrix heatmap
    * Data distribution analysis

*  **Live Stock Data**

    * Real-time stock price visualization
    * Multiple stock symbol selection
    * Time-interval based analysis

*  **Export Options**

    * Download processed data as CSV
    * Export plots (PNG format)
    * Custom plot width & height

---

##  Tech Stack

* **Language:** R
* **Framework:** Shiny
* **Libraries:**

  * `shiny`
  * `ggplot2`
  * `DT`
  * `corrplot`
  * `quantmod`
  * `dplyr`, `tidyr`


##  Installation & Setup

### 1️ Install R & RStudio

Download from: [https://posit.co/download/rstudio/](https://posit.co/download/rstudio/)

### 2️ Install Required Packages

```r
install.packages(c(
  "shiny", "ggplot2", "DT",
  "corrplot", "quantmod",
  "dplyr", "tidyr"
))
```

### 3️ Run the Application

```r
shiny::runApp("app.R")
```

The app will open in your browser at:

```
http://127.0.0.1:xxxx
```

---

##  Objective

To provide a **unified, interactive analytics platform** that simplifies exploratory data analysis, visualization, and real-time insights for students, analysts, and data science learners.

---
