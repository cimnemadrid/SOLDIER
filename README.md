# SOLDIER: SOLution for Dam Behavior Interpretation and Safety Evaluation

SOLDIER is an interactive application based on R-Shiny that facilitates the generation of machine-learning-based predictive models, their accuracy evaluation, and the analysis of predictor variables' effect on dam behavior. This tool is designed to aid dam engineers in identifying changes in dam response, detecting potential anomalies, and gaining a better understanding of the effect of loads on the dam structure.

SOLDIER aims to facilitate the application and interpretation of machine learning models for users who are not specialized in them. Although it is focused on the dam engineering sector their functionalities can be extended to structural health monitoring for other civil infrastructures and other engineering problems.

## Table of Contents
<!-- Add a table of contents if the README is lengthy -->

1. [Introduction](#introduction)
2. [Features](#features)
3. [Installation and Launch Guide](#installation-and-launch-guide)
4. [License](#license)

## Introduction

The behavior and safety evaluation of dams are crucial aspects of dam engineering. SOLDIER, which stands for SOLution for Dam behavior Interpretation and safety Evaluation with boosted Regression trees, is an application that allows engineers to easily harness the power of machine learning in the dam engineering domain. This interactive tool is built using R-Shiny and offers the capability to create predictive models, assess their accuracy, and gain valuable insights into the influence of predictor variables on the dam's response.

## Features

The application is structured into three main sections:

1. **Data Exploration:**
   - Load data from RDS or XLSX files.
   - Visualize data in different formats: scatterplots and time series.

3. **Model Fitting:**
   - Select target and predictor variables from the loaded data.
   - Control training and testing data split.
   - Generate predictive models using the Boosted Regression Trees (BRT) algorithm.
   - Assess model accuracy: Mean Absolute Error (MAE) and R-squared (R2) metrics.
   - Interactive graphical representation of predictions and observations.
   - Download fitted models in RDS format for integration or further analysis.

5. **Interpretation:**
   - Analyze the relative importance of variables in the fitted models.
   - Group variables and analyze their combined influence on the response.
   - Explore the partial dependence of the target variable on selected predictors.
   - Download variable importance and partial dependence results in CSV format.

## Installation and Launch Guide

SOLDIER operates just like any other R-Shiny application. To run it on your local machine, consider following these recommended steps:

1. **Clone the Repository:**
   ```
   git clone https://github.com/your-username/soldier.git
   ```

2. **Install R:**
   Ensure you have R version 4.3.1 or a later version installed on your system. If you haven't installed R yet, you can download it from the official R website: [https://cran.rstudio.com/](https://cran.rstudio.com/).

3. **Install RStudio:**
   RStudio provides an integrated development environment for working with R. You can download the latest version of RStudio from their official website: [https://www.rstudio.com/products/rstudio/](https://www.rstudio.com/products/rstudio/).

4. **Run the Application:**
   - Open RStudio on your machine.
   - Navigate to the cloned `soldier` directory and open `ui.R` file.
   - Press the "Run App" button within RStudio.
   - The first time you run the application, it will install the necessary packages, which may take some time.

5. **Access the Application:**
   The application will open in your default web browser window, and you can start using SOLDIER.

If you encounter any issues during the installation process, refer to the project's [documentation](https://github.com/cimnemadrid/SOLDIER/tree/main/manual) or seek support from the community.

## License

SOLDIER is distributed under the GNU Affero General Public License v3.0 (AGPL-3.0). This license is designed to ensure that users who interact with the software over a network, such as through a web application, also have access to the source code. It grants you the freedom to use, modify, and distribute SOLDIER under the terms and conditions outlined in the AGPL-3.0 license.

For more details about the rights and restrictions imposed by the AGPL-3.0 license, please refer to the [LICENSE](https://github.com/cimnemadrid/SOLDIER/blob/main/LICENSE) file in this repository.

---

**Disclaimer:**
SOLDIER is provided "as-is" and without any warranty. The developers make no guarantees regarding the accuracy, reliability, or suitability of the software for any particular purpose.

In no event shall the developers of SOLDIER be liable for any direct, indirect, incidental, special, exemplary, or consequential damages, including but not limited to loss of data, profits, or business interruption, resulting from the use or inability to use the SOLDIER software.

The user assumes full responsibility for the use of materials or information obtained from this application. Any service, repair, or correction of equipment or data required as a result of using SOLDIER is the sole responsibility of the user.

For assistance or support, please feel free to open an issue in this repository or contact us at cimnemadrid@cimne.upc.edu. However, please note that response times and support availability may vary.
