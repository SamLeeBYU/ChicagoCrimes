---
Title: Werewolves in Chicago?
Subtitle: An Predictive Analysis to Reduce Violent Crime in Chicago's 11th District
Authors: Sam Lee, Jeffry Troll
Contributors: Sam Lee, Jeffry Troll, Isaac Aguilar
Date: 04/17/2024
---

# Introduction

Violent crime in Chicago remains a persistent challenge. For the 2024 BYU Statistics Case Study Competition, the Chicago Police Department (CPD), particularly concerned about the situation in District 11, has partnered with Data Insight & Strategy Consultants (DISC) to explore innovative approaches. This project delves beyond traditional methods, incorporating human behavior, weather patterns, and even the lunar cycle, to analyze crime data and uncover potential predictors.

Our goal is to determine if and how these factors influence violent crime rates in Chicago. This knowledge will empower the CPD to proactively implement crime prevention strategies, such as public outreach and community events, during particularly vulnerable periods. By shedding light on the potential relationships between these factors and crime, we aim to make Chicago a safer place for all.

Our full predictive analysis can be read [here](MiniFinalReport.pdf).

--- 

# Data

#### [chicago-unemployment.csv](Data/chicago-unemployment.csv)

This data set contains the monthly unemployment for Chicago since 2010. This data was obtained from the Bureau of Labor Statistics.

#### [crimes_cleaned.csv](Data/crimes_cleaned.csv)

This data set is the analytic sample we use in our data analysis. This data is produced by [setup.R](Scripts/setup.r). This file is the cleaned agglomeration of all the data files we use.

#### [full_moon.csv](Data/full_moon.csv)

This data file contains dates of full moons since 2005. This was sourced from <https://www.fullmoonology.com/full-moon-calendar-2015/>.

#### [holidays.csv](Data/holidays.csv)

This data file contains a list of all major U.S. holidays and their associated dates throughout the decade. This file was created with help from ChatGPT.

#### [model_data.csv](Data/model_data.csv)

This file is created by [model.R](Scripts/model.R). This is the model matrix that is used by the generalized linear models discussed in our report.

#### [weather.csv](Data/weather.csv)

This contains daily weather data. This data was obtained from <https://www.visualcrossing.com/>.

#### [weather.json](Data/weather.json)

This is the API call from another weather database that we resorted to that provided hourly weather data. We used <https://open-meteo.com/>.

Note: We don't have raw crimes data (*Crimes.csv*) on this repo because the file was too large to upload. The data was obtained from <https://data.cityofchicago.org/Public-Safety/Crimes-2022/9hwr-2zxp/data>. 

---

# Scripts

#### [kfold.R](Scripts/kfold.R)

This script was created to provide K-fold cross validation and train-test-split functionalities in R, otherwise available in the scikit-learn package in Python.

#### [model.R](Script/model.R)

This script is the main script for computing all the generalized linear models that we run (see report). This also produces [model_data.csv](Data/model_data.csv) and other graphical features to visualize the results of the model.

#### [setup.R](Scripts/setup.r)

This script aggregates all of the raw data sources and produces [crimes_cleaned.csv](Data/crimes_cleaned.csv).

#### [sum_stats.R](Scripts/sum_stats.R)

This is a small package we made to automatically take raw R data frames and format them into relatively nice tables of summary statistics. We use this function to display summary statistics in our report.

#### [testing_cluster.ipynb](Scripts/testing_cluster.ipynb)

A script built to perform cluster analysis and K-means on our data.

#### [testing_KNN.ipynb](Scripts/testing_KNN.ipynb)

This script runs a regression to predict the number of daily violent crimes using K-Nearest-Neighbors (KNN) regression.

#### [testing_random_forest.ipynb](Scripts/testing_random_forest.ipynb)

This script runs a random forest model to predict the number of violent crimes.

#### [testing_shap.ipynb](Scripts/testing_shap.ipynb)

This script uses SHAP to analyze feature importance of our random forest models.

#### [testing_tree.ipynb](Scripts/testing_tree.ipynb)

This script uses gradient boosting on decision trees to predict the number of daily violent crimes.

---

All the scripts above are the polished scripts we use in our analysis and report. Other working scripts and files are available in each of the contributors' working folders.

