# ShinnyApp2
A shiny app to help with feature selection for genetic data relating to kidney graft survival

The main app visualises the Accuracy and F1 Score for a SVM and Random Forest Model based on a given number of selected features.

The main idea is to be bale to indentify the most differential RNA-sequences between stable recipients and those who experience a graft
rejection as well as how the feature selection affects a models performance.


#### Instruction
```
library(tidyverse)
library(shiny)
library(ggplot2)
library(GEOquery)
library(R.utils)
library(reshape2)
library(ggplot2)
library(limma)
library(dplyr)
library(plotly)
library(ggpubr)

shiny::runGitHub(<br>
    repo = "ShinnyApp2",
    username = "niruth98", 
    ref = "master")
```
