
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(rsconnect)

#setwd("C:/Users/bsully/OneDrive - Université Paris 1 Panthéon-Sorbonne/Documents/Cisse_Shiny")
#bd <- read.csv("./test.csv", sep = ",", header = T, encoding = "uft-8")
bd <- read.csv("./test.csv", sep = ",", header = T, encoding = "uft-8")
x<-bd[,-c(3,7,10)]
reg1<-lm(MEDV~CRIM + ZN + CHAS + NOX + RM + DIS + RAD + PTRATIO + B + LSTAT, data=bd)

