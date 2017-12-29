#!/bin/R

source('functions.R')

regije <- function() {
  counts <- table(wines$Region)
  barplot(counts, main = 'Regije')
}

serving <- function() {
  counts <- table(wines$ServingTemperature)
  barplot(counts, main = 'Temperature')
}

year <- function() {
  counts <- table(wines$Year)
  barplot(counts, main = 'Years')
}

bodies <- function() {
  counts <- table(wines$Body)
  barplot(counts, main = 'Body')
}

sugar <- function() {
  counts <- table(wines$SugarLevel)
  barplot(counts, main = 'Tipi')
}

alcohol <- function() {
  counts <- table(wines$Alcohol)
  barplot(counts, main = 'Alko')
}

maturation <- function() {
  
}


