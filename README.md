# ETF Time Series Analysis — Univariate & Multivariate GARCH with VAR Modeling

## 📊 Overview

This academic project simulates a client-oriented analysis, performing a time series study on three ETFs representing different asset classes and geographic exposures. The objective is to assist a hypothetical client in portfolio diversification by studying return dynamics, modeling volatility using univariate and multivariate GARCH models, and analyzing interdependencies through a Vector Autoregressive (VAR) approach.

## 📈 ETFs Analyzed

- CW8.PA - Amundi MSCI World UCITS ETF (Global Equities)

- CRP.PA - Amundi EUR Corporate Bond Climate Paris Aligned UCITS ETF (Corporate Bonds, Climate-aligned)

- LGQM.DE - Amundi Pan Africa UCITS ETF (African Equities)

## 🎯 Objectives

- Analyze historical returns and basic statistics of selected ETFs.

- Model conditional volatility with GARCH(1,1) models.

- Explore joint volatility dynamics with multivariate GARCH-BEKK models.

- Analyze interdependencies with VAR models and study cross-market reactions via Impulse Response Functions (IRF).

## 🔬 Methods & Models

**1️⃣ Descriptive Analysis**

- Annualized returns

- Volatility (standard deviation)

- Skewness and kurtosis

- Autocorrelation functions (ACF)

**2️⃣ GARCH(1,1) Modeling**

- Estimate conditional volatility per ETF

- Study shock sensitivity (alpha) and volatility persistence (beta)

**3️⃣ Multivariate GARCH-BEKK**

- Capture joint volatility and time-varying correlations between ETF pairs

**4️⃣ VAR Modeling & Impulse Response Functions**

- Study interdependencies between ETFs

- Analyze how shocks propagate across markets

## 📂 Data

- Source: Yahoo Finance

- Frequency: Weekly data

- Period covered: April 2018 — March 2025

## 🛠️ Tool

- R (RStudio)

## 🔎 Key Findings

- Global equities (CW8.PA) show persistent but moderate volatility.

- Corporate bonds (CRP.PA) are relatively stable but can experience extreme events.

- African equities (LGQM.DE) exhibit high volatility and strong sensitivity to shocks.

- Diversification benefits reduce significantly during global crises (e.g. COVID-19 2020).

**Thank you for taking the time to read and explore this project. I am constantly learning and improving my quantitative finance skills.
I warmly welcome any feedback, suggestions, or recommendations that could help me enhance this project and develop further.
Feel free to reach out!**

**Gianni Marchetti**

**Master 1 student in Finance at Aix-Marseille School of Economics**

**giannimarchetti@outlook.fr**
