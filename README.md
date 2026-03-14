# House Hunt: Price Per Square Foot Analysis

## Overview

This is an R programming script designed to help a house hunter answer one question: Am I paying too much per square foot?

It pulls sold-home data from Redfin's API, processes it, and creates visualizations of price per square foot (PPSF) trends over time, broken down by bedroom and bathroom counts. It downloads five years of sold-home data from Redfin, calculating average PPSF by beds/baths combinations, and plotting those trends with confidence intervals. 

## Core message 

You can make better informed offers by understanding how PPSF has moved over time for homes like the one you're considering. This is a starting point for market intuition, not a pricing model. Use it to understand broad trends and identify outliers, but don't make a final offer based solely on where a home falls relative to these PPSF curves.

## Steps in the procedure

1. Define your search parameters. 
2. Pull transaction data, sold prices from Redfin's API going back five years. 
3. Clean and transform the data: standardize names, remove incomplete records, calculate PPSF, and create grouping variables. 
4. Visualize trends. 
5. Export the data. 

## Search parameters

- Region
- Price range
- Property type
- Beds
- Baths
- Time window

## About the two visualizations

Two plots show average PPSF over time. The one-way plot shows annual PPSF trends grouped by beds alone. The two-way plot shows annual PPSF trends grouped by beds and baths. Both include 10th/90th percentile error bars and sample size labels. 

PPSF is a useful (if simplified) metric for comparing homes, and that tracking it over time by beds/baths gives you a rational basis for your offer price. PPSF varies based on many factors, so we reduce real-world complexity to PPSF's relationship with beds and baths. 

## An example of usage

Say you find a promising online listing in region with three beds and two baths. Refer to the two-way plot, to the facet with three beds, and follow the line representing two baths: did the 3-bed 2-bath line trended up or down? Next, refer to the CI in the latest year: does your listing's (list) PPSF fall closer to the lower or upper end of the CI. Also, where do you think it ought to be due to intrinsic quality, perceived value? 

## Motivation

This script exists because home buying can be emotionally charged and informationally asymmetric. Buyers often don't know whether a listing price reflects fair market value. List site provide standard tools with limited functionality. 

## Features and strengths 

- The script uses actual sold-price data, not list prices or estimates. Sold prices are a gold standard for market analysis because they reflect what buyers actually paid.
- Five-year time window gives enough history to see meaningful trends (pre-pandemic, pandemic boom, and recent normalization). 
- Confidence intervals (10th and 90th percentiles) show dispersion, not just averages. This helps you understand how much variability exists within each beds/baths group.
- Sample size labels on the plots let you see where the data is thin. A PPSF average based on 5 sales means something different than one based on 50.

## How the code is organized

The code is organized with section headers, commented parameters, and a logical pipeline from raw data through cleaning to visualization. The script follows a ETL pipeline pattern common in data analysis. It uses tidyverse conventions consistently and names intermediate data frames sequentially (redfin_00, redfin_01, redfin_02) to facilitate debug (if needed) by inspecting each stage. 

## Audience 

This script benefits data-literate home buyers who can run R scripts and interpret statistical plots. This tool gives them a structured, evidence-based framework for evaluating asking prices and calibrating offers. 

List sites have made transaction data more accessible, and technically skilled buyers increasingly build their own analyses rather than relying solely on agent comparative market analyses. 

I wrote this script as a solution to answer my own question and leave breadcrumbs for anyone who may find the script useful. 
