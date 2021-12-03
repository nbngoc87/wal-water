---
title: "Redistribution effects of water tariffs"
author: "Nguyen Bich Ngoc, Jacques Teller"  
date: "03 December 2021"
output: 
 bookdown::pdf_document2:
  theme: cosmo
  keep_md: true
---



# Introduction
## Rational:
* Water tariff objectives:
   + self-sufficiency for service providers
   + equity among customers
   + conservation/economic efficiency for society
* Current tariff in Wallonia:
   + 2 parts: fixed + volumetric
   + fixed: 20CVD + 20CVA, differences among distributors are negligible
   + volumetric: 2 increasing blocks at connection level --- IBT-con &#40;actually 3 blocks, but household rarely reach block 3&#41;
      - 0-30 m^3^: 0.5*CVD
      - 30-500 m^3^: CVD + CVA
* Arguments for IBT:
   + incentive to save water &#40;higher price for larger consumption&#41;
   + pro-poor &#40;supposedly&#41;
* Arguments against IBT:
   + pro-poor often not true due to low correlation between water consumption and income. That value of Wallonia is 0.3735144 for household consumption and -0.0462846 for consumption per inhabitant.
   + difficult to understand hence not clear signal for custumer to use water wisely  
   
## Objectives
* Assess social aspects of current price tariffs
* Compare social equity of different hypothesized tariff schemes 

# Data and method
## Data
* Utility survey data provided by Aquawal and CEHD
   + year: 2014     
   + 1534 households
   + 3 main distributors: SWDE &#40;1143&#41;, CILE &#40;265&#41;, inBW &#40;126&#41;
   + information include: water consumption, household size, income, rainwater tank ...
* built-up density
   + year: 2011
   + at 100x100m scale
   + 3 categories: low, medium, high
   
## Method
* assess social aspects of current tariff
   + divide households into 4 groups using household income quartiles
   + pairwise analyses of household income and other factors: income per equivalent adults, water use, water bill, TEH ...
* compare different tariff scheme
   + current format with different changing fixed part
      - assumptions: keep same total bill in 2014 for all households within the same distributor & keep CVA of 2014 not changing
      - changing fixed part: EUR 0, 40, ..., 200 
      - recalculate CVD for each distributor at each value of fixed part
      - recalculate household water bill in 2014 and compare with the actual one
   + current format with potential tax on rainwater tank
      - assumptions: keep same total bill in 2014 for all households within the same distributor & keep CVA of 2014 not changing
      - changing watertank tax: EUR 0, 40, ..., 200 
      - recalculate CVD for each distributor at each value of fixed part
      - recalculate household water bill in 2014 and compare with the actual one
   + compare IBT-con, IBT-cap, linear

# Results
## Social aspects of water tariff in Wallonia


\begin{table}

\caption{(\#tab:tabinc)Household income quartile characteristics}
\centering
\begin{tabular}[t]{l|r|r|r|r}
\hline
Quartile & Number of households & Number of people & Min income (EUR/month) & Max income (EUR/month)\\
\hline
1 & 384 & 615 & 125 & 1750\\
\hline
2 & 384 & 840 & 1750 & 2250\\
\hline
3 & 383 & 993 & 2250 & 3250\\
\hline
4 & 383 & 1224 & 3250 & 5250\\
\hline
\end{tabular}
\end{table}
![(\#fig:inceqa)Income per equivalent adults for different household income group](redistribution_files/figure-latex/inceqa-1.pdf) 




