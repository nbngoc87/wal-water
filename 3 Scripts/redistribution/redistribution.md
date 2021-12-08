Redistribution effects of water tariffs
================
Nguyen Bich Ngoc, Jacques Teller
08 December 2021

![Household income
histogram](redistribution_files/figure-gfm/inchist-1.png) ![Household
income histogram](redistribution_files/figure-gfm/hhsplot-1.png)
![Household income
histogram](redistribution_files/figure-gfm/cspthist-1.png)
<table>
<thead>
<tr>
<th style="text-align:left;">
Utilities
</th>
<th style="text-align:right;">
Number of households
</th>
<th style="text-align:right;">
CVD
</th>
<th style="text-align:right;">
CVA
</th>
<th style="text-align:right;">
Average price
</th>
<th style="text-align:right;">
Block 1 price
</th>
<th style="text-align:right;">
Block 2 price
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
SWDE
</td>
<td style="text-align:right;">
1143
</td>
<td style="text-align:right;">
2.4480
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.4613
</td>
<td style="text-align:right;">
1.2240
</td>
<td style="text-align:right;">
4.1930
</td>
</tr>
<tr>
<td style="text-align:left;">
CILE
</td>
<td style="text-align:right;">
265
</td>
<td style="text-align:right;">
2.6366
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.6732
</td>
<td style="text-align:right;">
1.3183
</td>
<td style="text-align:right;">
4.3816
</td>
</tr>
<tr>
<td style="text-align:left;">
IECBW
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
2.1600
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.0727
</td>
<td style="text-align:right;">
1.0800
</td>
<td style="text-align:right;">
3.9050
</td>
</tr>
</tbody>
</table>

    ## Warning: Removed 192 row(s) containing missing values (geom_path).

![](redistribution_files/figure-gfm/avprcsm-1.png)<!-- -->

    ## Warning: Removed 150 row(s) containing missing values (geom_path).

![](redistribution_files/figure-gfm/mgprcsm-1.png)<!-- -->
<table>
<caption>
Household income quintile characteristics
</caption>
<thead>
<tr>
<th style="text-align:left;">
Quintile
</th>
<th style="text-align:right;">
Number of households
</th>
<th style="text-align:right;">
Number of people
</th>
<th style="text-align:right;">
Min income (EUR/month)
</th>
<th style="text-align:right;">
Max income (EUR/month)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
307
</td>
<td style="text-align:right;">
476
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
1250
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
307
</td>
<td style="text-align:right;">
604
</td>
<td style="text-align:right;">
1250
</td>
<td style="text-align:right;">
2250
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
307
</td>
<td style="text-align:right;">
730
</td>
<td style="text-align:right;">
2250
</td>
<td style="text-align:right;">
2750
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
307
</td>
<td style="text-align:right;">
854
</td>
<td style="text-align:right;">
2750
</td>
<td style="text-align:right;">
3750
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
306
</td>
<td style="text-align:right;">
1008
</td>
<td style="text-align:right;">
3750
</td>
<td style="text-align:right;">
5250
</td>
</tr>
</tbody>
</table>

![Income per equivalent adults for different household income
group](redistribution_files/figure-gfm/inceqa1-1.png) ![Income per
equivalent adults for different household income
group](redistribution_files/figure-gfm/inceqa2-1.png)
![](redistribution_files/figure-gfm/incpc-1.png)<!-- -->

``` r
# Correlation between water consumption and household income should use spearman?????

cor.test(df$csmptv, df$income, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$csmptv and df$income
    ## t = 15.76, df = 1532, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3296273 0.4157908
    ## sample estimates:
    ##       cor 
    ## 0.3735144

``` r
cor.test(df$csmptv, df$income, method = "spearman")
```

    ## Warning in cor.test.default(df$csmptv, df$income, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$csmptv and df$income
    ## S = 362572476, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##      rho 
    ## 0.397343

``` r
# Correlation between water consumption and income per equivalent adult should use spearman?????

cor.test(df$csmptv, df$inceqa, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$csmptv and df$inceqa
    ## t = 1.9123, df = 1532, p-value = 0.05603
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.001253886  0.098606824
    ## sample estimates:
    ##        cor 
    ## 0.04879841

``` r
cor.test(df$csmptv, df$inceqa, method = "spearman")
```

    ## Warning in cor.test.default(df$csmptv, df$inceqa, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$csmptv and df$inceqa
    ## S = 557405323, p-value = 0.003974
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## 0.07349777

![Proportion of household paying in which block by
quantile](redistribution_files/figure-gfm/blprop1-1.png) ![Proportion of
household paying in which block by income quintile and
utilities](redistribution_files/figure-gfm/blprop2-1.png)
![](redistribution_files/figure-gfm/csinc1-1.png)<!-- -->
![](redistribution_files/figure-gfm/csinc2-1.png)<!-- -->
![](redistribution_files/figure-gfm/hhsinc-1.png)<!-- -->
![](redistribution_files/figure-gfm/rwtinc-1.png)<!-- -->
![](redistribution_files/figure-gfm/densinc-1.png)<!-- -->
![](redistribution_files/figure-gfm/billinc1-1.png)<!-- -->
![](redistribution_files/figure-gfm/billinc2-1.png)<!-- -->
![](redistribution_files/figure-gfm/TEHinc-1.png)<!-- -->
![](redistribution_files/figure-gfm/mgprinc1-1.png)<!-- -->
![](redistribution_files/figure-gfm/mgprinc2-1.png)<!-- -->
![](redistribution_files/figure-gfm/mgrprchhsinc-1.png)<!-- -->

``` r
summary(df$mgnprc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.080   4.193   4.193   3.679   4.193   4.382

``` r
summary(df$mgnprc[df$poorest == 1])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.080   1.224   4.193   3.104   4.193   4.382

``` r
### average price vs income ---------
```

    ## Warning: Removed 18 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 18 rows containing non-finite values (stat_summary).

    ## Warning: Removed 18 rows containing non-finite values (stat_summary).

![](redistribution_files/figure-gfm/avprinc1-1.png)<!-- -->
![](redistribution_files/figure-gfm/avprinc2-1.png)<!-- -->
![](redistribution_files/figure-gfm/avprinc3-1.png)<!-- -->
![](redistribution_files/figure-gfm/avrprchhsinc-1.png)<!-- -->

``` r
summary(df$avrprc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.955   4.325   4.421   5.218   4.578 102.534

``` r
summary(df$avrprc[df$poorest == 1])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   4.076   4.416   4.554   7.072   5.873 102.534

``` r
### subsidy vs income ----------------
```

![](redistribution_files/figure-gfm/subsinc1-1.png)<!-- -->
![](redistribution_files/figure-gfm/subsinc2-1.png)<!-- -->
![](redistribution_files/figure-gfm/csdens1-1.png)<!-- -->
![](redistribution_files/figure-gfm/csdens2-1.png)<!-- -->
![](redistribution_files/figure-gfm/incdens1-1.png)<!-- -->
![](redistribution_files/figure-gfm/incdens2-1.png)<!-- -->
![](redistribution_files/figure-gfm/hhsdens-1.png)<!-- -->
![](redistribution_files/figure-gfm/rwtdens-1.png)<!-- -->
![](redistribution_files/figure-gfm/billdens-1.png)<!-- -->
![](redistribution_files/figure-gfm/TEHdens-1.png)<!-- -->

    ## Warning: Removed 18 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 18 rows containing non-finite values (stat_summary).

    ## Warning: Removed 18 rows containing non-finite values (stat_summary).

![](redistribution_files/figure-gfm/avprdens1-1.png)<!-- -->
![](redistribution_files/figure-gfm/avprdens2-1.png)<!-- -->
![](redistribution_files/figure-gfm/avprdens3-1.png)<!-- -->
![](redistribution_files/figure-gfm/avrprchhsdens-1.png)<!-- -->
![](redistribution_files/figure-gfm/subsdens1-1.png)<!-- -->
![](redistribution_files/figure-gfm/subsdens2-1.png)<!-- -->
<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Scenarios

</div>

</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Fixed

</div>

</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

CVD

</div>

</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

CVA

</div>

</th>
</tr>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
</th>
<th style="text-align:right;">
SWDE
</th>
<th style="text-align:right;">
CILE
</th>
<th style="text-align:right;">
IECBW
</th>
<th style="text-align:right;">
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
As in 2014
</td>
<td style="text-align:right;">
101.4885
</td>
<td style="text-align:right;">
2.4480
</td>
<td style="text-align:right;">
2.6366
</td>
<td style="text-align:right;">
2.1600
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
4.3438
</td>
<td style="text-align:right;">
4.5928
</td>
<td style="text-align:right;">
3.7129
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
50.0000
</td>
<td style="text-align:right;">
3.4081
</td>
<td style="text-align:right;">
3.6620
</td>
<td style="text-align:right;">
2.9003
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
100.0000
</td>
<td style="text-align:right;">
2.4725
</td>
<td style="text-align:right;">
2.7312
</td>
<td style="text-align:right;">
2.0877
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
150.0000
</td>
<td style="text-align:right;">
1.5369
</td>
<td style="text-align:right;">
1.8004
</td>
<td style="text-align:right;">
1.2751
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
200.0000
</td>
<td style="text-align:right;">
0.6013
</td>
<td style="text-align:right;">
0.8696
</td>
<td style="text-align:right;">
0.4625
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
</tbody>
</table>

![](redistribution_files/figure-gfm/fixedpc-1.png)<!-- -->
![](redistribution_files/figure-gfm/fixedpcinc-1.png)<!-- -->
![](redistribution_files/figure-gfm/fixedTEH-1.png)<!-- -->
