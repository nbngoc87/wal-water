Redistribution effects of water tariffs
================
Nguyen Bich Ngoc, Jacques Teller
24 December 2021

![Household income
histogram](redistribution%20211224_1210_files/figure-gfm/inchist-1.png)

``` r
inccatdf <- df[!(is.na(df$inccat)),] %>%
  group_by(inccat) %>%
  summarise(
    count = n(),
    prop = n() / nrow(df),
    income_avr = mean(income),
    income_min = min(income),
    income_max = max(income),
    inceqa_avr = mean(inceqa),
    inceqa_min = min(inceqa),
    inceqa_max = max(inceqa)
  )

inccatdf
```

    ## # A tibble: 4 x 9
    ##   inccat     count   prop income_avr income_min income_max inceqa_avr
    ##   <fct>      <int>  <dbl>      <dbl>      <int>      <int>      <dbl>
    ## 1 precarious   164 0.0949      1144.        125       2250      8928.
    ## 2 modest       781 0.452       1894.       1250       3250     16022.
    ## 3 average      566 0.328       3216.       2250       4750     23687.
    ## 4 higher       216 0.125       4759.       3750       5250     30933.
    ## # ... with 2 more variables: inceqa_min <dbl>, inceqa_max <dbl>

![Household income
histogram](redistribution%20211224_1210_files/figure-gfm/hhsplot-1.png)
![Household income
histogram](redistribution%20211224_1210_files/figure-gfm/cspthist-1.png)

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
<th style="text-align:right;">
Block2/Block1
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
SWDE
</td>
<td style="text-align:right;">
1308
</td>
<td style="text-align:right;">
2.4480
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.7061
</td>
<td style="text-align:right;">
1.2974
</td>
<td style="text-align:right;">
4.4446
</td>
<td style="text-align:right;">
3.4257
</td>
</tr>
<tr>
<td style="text-align:left;">
CILE
</td>
<td style="text-align:right;">
277
</td>
<td style="text-align:right;">
2.6366
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.9270
</td>
<td style="text-align:right;">
1.3974
</td>
<td style="text-align:right;">
4.6445
</td>
<td style="text-align:right;">
3.3237
</td>
</tr>
<tr>
<td style="text-align:left;">
inBW
</td>
<td style="text-align:right;">
143
</td>
<td style="text-align:right;">
2.1600
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.3268
</td>
<td style="text-align:right;">
1.1448
</td>
<td style="text-align:right;">
4.1393
</td>
<td style="text-align:right;">
3.6157
</td>
</tr>
</tbody>
</table>

    ## Warning: Removed 192 row(s) containing missing values (geom_path).

![](redistribution%20211224_1210_files/figure-gfm/avprcsm-1.png)<!-- -->

    ## Warning: Removed 150 row(s) containing missing values (geom_path).

![](redistribution%20211224_1210_files/figure-gfm/mgprcsm-1.png)<!-- -->

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
346
</td>
<td style="text-align:right;">
550
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
1750
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
346
</td>
<td style="text-align:right;">
695
</td>
<td style="text-align:right;">
1750
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
346
</td>
<td style="text-align:right;">
823
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
345
</td>
<td style="text-align:right;">
967
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
345
</td>
<td style="text-align:right;">
1156
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
group](redistribution%20211224_1210_files/figure-gfm/inceqa1-1.png)
![Income per equivalent adults for different household income
group](redistribution%20211224_1210_files/figure-gfm/inceqa2-1.png)
![](redistribution%20211224_1210_files/figure-gfm/incpc-1.png)<!-- -->

``` r
# Correlation between water consumption and household income should use spearman?????

cor.test(df$csmptv, df$income, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$csmptv and df$income
    ## t = 15.729, df = 1726, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3121384 0.3946480
    ## sample estimates:
    ##      cor 
    ## 0.354082

``` r
cor.test(df$csmptv, df$income, method = "spearman")
```

    ## Warning in cor.test.default(df$csmptv, df$income, method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$csmptv and df$income
    ## S = 536353649, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.3763062

``` r
# Correlation between water consumption and income per equivalent adult should use spearman?????

cor.test(df$csmptv, df$inceqa, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$csmptv and df$inceqa
    ## t = 1.4269, df = 1726, p-value = 0.1538
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.01285165  0.08134826
    ## sample estimates:
    ##        cor 
    ## 0.03432454

``` r
cor.test(df$csmptv, df$inceqa, method = "spearman")
```

    ## Warning in cor.test.default(df$csmptv, df$inceqa, method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$csmptv and df$inceqa
    ## S = 803896839, p-value = 0.006707
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## 0.06519613

![Proportion of household paying in which block by
quantile](redistribution%20211224_1210_files/figure-gfm/blprop1-1.png)
![Proportion of household paying in which block by income quintile and
utilities](redistribution%20211224_1210_files/figure-gfm/blprop2-1.png)
![](redistribution%20211224_1210_files/figure-gfm/csinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/csinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/hhsinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/hhsinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/hhsieq-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwtinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/densinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/billinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/billinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/TEHinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/mgprinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/mgprinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/mgrprchhsinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avprinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avprinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avprinc3-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avrprchhsinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/subsinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/subsinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/csdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/csdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/incdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/incdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/hhsdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwtdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/billdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/TEHdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avprdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avprdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avprdens3-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/avrprchhsdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/subsdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/subsdens2-1.png)<!-- -->

``` r
summary(df$avrprc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   4.200   4.595   4.690   5.000   4.850  12.549

``` r
summary(df$avrprc[df$poorest == 1])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 

``` r
summary(df$avrprc[df$inccat == "precarious"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   4.222   4.605   4.734   5.152   4.915  10.592       1

``` r
summary(df$subs[df$inccat == "precarious"])
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    ## -68.1035  -4.7756   0.5658  -4.0094   8.3744  27.4910        1

``` r
summary(df$mgnprc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.158   4.458   4.458   3.982   4.458   4.658

``` r
summary(df$mgnprc[df$poorest == 1])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 

``` r
summary(df$mgnprc[df$inccat == "precarious"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   1.158   4.153   4.458   3.754   4.458   4.658       1

``` r
## 3.5. changing fixed  -----

### new cvd ------
```

<table>
<thead>
<tr>
<th style="text-align:right;">
CVD_SWDE
</th>
<th style="text-align:right;">
CVD_CILE
</th>
<th style="text-align:right;">
CVD_inBW
</th>
<th style="text-align:right;">
CVA
</th>
<th style="text-align:left;">
scenario
</th>
<th style="text-align:right;">
fixed
</th>
<th style="text-align:right;">
rwtt
</th>
<th style="text-align:right;">
mgpr_bl1
</th>
<th style="text-align:right;">
mgpr_bl2
</th>
</tr>
</thead>
<tbody>
<tr>
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
<td style="text-align:left;">
As in 2014
</td>
<td style="text-align:right;">
101.438
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.2272
</td>
<td style="text-align:right;">
4.1994
</td>
</tr>
<tr>
<td style="text-align:right;">
4.2744
</td>
<td style="text-align:right;">
4.5442
</td>
<td style="text-align:right;">
3.6839
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.1344
</td>
<td style="text-align:right;">
6.0138
</td>
</tr>
<tr>
<td style="text-align:right;">
3.3730
</td>
<td style="text-align:right;">
3.6365
</td>
<td style="text-align:right;">
2.8864
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
50.000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.6875
</td>
<td style="text-align:right;">
5.1200
</td>
</tr>
<tr>
<td style="text-align:right;">
2.4716
</td>
<td style="text-align:right;">
2.7289
</td>
<td style="text-align:right;">
2.0890
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
100.000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.2406
</td>
<td style="text-align:right;">
4.2262
</td>
</tr>
<tr>
<td style="text-align:right;">
1.5702
</td>
<td style="text-align:right;">
1.8212
</td>
<td style="text-align:right;">
1.2916
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
150.000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.7937
</td>
<td style="text-align:right;">
3.3324
</td>
</tr>
<tr>
<td style="text-align:right;">
0.6688
</td>
<td style="text-align:right;">
0.9135
</td>
<td style="text-align:right;">
0.4942
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
200.000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.3468
</td>
<td style="text-align:right;">
2.4386
</td>
</tr>
</tbody>
</table>

![](redistribution%20211224_1210_files/figure-gfm/fixpcinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixpcincinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixTEHinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixsubsinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixavprinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixavprinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixpcdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixpcincdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixTEHdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixsubsdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixavprdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixavprdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixdpcpreca-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixtehpreca-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixsubspreca-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/fixavprpreca-1.png)<!-- -->

<table>
<thead>
<tr>
<th style="text-align:right;">
CVD_SWDE
</th>
<th style="text-align:right;">
CVD_CILE
</th>
<th style="text-align:right;">
CVD_inBW
</th>
<th style="text-align:right;">
CVA
</th>
<th style="text-align:right;">
scenario
</th>
<th style="text-align:right;">
fixed
</th>
<th style="text-align:right;">
rwtt
</th>
<th style="text-align:right;">
mgpr_bl1
</th>
<th style="text-align:right;">
mgpr_bl2
</th>
</tr>
</thead>
<tbody>
<tr>
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
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
101.4380
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.2272
</td>
<td style="text-align:right;">
4.1994
</td>
</tr>
<tr>
<td style="text-align:right;">
2.1517
</td>
<td style="text-align:right;">
2.4683
</td>
<td style="text-align:right;">
1.8852
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
95.9578
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
1.0902
</td>
<td style="text-align:right;">
3.9254
</td>
</tr>
<tr>
<td style="text-align:right;">
1.8554
</td>
<td style="text-align:right;">
2.3000
</td>
<td style="text-align:right;">
1.6104
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
90.4777
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.9532
</td>
<td style="text-align:right;">
3.6514
</td>
</tr>
<tr>
<td style="text-align:right;">
1.5591
</td>
<td style="text-align:right;">
2.1318
</td>
<td style="text-align:right;">
1.3356
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
84.9975
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
0.8162
</td>
<td style="text-align:right;">
3.3774
</td>
</tr>
<tr>
<td style="text-align:right;">
1.2628
</td>
<td style="text-align:right;">
1.9635
</td>
<td style="text-align:right;">
1.0608
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
79.5174
</td>
<td style="text-align:right;">
200
</td>
<td style="text-align:right;">
0.6792
</td>
<td style="text-align:right;">
3.1034
</td>
</tr>
</tbody>
</table>

![](redistribution%20211224_1210_files/figure-gfm/rwttpcinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttpcincinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttTEHinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttsubsinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttavprinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttavprinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttpcdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttpcincdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttTEHdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttTEHdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttsubsdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttavprdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttavprdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttdpcpreca-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwtttehpreca-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttsubspreca-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/rwttavprpreca-1.png)<!-- -->

<table>
<thead>
<tr>
<th style="text-align:right;">
bl1_SWDE
</th>
<th style="text-align:right;">
bl1_CILE
</th>
<th style="text-align:right;">
bl1_inBW
</th>
<th style="text-align:right;">
fixed
</th>
<th style="text-align:right;">
revincr
</th>
<th style="text-align:right;">
mgpr_bl1
</th>
<th style="text-align:right;">
mgpr_bl2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
4.427231
</td>
<td style="text-align:right;">
4.635599
</td>
<td style="text-align:right;">
4.069370
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
4.431018
</td>
<td style="text-align:right;">
4.431018
</td>
</tr>
<tr>
<td style="text-align:right;">
3.710676
</td>
<td style="text-align:right;">
3.914732
</td>
<td style="text-align:right;">
3.423071
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
3.719586
</td>
<td style="text-align:right;">
3.719586
</td>
</tr>
<tr>
<td style="text-align:right;">
2.994121
</td>
<td style="text-align:right;">
3.193866
</td>
<td style="text-align:right;">
2.776773
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
3.008154
</td>
<td style="text-align:right;">
3.008154
</td>
</tr>
<tr>
<td style="text-align:right;">
5.315177
</td>
<td style="text-align:right;">
5.565218
</td>
<td style="text-align:right;">
4.885744
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
5.319721
</td>
<td style="text-align:right;">
5.319721
</td>
</tr>
<tr>
<td style="text-align:right;">
4.598622
</td>
<td style="text-align:right;">
4.844352
</td>
<td style="text-align:right;">
4.239446
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
4.608289
</td>
<td style="text-align:right;">
4.608289
</td>
</tr>
<tr>
<td style="text-align:right;">
3.882067
</td>
<td style="text-align:right;">
4.123486
</td>
<td style="text-align:right;">
3.593147
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
3.896857
</td>
<td style="text-align:right;">
3.896857
</td>
</tr>
<tr>
<td style="text-align:right;">
6.647096
</td>
<td style="text-align:right;">
6.959648
</td>
<td style="text-align:right;">
6.110305
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
6.652777
</td>
<td style="text-align:right;">
6.652777
</td>
</tr>
<tr>
<td style="text-align:right;">
5.930541
</td>
<td style="text-align:right;">
6.238782
</td>
<td style="text-align:right;">
5.464006
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
5.941345
</td>
<td style="text-align:right;">
5.941345
</td>
</tr>
<tr>
<td style="text-align:right;">
5.213986
</td>
<td style="text-align:right;">
5.517916
</td>
<td style="text-align:right;">
4.817708
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
5.229913
</td>
<td style="text-align:right;">
5.229913
</td>
</tr>
</tbody>
</table>
<table>
<thead>
<tr>
<th style="text-align:right;">
bl1_SWDE
</th>
<th style="text-align:right;">
bl1_CILE
</th>
<th style="text-align:right;">
bl1_inBW
</th>
<th style="text-align:right;">
fixed
</th>
<th style="text-align:right;">
revincr
</th>
<th style="text-align:right;">
mgpr_bl1
</th>
<th style="text-align:right;">
mgpr_bl2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1.789001
</td>
<td style="text-align:right;">
1.876093
</td>
<td style="text-align:right;">
1.594286
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.786848
</td>
<td style="text-align:right;">
6.253969
</td>
</tr>
<tr>
<td style="text-align:right;">
1.499448
</td>
<td style="text-align:right;">
1.584348
</td>
<td style="text-align:right;">
1.341081
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.499952
</td>
<td style="text-align:right;">
5.249832
</td>
</tr>
<tr>
<td style="text-align:right;">
1.209895
</td>
<td style="text-align:right;">
1.292603
</td>
<td style="text-align:right;">
1.087876
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.213056
</td>
<td style="text-align:right;">
4.245695
</td>
</tr>
<tr>
<td style="text-align:right;">
2.147811
</td>
<td style="text-align:right;">
2.252323
</td>
<td style="text-align:right;">
1.914122
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
2.145226
</td>
<td style="text-align:right;">
7.508291
</td>
</tr>
<tr>
<td style="text-align:right;">
1.858259
</td>
<td style="text-align:right;">
1.960578
</td>
<td style="text-align:right;">
1.660917
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
1.858330
</td>
<td style="text-align:right;">
6.504154
</td>
</tr>
<tr>
<td style="text-align:right;">
1.568706
</td>
<td style="text-align:right;">
1.668834
</td>
<td style="text-align:right;">
1.407712
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
1.571433
</td>
<td style="text-align:right;">
5.500017
</td>
</tr>
<tr>
<td style="text-align:right;">
2.686027
</td>
<td style="text-align:right;">
2.816669
</td>
<td style="text-align:right;">
2.393877
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
2.682792
</td>
<td style="text-align:right;">
9.389773
</td>
</tr>
<tr>
<td style="text-align:right;">
2.396474
</td>
<td style="text-align:right;">
2.524924
</td>
<td style="text-align:right;">
2.140672
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
2.395896
</td>
<td style="text-align:right;">
8.385636
</td>
</tr>
<tr>
<td style="text-align:right;">
2.106921
</td>
<td style="text-align:right;">
2.233179
</td>
<td style="text-align:right;">
1.887467
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
2.109000
</td>
<td style="text-align:right;">
7.381499
</td>
</tr>
</tbody>
</table>
<table>
<thead>
<tr>
<th style="text-align:right;">
bl1_SWDE
</th>
<th style="text-align:right;">
bl1_CILE
</th>
<th style="text-align:right;">
bl1_inBW
</th>
<th style="text-align:right;">
fixed
</th>
<th style="text-align:right;">
revincr
</th>
<th style="text-align:right;">
mgpr_bl1
</th>
<th style="text-align:right;">
mgpr_bl2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1.816621
</td>
<td style="text-align:right;">
1.870233
</td>
<td style="text-align:right;">
1.608886
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.808024
</td>
<td style="text-align:right;">
6.328085
</td>
</tr>
<tr>
<td style="text-align:right;">
1.522598
</td>
<td style="text-align:right;">
1.579399
</td>
<td style="text-align:right;">
1.353362
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.517698
</td>
<td style="text-align:right;">
5.311944
</td>
</tr>
<tr>
<td style="text-align:right;">
1.228575
</td>
<td style="text-align:right;">
1.288566
</td>
<td style="text-align:right;">
1.097838
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
1.227372
</td>
<td style="text-align:right;">
4.295803
</td>
</tr>
<tr>
<td style="text-align:right;">
2.180972
</td>
<td style="text-align:right;">
2.245288
</td>
<td style="text-align:right;">
1.931651
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
2.170649
</td>
<td style="text-align:right;">
7.597272
</td>
</tr>
<tr>
<td style="text-align:right;">
1.886948
</td>
<td style="text-align:right;">
1.954454
</td>
<td style="text-align:right;">
1.676128
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
1.880323
</td>
<td style="text-align:right;">
6.581131
</td>
</tr>
<tr>
<td style="text-align:right;">
1.592925
</td>
<td style="text-align:right;">
1.663621
</td>
<td style="text-align:right;">
1.420604
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
1.589997
</td>
<td style="text-align:right;">
5.564990
</td>
</tr>
<tr>
<td style="text-align:right;">
2.727497
</td>
<td style="text-align:right;">
2.807871
</td>
<td style="text-align:right;">
2.415800
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
2.714586
</td>
<td style="text-align:right;">
9.501052
</td>
</tr>
<tr>
<td style="text-align:right;">
2.433473
</td>
<td style="text-align:right;">
2.517037
</td>
<td style="text-align:right;">
2.160276
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
2.424260
</td>
<td style="text-align:right;">
8.484911
</td>
</tr>
<tr>
<td style="text-align:right;">
2.139450
</td>
<td style="text-align:right;">
2.226204
</td>
<td style="text-align:right;">
1.904752
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
2.133934
</td>
<td style="text-align:right;">
7.468770
</td>
</tr>
</tbody>
</table>

![](redistribution%20211224_1210_files/figure-gfm/upccpcinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upcctehinc1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upcctehinc2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upccsubsinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upccavprinc-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upccpcdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upcctehdens1-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upcctehdens2-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upccsubsdens-1.png)<!-- -->
![](redistribution%20211224_1210_files/figure-gfm/upccavprdens-1.png)<!-- -->
