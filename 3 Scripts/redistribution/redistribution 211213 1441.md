Redistribution effects of water tariffs
================
Nguyen Bich Ngoc, Jacques Teller
13 December 2021

![Household income
histogram](redistribution%20211213%201441_files/figure-gfm/inchist-1.png)

``` r
inccatdf <- df[!(is.na(df$inccat)),] %>%
  group_by(inccat) %>%
  summarise(count = n(),
            prop = n()/nrow(df),
            income_avr = mean(income),
            income_min = min(income),
            income_max = max(income),
            inceqa_avr = mean(inceqa),
            inceqa_min = min(inceqa),
            inceqa_max = max(inceqa))

inccatdf
```

    ## # A tibble: 4 x 9
    ##   inccat     count   prop income_avr income_min income_max inceqa_avr
    ##   <fct>      <int>  <dbl>      <dbl>      <int>      <int>      <dbl>
    ## 1 precarious   148 0.0970      1135.        125       2250      8854.
    ## 2 modest       697 0.457       1881.       1250       3250     15961.
    ## 3 average      501 0.329       3203.       2250       4750     23622.
    ## 4 higher       178 0.117       4739.       3750       5250     30957.
    ## # ... with 2 more variables: inceqa_min <dbl>, inceqa_max <dbl>

![Household income
histogram](redistribution%20211213%201441_files/figure-gfm/hhsplot-1.png)
![Household income
histogram](redistribution%20211213%201441_files/figure-gfm/cspthist-1.png)

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
1138
</td>
<td style="text-align:right;">
2.4480
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.4551
</td>
<td style="text-align:right;">
1.2240
</td>
<td style="text-align:right;">
4.1930
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
261
</td>
<td style="text-align:right;">
2.6366
</td>
<td style="text-align:right;">
1.745
</td>
<td style="text-align:right;">
4.6523
</td>
<td style="text-align:right;">
1.3183
</td>
<td style="text-align:right;">
4.3816
</td>
<td style="text-align:right;">
3.3237
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
<td style="text-align:right;">
3.6157
</td>
</tr>
</tbody>
</table>

    ## Warning: Removed 192 row(s) containing missing values (geom_path).

![](redistribution%20211213%201441_files/figure-gfm/avprcsm-1.png)<!-- -->

    ## Warning: Removed 150 row(s) containing missing values (geom_path).

![](redistribution%20211213%201441_files/figure-gfm/mgprcsm-1.png)<!-- -->

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
305
</td>
<td style="text-align:right;">
474
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
305
</td>
<td style="text-align:right;">
602
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
305
</td>
<td style="text-align:right;">
734
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
305
</td>
<td style="text-align:right;">
845
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
305
</td>
<td style="text-align:right;">
1006
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
group](redistribution%20211213%201441_files/figure-gfm/inceqa1-1.png)
![Income per equivalent adults for different household income
group](redistribution%20211213%201441_files/figure-gfm/inceqa2-1.png)
![](redistribution%20211213%201441_files/figure-gfm/incpc-1.png)<!-- -->

``` r
# Correlation between water consumption and household income should use spearman?????

cor.test(df$csmptv, df$income, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$csmptv and df$income
    ## t = 15.505, df = 1523, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3250574 0.4117940
    ## sample estimates:
    ##       cor 
    ## 0.3692295

``` r
cor.test(df$csmptv, df$income, method = "spearman")
```

    ## Warning in cor.test.default(df$csmptv, df$income, method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$csmptv and df$income
    ## S = 358665135, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.3932203

``` r
# Correlation between water consumption and income per equivalent adult should use spearman?????

cor.test(df$csmptv, df$inceqa, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$csmptv and df$inceqa
    ## t = 1.8473, df = 1523, p-value = 0.06489
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.00291998  0.09724963
    ## sample estimates:
    ##       cor 
    ## 0.0472837

``` r
cor.test(df$csmptv, df$inceqa, method = "spearman")
```

    ## Warning in cor.test.default(df$csmptv, df$inceqa, method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$csmptv and df$inceqa
    ## S = 547594684, p-value = 0.004034
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## 0.07359449

![Proportion of household paying in which block by
quantile](redistribution%20211213%201441_files/figure-gfm/blprop1-1.png)
![Proportion of household paying in which block by income quintile and
utilities](redistribution%20211213%201441_files/figure-gfm/blprop2-1.png)
![](redistribution%20211213%201441_files/figure-gfm/csinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/csinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/hhsinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/hhsinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/hhsieq-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwtinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/densinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/billinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/billinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/TEHinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/mgprinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/mgprinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/mgrprchhsinc-1.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 9 rows containing non-finite values (stat_summary).

    ## Warning: Removed 9 rows containing non-finite values (stat_summary).

![](redistribution%20211213%201441_files/figure-gfm/avprinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/avprinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/avprinc3-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/avrprchhsinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/subsinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/subsinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/csdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/csdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/incdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/incdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/hhsdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwtdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/billdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/TEHdens-1.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 9 rows containing non-finite values (stat_summary).

    ## Warning: Removed 9 rows containing non-finite values (stat_summary).

![](redistribution%20211213%201441_files/figure-gfm/avprdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/avprdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/avprdens3-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/avrprchhsdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/subsdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/subsdens2-1.png)<!-- -->

``` r
summary(df$avrprc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.955   4.324   4.419   4.910   4.576  20.055

``` r
summary(df$avrprc[df$poorest == 1])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   4.076   4.416   4.551   5.606   5.499  16.277

``` r
summary(df$avrprc[df$inccat == "precarious"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   3.971   4.332   4.457   5.098   4.665  16.277       1

``` r
summary(df$subs[df$inccat == "precarious"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ## -79.564  -4.286   1.840  -3.918  10.142  30.484       1

``` r
summary(df$mgnprc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.080   4.193   4.193   3.693   4.193   4.382

``` r
summary(df$mgnprc[df$poorest == 1])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.080   1.224   4.193   3.155   4.193   4.382

``` r
summary(df$mgnprc[df$inccat == "precarious"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   1.080   3.905   4.193   3.478   4.193   4.382       1

``` r
## 3.5. changing fixed  -----

### new cvd ------
```

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
101.4797
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
4.3356
</td>
<td style="text-align:right;">
4.5642
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
3.4040
</td>
<td style="text-align:right;">
3.6470
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
2.4724
</td>
<td style="text-align:right;">
2.7298
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
1.5408
</td>
<td style="text-align:right;">
1.8126
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
0.6092
</td>
<td style="text-align:right;">
0.8954
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

![](redistribution%20211213%201441_files/figure-gfm/fixpcinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixpcincinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixTEHinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixsubsinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixavprinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixavprinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixpcdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixpcincdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixTEHdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixsubsdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixavprdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixavprdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixdpcpreca-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixtehpreca-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixsubspreca-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/fixavprpreca-1.png)<!-- -->

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Rainwater tank tax

</div>

</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Averaged Fixed

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
<th style="text-align:right;">
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
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
101.1914
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
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
95.4887
</td>
<td style="text-align:right;">
2.1349
</td>
<td style="text-align:right;">
2.4669
</td>
<td style="text-align:right;">
1.8680
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
89.7860
</td>
<td style="text-align:right;">
1.8218
</td>
<td style="text-align:right;">
2.2972
</td>
<td style="text-align:right;">
1.5759
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
84.0834
</td>
<td style="text-align:right;">
1.5087
</td>
<td style="text-align:right;">
2.1276
</td>
<td style="text-align:right;">
1.2839
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
<tr>
<td style="text-align:right;">
200
</td>
<td style="text-align:right;">
78.3807
</td>
<td style="text-align:right;">
1.1956
</td>
<td style="text-align:right;">
1.9579
</td>
<td style="text-align:right;">
0.9919
</td>
<td style="text-align:right;">
1.745
</td>
</tr>
</tbody>
</table>

![](redistribution%20211213%201441_files/figure-gfm/rwttpcinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttpcincinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttTEHinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttsubsinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttavprinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttavprinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttpcdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttpcincdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttTEHdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttTEHdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttsubsdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttavprdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttavprdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttdpcpreca-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwtttehpreca-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttsubspreca-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/rwttavprpreca-1.png)<!-- -->

<table>
<thead>
<tr>
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
fixed
</th>
<th style="text-align:right;">
revincr
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
4.455142
</td>
<td style="text-align:right;">
4.652346
</td>
<td style="text-align:right;">
4.072663
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
3.717046
</td>
<td style="text-align:right;">
3.923825
</td>
<td style="text-align:right;">
3.416345
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
2.978950
</td>
<td style="text-align:right;">
3.195304
</td>
<td style="text-align:right;">
2.760027
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
5.346171
</td>
<td style="text-align:right;">
5.582816
</td>
<td style="text-align:right;">
4.887196
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
4.608075
</td>
<td style="text-align:right;">
4.854294
</td>
<td style="text-align:right;">
4.230878
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
3.869979
</td>
<td style="text-align:right;">
4.125773
</td>
<td style="text-align:right;">
3.574559
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
6.682713
</td>
<td style="text-align:right;">
6.978520
</td>
<td style="text-align:right;">
6.108995
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:right;">
5.944617
</td>
<td style="text-align:right;">
6.249999
</td>
<td style="text-align:right;">
5.452677
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:right;">
5.206522
</td>
<td style="text-align:right;">
5.521477
</td>
<td style="text-align:right;">
4.796358
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
</tbody>
</table>
<table>
<thead>
<tr>
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
fixed
</th>
<th style="text-align:right;">
revincr
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1.809970
</td>
<td style="text-align:right;">
1.882461
</td>
<td style="text-align:right;">
1.604395
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
1.510107
</td>
<td style="text-align:right;">
1.587682
</td>
<td style="text-align:right;">
1.345843
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
1.210244
</td>
<td style="text-align:right;">
1.292903
</td>
<td style="text-align:right;">
1.087292
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
2.171964
</td>
<td style="text-align:right;">
2.258953
</td>
<td style="text-align:right;">
1.925274
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
1.872101
</td>
<td style="text-align:right;">
1.964174
</td>
<td style="text-align:right;">
1.666723
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
1.572239
</td>
<td style="text-align:right;">
1.669396
</td>
<td style="text-align:right;">
1.408171
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
2.714955
</td>
<td style="text-align:right;">
2.823691
</td>
<td style="text-align:right;">
2.406593
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:right;">
2.415092
</td>
<td style="text-align:right;">
2.528913
</td>
<td style="text-align:right;">
2.148041
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:right;">
2.115229
</td>
<td style="text-align:right;">
2.234134
</td>
<td style="text-align:right;">
1.889489
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
</tbody>
</table>
<table>
<thead>
<tr>
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
fixed
</th>
<th style="text-align:right;">
revincr
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1.839406
</td>
<td style="text-align:right;">
1.875260
</td>
<td style="text-align:right;">
1.611007
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
1.534667
</td>
<td style="text-align:right;">
1.581609
</td>
<td style="text-align:right;">
1.351390
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
1.229927
</td>
<td style="text-align:right;">
1.287958
</td>
<td style="text-align:right;">
1.091772
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:right;">
2.207288
</td>
<td style="text-align:right;">
2.250312
</td>
<td style="text-align:right;">
1.933208
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
1.902548
</td>
<td style="text-align:right;">
1.956661
</td>
<td style="text-align:right;">
1.673591
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
1.597808
</td>
<td style="text-align:right;">
1.663010
</td>
<td style="text-align:right;">
1.413974
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.2
</td>
</tr>
<tr>
<td style="text-align:right;">
2.759110
</td>
<td style="text-align:right;">
2.812890
</td>
<td style="text-align:right;">
2.416510
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:right;">
2.454370
</td>
<td style="text-align:right;">
2.519239
</td>
<td style="text-align:right;">
2.156893
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:right;">
2.149630
</td>
<td style="text-align:right;">
2.225588
</td>
<td style="text-align:right;">
1.897276
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
</tbody>
</table>

![](redistribution%20211213%201441_files/figure-gfm/upccpcinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upcctehinc1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upcctehinc2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upccsubsinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upccavprinc-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upccpcdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upcctehdens1-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upcctehdens2-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upccsubsdens-1.png)<!-- -->
![](redistribution%20211213%201441_files/figure-gfm/upccavprdens-1.png)<!-- -->
