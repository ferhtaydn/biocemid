## McNemar's Test
[McNemar's test](https://www.wikiwand.com/en/McNemar's_test)
[Calculator](https://graphpad.com/quickcalcs/McNemar1.cfm)
[Calculator 2](https://scistatcalc.blogspot.com.tr/2013/11/mcnemars-test-calculator.html)

- Chi-squared statistic with Yates correction of `1.0`
- `95%` Confidence interval
- The calculated `p-value < 0.05` could be considered a significant result, rejecting the `Null Hypothesis` (compared two systems are not different).

```
gold: word2vec, test: word2vec_genia_ino
TN -> 586.0, FN -> 49.0, TP -> 377.0, FP -> 0.0
accuracy: 0.9515810276679841
precision: 1.0
recall: 0.8849765258215962
fscore: 0.9389788293897883

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 47.020 with 1 degrees of freedom. 
```

```
gold: baseline, test: baseline_genia_ino
TN -> 701.0, FN -> 25.0, TP -> 215.0, FP -> 0.0
accuracy: 0.973432518597237
precision: 1.0
recall: 0.8958333333333334
fscore: 0.945054945054945

Corresponding p-value is 0.000002
The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 23.040 with 1 degrees of freedom. 
```

```
gold: baseline, test: word2vec
TN -> 585.0, FN -> 7.0, TP -> 214.9615396139974, FP -> 215.0384603860026
accuracy: 0.782741232499019
precision: 0.4999105572418544
recall: 0.9684630048423102
fscore: 0.6594301244863868

McNemar chi-squared statistic with Yates correction of 1.0 is 193.013514
Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 193.014 with 1 degrees of freedom. 

The odds ratio is 30.714, with a 95% confidence interval extending from 14.653 to 77.312
```

```
gold: baseline_genia_ino, test: word2vec_genia_ino
TN -> 629.0, FN -> 6.0, TP -> 190.9615396139974, FP -> 190.03846038600258
accuracy: 0.8070487594625958
precision: 0.5012113900629853
recall: 0.9695371999439143
fscore: 0.660810543696505

McNemar chi-squared statistic with Yates correction of 1.0 is 170.862245
Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 170.862 with 1 degrees of freedom. 

The odds ratio is 31.667, with a 95% confidence interval extending from 14.282 to 87.375 
```

```
gold: baseline, test: tfrf7
TN -> 445.0, FN -> 0.0, TP -> 218.790600507391, FP -> 622.2093994926089
accuracy: 0.5161668744225435
precision: 0.260155291923176
recall: 1.0
fscore: 0.4128940196349008

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 620.002 with 1 degrees of freedom. 
```

```
gold: baseline, test: tfrf10
TN -> 304.0, FN -> 0.0, TP -> 212.46757659826125, FP -> 1208.532423401739
accuracy: 0.29940149368015145
precision: 0.14951975833797412
recall: 1.0
fscore: 0.26014299841902033

McNemar chi-squared statistic with Yates correction of 1.0 is 1206.000828
Corresponding p-value is 0.000000
```

```
gold: baseline, test: tfrfmanual
TN -> 633.0, FN -> 0.0, TP -> 224.10580249765727, FP -> 128.89419750234276
accuracy: 0.8692756617623298
precision: 0.6348606303049781
recall: 1.0
fscore: 0.7766541300667896

McNemar chi-squared statistic with Yates correction of 1.0 is 126.007813
Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 126.008 with 1 degrees of freedom. 
```

```
gold: baseline_genia_ino, test: tfrf7_genia_ino
TN -> 506.0, FN -> 0.0, TP -> 196.88858025579924, FP -> 554.1114197442007
accuracy: 0.559179459232935
precision: 0.2621685489424757
recall: 1.0
fscore: 0.41542557713411105

Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 552.002 with 1 degrees of freedom. 
```

```
gold: baseline_genia_ino, test: tfrf10_genia_ino
TN -> 382.0, FN -> 0.0, TP -> 190.5655563466695, FP -> 1097.4344436533306
accuracy: 0.34285362655489193
precision: 0.14795462449275582
recall: 1.0
fscore: 0.2577708584224436

McNemar chi-squared statistic with Yates correction of 1.0 is 1095.000912
Corresponding p-value is 0.000000
```

```
gold: baseline_genia_ino, test: tfrfmanual_genia_ino
TN -> 666.0, FN -> 0.0, TP -> 199.10580249765724, FP -> 115.89419750234275
accuracy: 0.8818611646255425
precision: 0.6320819126909754
recall: 1.0
fscore: 0.7745713101480295

Corresponding p-value is 0.000000
The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 113.009 with 1 degrees of freedom. 
```

```
gold: tfrf7, test: tfrf7_genia_ino
TN -> 445.0, FN -> 89.0, TP -> 747.0, FP -> 0.0
accuracy: 0.9305230288836847
precision: 1.0
recall: 0.8935406698564593
fscore: 0.9437776373973468

McNemar chi-squared statistic with Yates correction of 1.0 is 87.011236
Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 87.011 with 1 degrees of freedom. 
```

```
gold: tfrf10, test: tfrf10_genia_ino
TN -> 304.0, FN -> 132.0, TP -> 1282.0, FP -> 0.0
accuracy: 0.9231664726426076
precision: 1.0
recall: 0.9066478076379066
fscore: 0.9510385756676557

McNemar chi-squared statistic with Yates correction of 1.0 is 130.007576
Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 130.008 with 1 degrees of freedom. 
```

```
gold: tfrfmanual, test: tfrfmanual_genia_ino
TN -> 633.0, FN -> 38.0, TP -> 313.0, FP -> 0.0
accuracy: 0.9613821138211383
precision: 1.0
recall: 0.8917378917378918
fscore: 0.9427710843373494

McNemar chi-squared statistic with Yates correction of 1.0 is 36.026316
Corresponding p-value is 0.000000

The two-tailed P value is less than 0.0001
By conventional criteria, this difference is considered to be extremely statistically significant.

The P value was calculated with McNemar's test with the continuity correction.
Chi squared equals 36.026 with 1 degrees of freedom. 
```

