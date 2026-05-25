Exploring Heterogeneity in MetaBus Effect Sizes
================
25 May, 2026

This manuscript uses the Workflow for Open Reproducible Code in Science
(**vanlissaWORCSWorkflowOpen2021?**) to ensure reproducibility and
transparency. All code <!--and data--> are available at
<https://github.com/cjvanlissa/alper_country_conspiracy.git>.

This is an example of a non-essential citation
(**vanlissaWORCSWorkflowOpen2021?**). If you change the rendering
function to `worcs::cite_essential`, it will be removed.

<!--The function below inserts a notification if the manuscript is knit using synthetic data. Make sure to insert it after load_data().-->

## Results

These are the Rsquared values on training and test data (use test data
to determine unbiased performance estimates):

|  mse | mse_se | rsq_test | rsq_train | model      |
|-----:|-------:|---------:|----------:|:-----------|
| 1.17 |   0.91 |     0.07 |     -0.02 | brma       |
| 0.97 |   0.99 |    -0.09 |     -0.03 | metaforest |
| 2.09 |   0.38 |    -0.16 |      0.11 | mlrf       |

The best performing model (or interpretable model whose cross-validated
mean squared error was within 1SE of the best model’s cross-validated
mean squared error) is brma.

|                        |  mean | se_mean |   sd |  2.5% |  25% |  50% |  75% | 97.5% |   n_eff | Rhat |
|:-----------------------|------:|--------:|-----:|------:|-----:|-----:|-----:|------:|--------:|-----:|
| Intercept              |  0.70 |    0.01 | 0.23 |  0.24 | 0.57 | 0.71 | 0.84 |  1.15 |  327.00 | 1.01 |
| Pub.YearPre_CCSSM      |  0.00 |    0.00 | 0.04 | -0.04 | 0.00 | 0.00 | 0.00 |  0.08 | 2260.86 | 1.00 |
| DesignRCT              |  0.01 |    0.00 | 0.06 | -0.04 | 0.00 | 0.00 | 0.00 |  0.12 |  887.17 | 1.00 |
| ControlBAU             |  0.00 |    0.00 | 0.04 | -0.05 | 0.00 | 0.00 | 0.00 |  0.07 | 2097.45 | 1.00 |
| CriteriaMulti.         |  0.00 |    0.00 | 0.03 | -0.06 | 0.00 | 0.00 | 0.00 |  0.05 | 1639.71 | 1.00 |
| CriteriaOther          |  0.00 |    0.00 | 0.05 | -0.04 | 0.00 | 0.00 | 0.00 |  0.10 | 1455.37 | 1.01 |
| GenderM                |  0.01 |    0.00 | 0.05 | -0.04 | 0.00 | 0.00 | 0.00 |  0.08 |  846.65 | 1.00 |
| EthnictyMaj..W         |  0.00 |    0.00 | 0.05 | -0.10 | 0.00 | 0.00 | 0.00 |  0.04 | 1565.72 | 1.00 |
| EthnictyNR             |  0.01 |    0.00 | 0.06 | -0.05 | 0.00 | 0.00 | 0.00 |  0.17 |  580.16 | 1.01 |
| ImplementerOther       |  0.00 |    0.00 | 0.03 | -0.07 | 0.00 | 0.00 | 0.00 |  0.04 | 1677.00 | 1.00 |
| ImplementerRes.        |  0.00 |    0.00 | 0.03 | -0.04 | 0.00 | 0.00 | 0.00 |  0.06 | 1767.12 | 1.01 |
| ImplementerTea.        |  0.00 |    0.00 | 0.03 | -0.05 | 0.00 | 0.00 | 0.00 |  0.06 | 2205.08 | 1.00 |
| SettingNon.GEd.        |  0.00 |    0.00 | 0.04 | -0.06 | 0.00 | 0.00 | 0.00 |  0.06 |  883.27 | 1.01 |
| GroupGroup_Based       |  0.00 |    0.00 | 0.03 | -0.06 | 0.00 | 0.00 | 0.00 |  0.04 | 3651.39 | 1.00 |
| Frequency3x.or.4x      |  0.00 |    0.00 | 0.04 | -0.04 | 0.00 | 0.00 | 0.00 |  0.08 | 2440.08 | 1.00 |
| Frequency5x            |  0.00 |    0.00 | 0.04 | -0.07 | 0.00 | 0.00 | 0.00 |  0.04 | 2386.60 | 1.00 |
| Sessions..30..Ses.     |  0.00 |    0.00 | 0.03 | -0.04 | 0.00 | 0.00 | 0.00 |  0.05 | 3234.68 | 1.00 |
| Duration..10.hrs       |  0.00 |    0.00 | 0.04 | -0.05 | 0.00 | 0.00 | 0.00 |  0.05 |  419.66 | 1.01 |
| FOI1                   |  0.00 |    0.00 | 0.03 | -0.07 | 0.00 | 0.00 | 0.00 |  0.04 | 1095.54 | 1.00 |
| Funded1                | -0.02 |    0.00 | 0.09 | -0.35 | 0.00 | 0.00 | 0.00 |  0.03 |  470.46 | 1.01 |
| MeasureSTZ             | -0.01 |    0.00 | 0.04 | -0.10 | 0.00 | 0.00 | 0.00 |  0.04 |  893.99 | 1.00 |
| OperationOther         |  0.01 |    0.00 | 0.08 | -0.04 | 0.00 | 0.00 | 0.00 |  0.20 |  651.17 | 1.00 |
| OutcomeFF              |  0.14 |    0.01 | 0.15 | -0.01 | 0.00 | 0.08 | 0.26 |  0.43 |  225.08 | 1.03 |
| OutcomeOther           | -0.01 |    0.00 | 0.06 | -0.21 | 0.00 | 0.00 | 0.00 |  0.02 |  674.22 | 1.01 |
| OutcomeWPS             | -0.02 |    0.00 | 0.06 | -0.20 | 0.00 | 0.00 | 0.00 |  0.03 | 1259.03 | 1.01 |
| X.Interv.Led.Inst.1    |  0.01 |    0.00 | 0.06 | -0.02 | 0.00 | 0.00 | 0.00 |  0.21 |  462.47 | 1.02 |
| Explicit.In1           |  0.00 |    0.00 | 0.04 | -0.04 | 0.00 | 0.00 | 0.00 |  0.07 | 2174.40 | 1.00 |
| X.Concrete.Rep..1      |  0.00 |    0.00 | 0.03 | -0.05 | 0.00 | 0.00 | 0.00 |  0.04 | 1514.36 | 1.00 |
| Mult.Rep1              |  0.00 |    0.00 | 0.04 | -0.09 | 0.00 | 0.00 | 0.00 |  0.05 |  363.82 | 1.01 |
| X.Specific.Facts.1     |  0.00 |    0.00 | 0.03 | -0.04 | 0.00 | 0.00 | 0.00 |  0.06 | 2719.51 | 1.00 |
| X.Counting.Alg.Inst..1 |  0.00 |    0.00 | 0.03 | -0.04 | 0.00 | 0.00 | 0.00 |  0.06 | 2217.94 | 1.00 |
| X.Timed.Fluency.1      |  0.00 |    0.00 | 0.03 | -0.03 | 0.00 | 0.00 | 0.00 |  0.05 | 3333.63 | 1.00 |
| X.Untimed.Fluency.1    |  0.00 |    0.00 | 0.03 | -0.05 | 0.00 | 0.00 | 0.00 |  0.05 | 1413.07 | 1.00 |
| Games1                 |  0.00 |    0.00 | 0.03 | -0.05 | 0.00 | 0.00 | 0.00 |  0.05 | 2243.36 | 1.00 |
| Indep.Prac.1           | -0.02 |    0.00 | 0.08 | -0.28 | 0.00 | 0.00 | 0.00 |  0.03 |  230.57 | 1.01 |
| Corr.Feedback1         |  0.00 |    0.00 | 0.03 | -0.06 | 0.00 | 0.00 | 0.00 |  0.04 | 2856.09 | 1.00 |
| Err.Corr.Proc.1        |  0.00 |    0.00 | 0.04 | -0.10 | 0.00 | 0.00 | 0.00 |  0.04 | 1462.91 | 1.00 |
| Incentives1            |  0.00 |    0.00 | 0.03 | -0.07 | 0.00 | 0.00 | 0.00 |  0.04 | 2231.38 | 1.01 |
| tau2_w                 |  0.15 |    0.00 | 0.03 |  0.11 | 0.13 | 0.15 | 0.17 |  0.21 |  840.78 | 1.00 |
| tau2_b                 |  0.81 |    0.01 | 0.26 |  0.43 | 0.63 | 0.77 | 0.95 |  1.41 | 1143.34 | 1.00 |
