1) Created an aggregate Excel file ("aggregate") with all the time series with missing values, each "interaction"/edge in a worksheet.
2) Implemented data cleaning techniques to fill some gaps in years and highlight the missing data.
3) Created an Excel file with the percentage of missing data for each time series 
4) A first attempt to fill the missing data based on their percentage in each time series 
      [a] Forward fill or Backward Fill for <= 10% of missing data
      [b] Linear interpolation for 10-50%
      [c] Mean imputation for > 50%: 
5) Our last attempt is based on the Expectation Maximisation (EM)* method of filling missing data. 
6) Some countries have zero reported interaction**, hence they are excluded from the analysis.


*
The Expectation-Maximization (EM) algorithm is a powerful statistical technique used to find maximum likelihood estimates of parameters in models with incomplete data.
Expectation (E-step): Estimate the missing data based on the observed data and the current estimates of the model parameters.
Maximization (M-step): Update the model parameters by maximizing the likelihood function, treating the estimated missing data as if they were the true values.
**
- Germany -> Croatia
- Luxembourg -> Out
- Self-loops -> Out
- Netherlands -> Belgium, Estonia, Croatia, Lithuania, Latvia, Slovenia, Slovakia
- Portugal -> Estonia
