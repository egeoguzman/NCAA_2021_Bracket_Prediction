## A comparative analysis of machine learning algorithms in predicting 2021 NCAAM March Madness outcomes
Predicting college basketball results is both chal- lenging and an interesting problem to solve. Organized college leagues in the Unites States are receiving incredible attention. These leagues have also become a field for sports analysts, data scientists and even academics to work on. Teams, players and even coaches are compared according to season statistics and predictions are made about which teams is better for that season. In this study, the match statistics of the NCAA Men’s Basketball League between 2003 and 2020 were collected and analyzed. The main purpose is to predict which of the two teams will win the match in 2021 March Madness which is the most important tournament of the NCAA Basketball league. This means that the binary classification problem must be solved. These collected data were trained with machine learning algorithms (namely xgboost, logistic regression, kNN, random forest, decision tree) and predictions were produced. Xgboost and logistic regression yield similar results, with an accuracy of over 92%.

### Dataset
Dataset have been published for the March Machine Learning Mania 2021 - NCAAM competition on Kag- gle [15]. It is available on https://www.kaggle.com/c/ncaam-march-mania-2021.

Elo and Insemination values were provided by two different sports statisticians. Ken Pomeroy (https://kenpom.com) and Kenneth Massey (MMasseyOrdinals) are two famous United State sports statis- ticians. They estimate and publish these values every year with the elo rating and seeding estimation systems they have established. Another team ranking system that provides data to this study is created by Warren Nolan (https://www.warrennolan.com/basketball/2021/elochess).

### Results

![alt text](https://github.com/egeoguzman/NCAA_2021_Bracket_Prediction/blob/main/graphs/results.png "results")

When Table is examined, it is seen that logistic regression gives the highest performance with an accuracy score of 0.95. In addition, xgboost and random forest algorithms followed logistic regression with their performances above 0.90, while other algorithms were far below the expected performance.

### Predicted Bracket

![alt text](https://github.com/egeoguzman/NCAA_2021_Bracket_Prediction/blob/main/bracket/prediction_bracket.png "results")

The logistic regression model fed by basketball and match statistics gave the highest performance with an accuracy of 0.95 in the problem that was tried to be solved by using machine learning algorithms such as xgboost, kNN, decision tree and random forest. However, as a result of the study, it was concluded that no matter how effective the model selection is, the main key operation is feature engineering and feature selection.
It has been proven that more successful results can be achieved with the help of more detailed statistics and mathe- matics for further studies, examining each match and correct feature engineer techniques.
