Contains the data and code used within the M6 International Journal of Forecasting main paper (available at https://arxiv.org/abs/2310.13357) for analyzing the results of the competition.

Below you may find a brief description of each file:

* **assets_m6.csv**: Daily adjusted close prices for the M6 assets (symbol, date, price).

* **submissions.csv**: Submissions made by the participating teams (team id - Team, month of submission - Submission, month used for evaluation purposes - Evaluation, asset symbol - Symbol, investment weight - Decision, probability of being ranked 1-5 - Rank1 to Rank5, indication whether the team is active and eligible for any prize - IsActive).

* **summary_leaderboard.xlsx**: Summarizes the performance (leaderboard) of the participating teams per month, quarter and in total. The file also includes in separate tabs the information available in the "assets_m6.csv" and "submissions.csv" files. Moreover, for each team, it displays the evolution of its RPS, IR and OR scores.

* **performance_vs_answers_unique.csv**: Provides - based on the answers of the participants to the questionnaire - an indicative classification (TS, ML, Combination, Judgment, Related, NotRelated) of the methods used by the participating teams.

* **Baseline for evaluating the hypotheses.R**: Used for processing the original submissions and computing key statistics, such as returns, risk, IR, RPS and OR per month, quarter, and in total. The output of this script ("Score compute.Rdata") is used as input in scripts "Hypothesis1.R", "Hypothesis2.R", ... "Hypothesis9_10.R".

* **IR and RPS evolution.R**: Used to demonstrate the daily evolution of the IR and RPS scores of the participating teams.

* **Better than the benchmark.R**: Used to identify the teams that performed better than the benchmark overall and per month in terms of IR and RPS.

* **submission stats.R**: Used to extract summary statistics of the submissions, including the number of participating teams and those that updated their submissions regularly.

* **HypothesisX.R**: A set of scripts used for evaluating the ten hypotheses of the M6 made before launching the competition.

* **forecast-investing connection.R**: Used to evaluate the degree of connection between the forecasts (predicted rank) and the decisions (investment weights). The analysis effectively classifies the submissions of the participating teams into "Well Connected", "Connected", "Weakly Connected", "Disconnected" and "Opposite Connection".

