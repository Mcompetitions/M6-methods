Contains the data and code used within the M6 International Journal of Forecasting main paper (available at https://arxiv.org/abs/2310.13357) for analyzing the results of the competition.

Below you may find a brief description of each file:

* **assets_m6.csv**: Daily adjusted close prices for the M6 assets.

* **submissions.csv**: Submissions made by the participating teams (team id, month of submission, month used for evaluation purposes, asset symbol, investment weight, probability of being ranked 1-5, indication whether the team is eligible).

* **summary_leaderboard.xlsx**: Summarizes the performance (leaderboard) of the participating teams per month, quarter and in total. The file also included in separate tabs the information available in the "assets_m6.csv" and "submissions.csv" files.

* **Baseline for evaluating the hypotheses.R**: Used for processing the original submissions and computing key statistics, such as returns, risk, IR, RPS and OR per month, quarter and in total. The output of this script ("Score compute.Rdata") is used as input in scripts Hypothesis1.R, Hypothesis2.R, ... Hypothesis8.R.

* **HypothesisX.R**: A separate script used for evaluating hypotheses 1-8.

* **IR and RPS evolution.R**: Used to demonstrate the daily evolution of the IR and RPS scores of the participating teams.

* **Better than the benchmark.R**: Used to identify the teams that performed better than the benchmark overall, as well as per month in terms of IR and RPS.

* **submission stats.R**: Used to extract summary statistics of the submissions.



