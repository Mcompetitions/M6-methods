* **assets_m6.csv**: Daily adjusted close prices of the M6 assets (symbol, date, price).

* **submissions.csv**: Submissions made by the participating teams. This includes the id of the team (Team), month of submission (Submission), month the submissions was used for evaluation purposes (Evaluation), asset symbol (Symbol), investment weight (Decision), probability that the asset is ranked 1-5 (Rank1, Rank2, Rank3, Rank4, Rank5), and an indication whether the team is active and eligible for a prize (IsActive).

* **Evaluation - example.xlsx**: Example of IR and RPS calculations in an excel file, assuming benchmark submissions.

Also, **R** and **Python** scripts to:

* Download historical prices data for the M6 universe assets 
Here, we provide an example when the free-to-use Yahoo API is employed, but other APIs may be more appropriate for collecting historical data. 
After the first two months of the competition, the M6 has been fetching data using the API provided by https://eodhistoricaldata.com/.

* Compute RPS and IR for a given evaluation period
An excel file providing an example of how the RPS and IR can be computed for a dummy submission (similar to the template of the competition). 
The example is given for the first quanrter of the M6 and the pilot phase.
