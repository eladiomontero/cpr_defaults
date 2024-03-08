# Data analysis code for the Common Pool Resource experimental data

The data used in this experiment is available in [this link](https://zenodo.org/records/10228658)

In the file called `all_participants.csv` is the full dataset of all participants that took part of the experiment. This includes participants who will end up excluded and dropouts.
To filter the ones that were used for the analysis, the Python Notebook `data_analysis.ipynb` filters out the excluded participants. For more information on the criteria for exclusion, see Methods of the paper.
The Python notebook also creates two datasets: `data_long_format.csv` and `data_wide_format.csv` where the latter is the format that Prolific gives for the experimental data. 
In `data_long_format.csv` there is a row for each round, for the analysis over time.

The R script `data_analysis.R` is used for modelling and some statistical tests.
