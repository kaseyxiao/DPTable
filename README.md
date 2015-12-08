## DPTable
A differentially private tabular data publishing algorithm using sampling and junction tree inference

Tips for installation:
Required R library:  R.oo; methods; R.matlab; data.table; distr; entropy; plyr; FNN; e1071; stringr; 
MAKE SURE you have MATLAB installed, otherwise the script will keep waiting to connect matlab.


In Rstudio, 
first, choose the working directory to the source code directory,

then run
```sh
> system("Rscript exp/run-category.R -f Data3 -e1 0.2 -e2 0.8")
```
```sh
> system("Rscript exp/run-category.R -f Data3 -e1 0.2 -e2 0.8 -nrun 1 -CV 0.3")
```

###Arguments:
* -f: dataset name
* -e1: epsilon value for constructing noisy junction tree
* -e2: epsilon value for injecting noise into the marginal tables
* -nrun: num of runs, 10 as default
* -CV: numeric value in (0, 1),  threshold  Cramer’s V value for picking correlated attributes pairs, 0.2 as default,typically choose 0.2 for weakly correlated datasets; 0.3 for highly correlated datasets
* -q: boolean value indicates whether to process query on the releasing tables, True as default