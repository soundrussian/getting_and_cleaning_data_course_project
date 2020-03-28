# What is this

This a Getting and Cleaning Data Course Project for [R Course by John Hopkins University](https://www.coursera.org/learn/data-cleaning/home/welcome).

# How to get the tidy data?

Just run `run_analysis.R` script. It will download messy source data, combine test and train, filter our the required columns, replace activities with human readable names, group data by activity and subject and calculate the mean of each variable, and at last output the result to `./output/tidy_set.txt`. The result is commited to this repo, but if you want to redo the cleaning, go ahead - the file will be overwritten.

# How did you get the clean data? Which steps did you take?

First, I examined the source files. It became clear, that `X_`, `Y_` and `subject_` files contained different variables of the same observations, so the first step was to combine them into one data set. `cbind` worked nicely.

Then, I combined train and test data with the help of `rbind`.

The next step was to leave only `mean` and `std` variables. First, I filled in colnames for my data from the `features.txt` file in the source, and then filtered only those having `mean(` or `std(` in their names.

After this I cleaned up column names, removing brackets, converting them to lowercase, and replacing dashes with underscores.

Then I factorized activity codes by labeling them with the data from `activity_labels.txt`.

The last thing to do was to group data by subject and activity and summarize it by calculating mean value for each observation. It was a breeze with `dyplr`. 

The result was written as CSV with headers and comma as separator to `./output/tidy_set.txt`

# Things to improve

I'm new to R, so the code may be not very efficient and not very idiomatic. I wonder if I could use `dyplr` to change column names, select only those I needed, etc. Anyway, I think that the script does its job as it is.