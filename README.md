# Psychophysiology2017
R scripts and dataset from Psychophysiology paper:

Ronan McGarrigle, Piers Dawes, Andrew Stewart, Stefanie Kuchinsky, & Kevin Munro (2017). Pupillometry reveals changes in physiological arousal during a sustained listening task, Psychophysiology, 5, 193-203.

The within-trial pupil scaling method is reported in the above paper as dividing each data point by the mean of the entire trial. However, please note that the scale() function in R actually normalises data by subtracting the mean from each data point and dividing by the standard deviation (i.e., creates z-scores). Thank you to Graham Naylor and Defne Alfandari Menase for bringing this to my attention!
