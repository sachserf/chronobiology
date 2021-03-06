# chronobiology
Many biological datasets include information about date and time (e.g. telemetry data, camera traps, observations...). This R-package provides functions to get additional information from this kind of data.

## case examples
Many species show diurnal movement pattern due to different lighting conditions. Additionally some species adapt to repeated disturbance events especially when time and place is predictable (e.g. hikers travelling along a path). Following this it could be of interest to classify your data according to specified points in time (e.g. Sunday noon, time before and after sunrise/sunset, night or day...). The functions within the chronobiology-package provide additional information for further analysis of this topic.

## references
The formula within the function twilight() was adapted from the following webpage:
[http://lexikon.astronomie.info/zeitgleichung/](http://lexikon.astronomie.info/zeitgleichung/)

The function stimuli_distance() uses external functions from the geosphere-package.

## Installation
devtools::install_github("sachserf/chronobiology")
