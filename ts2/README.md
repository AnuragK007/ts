# ts2

Time Series Analysis practical code (Sets 1 to 6, AirPassengers data), split topic-wise.

## Install

```r
install.packages("remotes")
remotes::install_github("AnuragK007/ts", subdir = "ts2")
library(ts2)
ts_install_deps()   # one time: installs forecast, tseries, lmtest, FinTS, rugarch
```

## Use

```r
ts_index()                 # list every set and its topics

set1()                     # print all code of Set 1
set1(1)                    # print only Topic 1 of Set 1
set1("AR")                 # pick a topic by a word in its title
set1(2, run = TRUE)        # run Topic 2 of Set 1 (code + output + plots)
set1(1, copy = TRUE)       # print and copy to clipboard (needs clipr)
```

`set1` ... `set6` all work the same way.
