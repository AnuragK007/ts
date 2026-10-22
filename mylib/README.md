# mylib — Time Series Analysis Practical Codebook

This R package puts your six sets of complete practical code in the RStudio Help system.

## Install from a local folder
Open the `mylib` folder as a project in RStudio, then run:

```r
install.packages("devtools")
devtools::install()
```

## Open a set
```r
library(mylib)
?set1
?set2
?set3
?set4
?set5
?set6
help(package = "mylib")
```

Each Help page displays the code from the provided Time Series Analysis Practical Codebook as text, so it can be selected and copied. Opening a Help page does not execute the practical code.

## Install from GitHub on another PC
After uploading the contents of this folder to a public GitHub repository named `mylib`, run:

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/mylib")
```

Replace `YOUR_GITHUB_USERNAME` with your GitHub username.
