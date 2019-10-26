HW2 Solutions
================

Directions: For this HW, you will create a .Rmd file, and will complete
your HW in this file and knit to HTML. Your final github repository
should contain 1) the .Rmd file and 2) the knit file (make sure to
commit both\!). Also, please make sure that your code chunk for each
question is visible in the HTML document, as that is what we will be
grading. The assignment is due on Tuesday September 24th, at midnight.

## Question 0.

Create a .Rmd file. Add a header to this file that contains 1) author
(your name), 2) a title (Homework 2), and 3) the due date of the HW. For
the remainder of the homework, please create headers for each question,
like below, so we can follow your work.

## Question 1.

Import the HANES dataset (NYC\_HANES\_DIAB.csv) from Github. Remove any
messages that are printed out in the HTML file by adding an argument to
the code chunk header.

``` r
library(RCurl)
URL_text_1 <- "https://raw.githubusercontent.com/NYUClasses/NHANES_datasets/"
URL_text_2 <- "/master/NYC_HANES_DIAB.csv"
# Paste it to constitute a single URL 
URL <- paste(URL_text_1, URL_text_2, sep="")
HANES <- read.csv(text = getURL(URL))
```

## Question 2.

### Part A:

Print out 1) number of observations in the dataset, 2) number of
variables in the dataset, and 3) the names of each of the variables in
the dataset.

``` r
print(nrow(HANES)) # number of observations
```

    ## [1] 1527

``` r
length(HANES) # number of variables
```

    ## [1] 23

``` r
names(HANES)
```

    ##  [1] "KEY"               "GENDER"            "SPAGE"            
    ##  [4] "AGEGROUP"          "HSQ_1"             "UCREATININE"      
    ##  [7] "UALBUMIN"          "UACR"              "MERCURYU"         
    ## [10] "DX_DBTS"           "A1C"               "CADMIUM"          
    ## [13] "LEAD"              "MERCURYTOTALBLOOD" "HDL"              
    ## [16] "CHOLESTEROLTOTAL"  "GLUCOSESI"         "CREATININESI"     
    ## [19] "CREATININE"        "TRIGLYCERIDE"      "GLUCOSE"          
    ## [22] "COTININE"          "LDLESTIMATE"

### Part B:

Based on the variable for Total Cholesterol, create a new factor
variable that labels ‘1’ if \<100, ‘2’ if between 100-200’, ‘3’ if above
200, and ‘Unknown’ if the value is missing. Add this new variable to the
data
frame.

``` r
HANES$CHOLESTEROL_CAT <- as.factor(ifelse(HANES$CHOLESTEROLTOTAL<100, '1',
                                          ifelse(HANES$CHOLESTEROLTOTAL>100 & HANES$CHOLESTEROLTOTAL <200, '2', ifelse(HANES$CHOLESTEROLTOTAL>200, '3', 'Unknown'))))
levels(HANES$CHOLESTEROL_CAT)
```

    ## [1] "1"       "2"       "3"       "Unknown"

## Question 3.

Create a bulleted list that contains 4 bullets- minimum, mean, maximum,
and number of NA’s (missing values) for the variable ‘MERCURYU’.
Italicize the first word of each bullet.

  - *Minimum*: 0.106
  - *Mean*: 0.8140581
  - *Median*: 27.3625887
  - *Missing Values*: 119

## Question 4.

Change the names of the levels of the HSQ\_1 variable to 1- Excellent,
2- Very Good, 3- Good, 4- Fair, and 5-Poor. Then, create an ordered list
(1., 2., etc) that contains the name of each level, and the number of
individuals in the dataset who are in each level. Also, report if there
are any missing data values for the variable, and if so, the number
missing.

``` r
HANES$HSQ_1 <- factor(HANES$HSQ_1)
levels(HANES$HSQ_1) <- c("Excellent","Very Good","Good", "Fair", "Poor")
summary(HANES$HSQ_1)
```

    ## Excellent Very Good      Good      Fair      Poor      NA's 
    ##       256       445       508       257        60         1

1.  Excellent - 256

2.  Very Good - 445

3.  Good - 508

4.  Fair - 257

5.  Poor - 60

6.  Missing Values - 1

## Question 5.

Replicate the graph below. Pay close attention to the axes, title,
labels, colors, etc. Hint: look up the paste() function.

``` r
plot(HANES$GLUCOSE, HANES$GLUCOSESI, 
     xlab= "Plasma Glucose [mg/dL]", 
     ylab = expression(paste("Blood Glucose SI units [  ", mu, "mole/L]")), 
     main = "Plasma vs Blood Glucose", type = "o", col="blue", lwd = 1, 
     yaxt='n', cex.axis = 1.5)
axis(2, seq(5, 25, 10), las=2, cex.axis = 1.5)
text(220, 25, paste("Model : y= ", 6.9e-05, "*x"))
```

![](HW2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Question 6.

Plot the distribution of A1C for the male and female populations in the
HANES data set. Are they different?

``` r
male_pop <- which(HANES$GENDER == 2)
female_pop <- which(HANES$GENDER == 1)

hist(HANES$A1C[male_pop],
     main = "Histogram of A1C for the male population",
     xlab = "A1C")
```

![](HW2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
hist(HANES$A1C[female_pop],
     main = "Histogram of A1C for the female population",
     xlab = "A1C")
```

![](HW2_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

## Question 7.

Add vertical lines that indicate the boundaries of the standard error of
the mean of A1C for the female population in the HANES data set. Hint:
std.error().

``` r
library(plotrix)

# calculate std.err of mean 
se <- std.error(HANES$A1C[female_pop], na.rm = TRUE) 

#sd(HANES$A1C[female_pop], na.rm = TRUE)/sqrt(sum(!is.na(HANES$A1C[female_pop])))

m <- mean(HANES$A1C[female_pop], na.rm = TRUE)

hist(HANES$A1C[female_pop],
     main = "Histogram of A1C for the female population",
     breaks = 100) 
abline(v = m-se, col = "red", lwd = 2)
abline(v = m+se, col = "red", lwd = 2)
abline(v = m, col = "green", lwd = 2)
```

![](HW2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Question 8.

### Part A:

Plot the distribution of one more numeric variable (other than A1C) for
the three age-groups.

``` r
age_group_1 <- which(HANES$AGEGROUP == 1)
age_group_2 <- which(HANES$AGEGROUP == 2)
age_group_3 <- which(HANES$AGEGROUP == 3)


hist(HANES$UCREATININE[age_group_1], 
     main = "Histogram of HANES of UCREATININE AGEGROUP 1",
     xlab = "UCREATININE")
```

![](HW2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
hist(HANES$UCREATININE[age_group_2],
      main = "Histogram of HANES of UCREATININE AGEGROUP 2",
     xlab = "UCREATININE")
```

![](HW2_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
hist(HANES$UCREATININE[age_group_3],
      main = "Histogram of HANES of UCREATININE AGEGROUP 3",
     xlab = "UCREATININE")
```

![](HW2_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

### Part B:

Re-plot the plots from question 8A with three different bin sizes. What
happens?

## Question 9.

Replicate this plot of LDL vs A1C from the HANES dataset. Try using
seq() and rep() and look up the function points().  
![](HW2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Question 10.

Check the Hmisc::label() function. Think about how one can leverage this
function to save some typing when plotting several graphs with the same
variable. Give an example.

``` r
plot(HANES$A1C, 
     HANES$GLUCOSE, main="Glucose vs. A1C",
     xlab="A1C and this and some more description. More description. More description",
     ylab="Glucose", 
     col.main= "Blue")
```

![](HW2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(HANES$A1C, 
     HANES$UCREATININE, main="Glucose vs. A1C",
     xlab="A1C and this and some more description. More description. More description",
     ylab="UCREATININE",
     col.main= "Blue")
```

![](HW2_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
plot(HANES$A1C, 
     HANES$MERCURYU, main="Glucose vs. A1C",
     xlab="A1C and this and some more description. More description. More description",
     ylab="MERCURYU",
     col.main= "Blue")
```

![](HW2_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
Hmisc::label(HANES$A1C) <- "A1C and this and some more description. More description. More description"

plot(HANES$A1C, 
     HANES$GLUCOSE, main="Glucose vs. A1C",
     xlab=Hmisc::label(HANES$A1C),
     ylab="Glucose", 
     col.main= "Blue")
```

![](HW2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plot(HANES$A1C, 
     HANES$UCREATININE, main="Glucose vs. A1C",
     xlab=Hmisc::label(HANES$A1C),
     ylab="UCREATININE",
     col.main= "Blue")
```

![](HW2_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
plot(HANES$A1C, 
     HANES$MERCURYU, main="Glucose vs. A1C",
     xlab=Hmisc::label(HANES$A1C),
     ylab="MERCURYU",
     col.main= "Blue")
```

![](HW2_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
plot(HANES$A1C, 
     HANES$MERCURYU)
abline(a = 5, b = 1)
```

![](HW2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Question 11.

### Part A:

Check the distribution of log transformed A1C.

### Part B:

Plot log-transformed A1C against SPAGE. Annotate (axes, title) and
prettify the graph in a way you think shows the data best. Center the
graph in the HTML file.

``` r
  # Plot log-A1C vs SPAGE
plot(HANES$SPAGE, log(HANES$A1C), 
     xlab= "Age",
     ylab = "log-A1C", 
     main = "log-A1C by Age", 
     col="blue")
```

<img src="HW2_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

## Question 12.

What does the plot in question 11 tell us?

## Question 13.

Plot (1) log-A1C vs. HDL (2) log-A1C vs. LDLESTIMATE, and (3) log-A1C
vs. TRIGLYCERIDE in one 1x3 figure. Make sure to include labels on each
axes and titles. Use the attach() function to shorten the variable
names.

``` r
  attach(HANES)
  par(mfrow = c(1,3))
  plot(HDL, log(A1C), xlab= "HDL",ylab = "log-A1C", main = "A1C by HDL")
  plot(LDLESTIMATE, log(A1C), xlab= "LDL",ylab = "log-A1C", main = "A1C by LDL")
  plot(TRIGLYCERIDE, log(A1C), xlab= "TRI",ylab = "log-A1C", main = "A1C by TRI")
```

![](HW2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Question 14.

### Part A:

Create two vectors, each with four elements: c(TRUE,FALSE,1,0) and
c(FALSE,TRUE,1,8).

``` r
x <- c(TRUE,FALSE,1,0)
y <- c(FALSE,TRUE,1,8)
```

### Part B:

Examine the 4 logical operators: &, &&, |, || between the two vectors
from part A. Explain the differences you see and why.

``` r
x&y
```

    ## [1] FALSE FALSE  TRUE FALSE

``` r
x&&y
```

    ## [1] FALSE

``` r
x|y
```

    ## [1] TRUE TRUE TRUE TRUE

``` r
x||y
```

    ## [1] TRUE

## Question 15.

### Part A:

Take a random number (age) between 0~100.

``` r
age <-  runif(1,0,100)
```

### Part B:

Create an if() condition that tells whether or not the age in part A is
elderly, where elderly is defined as age \>= 65.

``` r
if (age >= 65) {
    print('Elderly')
} else {
    print('Not Elderly')  
}
```

    ## [1] "Not Elderly"

``` r
age
```

    ## [1] 26.06336

### Part C:

Using the ifelse() condition, write code that returns categorical age
groups (\<18:child, \>=18 and \<65: adult, \>=65 elderly).

``` r
agegroup <- ifelse(age<18,"under",
       ifelse(age>=18 & age<65, "adult","elderly"))

agegroup
```

    ## [1] "adult"
