setwd("~/Desktop/Bioconductor/scratch")

# Tidyverse
# Motivation: a subset of R functions that work together and preform certain regular tasks in a uniform way i.e. dataframe manipulation

library(dplyr)

#------------------------------------
# tibble

# tibble is a flavor of a dataframe, but it displays dimensions and truncates output automatically and other features
# also give you the type of the column variables
# also drops row names > must specify that you want them as a column
tbl <- as_tibble(mtcars, rownames = "model")

homemade_tbl <- tibble(
    age = c(23, 32, 27),
    city = c("NY", "Buffalo", "Rochester"),
    name = c("A", "B", "C")
)

# data.frame
class(mtcars)

# class gives linear hierarchy of classes
class(tbl)

#------------------------------------------
# pipes

# tidyverse also gives us pipes for passing outputs from one action is the input for another > makes output the first input of the following function
# note, one statement per line
tbl <- 
    mtcars %>%
    as_tibble(rownames = "model")

# R-devel introduced 'native' pipes; tidyverse will move to this next
# syntax error for those not using R-devel
mtcars |>
    as_tibble(rownames = "model")

#------------------------------------------
# non-standard evaluation

# classic R to get mpg
tbl[, "mpg"]

# non-standard evaluation allows you to use unquoted variables to reference tibble columns

#-------------------------------------------
# a few explicit verbs for data transformation
# each verb takes in and outputs a tibble, making them easily chainable

# filtering (row-wise operation)
tbl %>%
    filter(mpg > 20, cyl >=6)
    
# select (column-wise operation)
tbl %>%
    select(model, mpg, cyl, disp)

# filter and select
tbl %>%
    filter(mpg > 15) %>%
    select(model, mpg, cyl, disp)
    
# summarize (reduction i.e. takes columns or rows and reduces them down to a single measure)

tbl %>% 
    summarize(
        mean.mpg = mean(mpg),
        mean.disp = mean(disp)
    )

# group_by (perform calculations group-wise)
tbl %>% 
    group_by(cyl) %>%
    summarize(
        mean.mpg = mean(mpg),
        mean.disp = mean(disp)
    )

# mutate (update or add columns)
tbl %>%
    mutate(
        sqrt.disp = sqrt(disp)
    ) %>%
    select(model, mpg, disp, sqrt.disp)

tbl %>%
    mutate(mpg = -mpg)

# Good tidyverse functions take as their first object the data to be transformed, and return the same class as 