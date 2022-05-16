---
title: "PSTAT131HW05"
author: "Yifei Zhang"
date: '2022-05-10'
output: pdf_document
toc_float: true
code_folding: show
---



```r
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(klaR)
library(glmnet)
tidymodels_prefer()
Pokemon <- read_csv("Pokemon.csv")
pokemon <- readr::spec(Pokemon)
pokemon
```

```
## cols(
##   `#` = col_double(),
##   Name = col_character(),
##   `Type 1` = col_character(),
##   `Type 2` = col_character(),
##   Total = col_double(),
##   HP = col_double(),
##   Attack = col_double(),
##   Defense = col_double(),
##   `Sp. Atk` = col_double(),
##   `Sp. Def` = col_double(),
##   Speed = col_double(),
##   Generation = col_double(),
##   Legendary = col_logical()
## )
```

```r
library(janitor)
```


### Exercise 1

Install and load the `janitor` package. Use its `clean_names()` function on the Pokémon data, and save the results to work with for the rest of the assignment. What happened to the data? Why do you think `clean_names()` is useful?


```r
cleaned <- clean_names(Pokemon)
cleaned
```

```
## # A tibble: 800 x 13
##    number name      type_1 type_2 total    hp attack defense sp_atk sp_def speed
##     <dbl> <chr>     <chr>  <chr>  <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <dbl>
##  1      1 Bulbasaur Grass  Poison   318    45     49      49     65     65    45
##  2      2 Ivysaur   Grass  Poison   405    60     62      63     80     80    60
##  3      3 Venusaur  Grass  Poison   525    80     82      83    100    100    80
##  4      3 Venusaur~ Grass  Poison   625    80    100     123    122    120    80
##  5      4 Charmand~ Fire   <NA>     309    39     52      43     60     50    65
##  6      5 Charmele~ Fire   <NA>     405    58     64      58     80     65    80
##  7      6 Charizard Fire   Flying   534    78     84      78    109     85   100
##  8      6 Charizar~ Fire   Dragon   634    78    130     111    130     85   100
##  9      6 Charizar~ Fire   Flying   634    78    104      78    159    115   100
## 10      7 Squirtle  Water  <NA>     314    44     48      65     50     64    43
## # ... with 790 more rows, and 2 more variables: generation <dbl>,
## #   legendary <lgl>
```
As the function name suggests, it cleaned the names of our Pokemon data, made the names unique. I think clean_names() is useful, because sometime we may have variables with names that are very similar to each other, and have space as a part of the name, which can cause a lot of problems, and by using clean_names() we can avoid these problems, and we can change the letter case all at once. 


### Exercise 2

Using the entire data set, create a bar chart of the outcome variable, `type_1`.


```r
count <- table(cleaned$type_1)
count
```

```
## 
##      Bug     Dark   Dragon Electric    Fairy Fighting     Fire   Flying 
##       69       31       32       44       17       27       52        4 
##    Ghost    Grass   Ground      Ice   Normal   Poison  Psychic     Rock 
##       32       70       32       24       98       28       57       44 
##    Steel    Water 
##       27      112
```

```r
barplot(count, xlab = "types", ylab = "counts", main ="type 1 counts")
```

![](PSTAT131HW05_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

```r
# not all the type names are shown in the xlab because thats too long
```

How many classes of the outcome are there? Are there any Pokémon types with very few Pokémon? If so, which ones?
    
    There are 18 class of outcomes. There are Pokémon types with very few Pokémon. Such as the  Flying class which only has 4,  Fairy with 17. There are a few other classes that are barely having above 20 pokemons. 

For this assignment, we'll handle the rarer classes by simply filtering them out. Filter the entire data set to contain only Pokémon whose `type_1` is Bug, Fire, Grass, Normal, Water, or Psychic.


```r
filtered <- cleaned %>% filter(
  type_1 == "Bug" | type_1 == "Fire" | type_1 == "Grass" 
  | type_1 == "Normal" | type_1 == "Water" | type_1 == "Psychic"
  )
filtered
```

```
## # A tibble: 458 x 13
##    number name      type_1 type_2 total    hp attack defense sp_atk sp_def speed
##     <dbl> <chr>     <chr>  <chr>  <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <dbl>
##  1      1 Bulbasaur Grass  Poison   318    45     49      49     65     65    45
##  2      2 Ivysaur   Grass  Poison   405    60     62      63     80     80    60
##  3      3 Venusaur  Grass  Poison   525    80     82      83    100    100    80
##  4      3 Venusaur~ Grass  Poison   625    80    100     123    122    120    80
##  5      4 Charmand~ Fire   <NA>     309    39     52      43     60     50    65
##  6      5 Charmele~ Fire   <NA>     405    58     64      58     80     65    80
##  7      6 Charizard Fire   Flying   534    78     84      78    109     85   100
##  8      6 Charizar~ Fire   Dragon   634    78    130     111    130     85   100
##  9      6 Charizar~ Fire   Flying   634    78    104      78    159    115   100
## 10      7 Squirtle  Water  <NA>     314    44     48      65     50     64    43
## # ... with 448 more rows, and 2 more variables: generation <dbl>,
## #   legendary <lgl>
```

After filtering, convert `type_1` and `legendary` to factors.


```r
data <- filtered %>% 
  mutate(type_1 = factor(type_1),
         legendary = factor(legendary),
         generation = factor(generation)
         )
data
```

```
## # A tibble: 458 x 13
##    number name      type_1 type_2 total    hp attack defense sp_atk sp_def speed
##     <dbl> <chr>     <fct>  <chr>  <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <dbl>
##  1      1 Bulbasaur Grass  Poison   318    45     49      49     65     65    45
##  2      2 Ivysaur   Grass  Poison   405    60     62      63     80     80    60
##  3      3 Venusaur  Grass  Poison   525    80     82      83    100    100    80
##  4      3 Venusaur~ Grass  Poison   625    80    100     123    122    120    80
##  5      4 Charmand~ Fire   <NA>     309    39     52      43     60     50    65
##  6      5 Charmele~ Fire   <NA>     405    58     64      58     80     65    80
##  7      6 Charizard Fire   Flying   534    78     84      78    109     85   100
##  8      6 Charizar~ Fire   Dragon   634    78    130     111    130     85   100
##  9      6 Charizar~ Fire   Flying   634    78    104      78    159    115   100
## 10      7 Squirtle  Water  <NA>     314    44     48      65     50     64    43
## # ... with 448 more rows, and 2 more variables: generation <fct>,
## #   legendary <fct>
```

### Exercise 3

Perform an initial split of the data. Stratify by the outcome variable. You can choose a proportion to use. Verify that your training and test sets have the desired number of observations.


```r
set.seed(1010)
pokemon_split <- data %>% 
  initial_split(strata = type_1, prop = 0.7)
pokemon_train <- training(pokemon_split)
pokemon_test <- testing(pokemon_split)
dim(pokemon_train)
```

```
## [1] 318  13
```
    
    There are 318 observations in our training set, which is roughly 70 percent of our filtered overall dataset.

Next, use *v*-fold cross-validation on the training set. Use 5 folds. Stratify the folds by `type_1` as well. *Hint: Look for a `strata` argument.* Why might stratifying the folds be useful?


```r
pokemon_folds <- vfold_cv(pokemon_train, v = 5, strata = 'type_1')
pokemon_folds
```

```
## #  5-fold cross-validation using stratification 
## # A tibble: 5 x 2
##   splits           id   
##   <list>           <chr>
## 1 <split [252/66]> Fold1
## 2 <split [253/65]> Fold2
## 3 <split [253/65]> Fold3
## 4 <split [256/62]> Fold4
## 5 <split [258/60]> Fold5
```
  
  Stratifying the folds can be useful useful because we want to make sure the cross validation reaches the result as realistic as possible. 

### Exercise 4

Set up a recipe to predict `type_1` with `legendary`, `generation`, `sp_atk`, `attack`, `speed`, `defense`, `hp`, and `sp_def`.

- Dummy-code `legendary` and `generation`;

- Center and scale all predictors.


```r
pokemon_recipe <- recipe(type_1 ~ legendary + generation + sp_atk +
                           attack + speed + defense + hp + sp_def,
                         data = pokemon_train) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors()) 
```

### Exercise 5

We'll be fitting and tuning an elastic net, tuning `penalty` and `mixture` (use `multinom_reg` with the `glmnet` engine).

Set up this model and workflow. Create a regular grid for `penalty` and `mixture` with 10 levels each; `mixture` should range from 0 to 1. For this assignment, we'll let `penalty` range from -5 to 5 (it's log-scaled).

How many total models will you be fitting when you fit these models to your folded data?

We will be fitting 500 models to our folded data.


```r
elastic_spec <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

elastic_workflow <- workflow() %>% 
  add_recipe(pokemon_recipe) %>% 
  add_model(elastic_spec)

penalty_grid <- grid_regular(penalty(range = c(-5, 5)), 
                             mixture(range = c(0, 1)),
                             levels = 10)
penalty_grid
```

```
## # A tibble: 100 x 2
##          penalty mixture
##            <dbl>   <dbl>
##  1      0.00001        0
##  2      0.000129       0
##  3      0.00167        0
##  4      0.0215         0
##  5      0.278          0
##  6      3.59           0
##  7     46.4            0
##  8    599.             0
##  9   7743.             0
## 10 100000              0
## # ... with 90 more rows
```

### Exercise 6

Fit the models to your folded data using `tune_grid()`.

Use `autoplot()` on the results. What do you notice? Do larger or smaller values of `penalty` and `mixture` produce better accuracy and ROC AUC?


```r
tune_res <- tune_grid(
  elastic_workflow,
  resamples = pokemon_folds, 
  grid = penalty_grid
)
tune_res
```

```
## # Tuning results
## # 5-fold cross-validation using stratification 
## # A tibble: 5 x 4
##   splits           id    .metrics           .notes          
##   <list>           <chr> <list>             <list>          
## 1 <split [252/66]> Fold1 <tibble [200 x 6]> <tibble [0 x 1]>
## 2 <split [253/65]> Fold2 <tibble [200 x 6]> <tibble [0 x 1]>
## 3 <split [253/65]> Fold3 <tibble [200 x 6]> <tibble [0 x 1]>
## 4 <split [256/62]> Fold4 <tibble [200 x 6]> <tibble [0 x 1]>
## 5 <split [258/60]> Fold5 <tibble [200 x 6]> <tibble [0 x 1]>
```

```r
autoplot(tune_res)
```

![](PSTAT131HW05_files/figure-latex/unnamed-chunk-11-1.pdf)<!-- --> 
lower penalty and higher mixture produce better accuracy and ROC AUC


### Exercise 7

Use `select_best()` to choose the model that has the optimal `roc_auc`. Then use `finalize_workflow()`, `fit()`, and `augment()` to fit the model to the training set and evaluate its performance on the testing set.


```r
collect_metrics(tune_res)
```

```
## # A tibble: 200 x 8
##     penalty mixture .metric  .estimator  mean     n std_err .config             
##       <dbl>   <dbl> <chr>    <chr>      <dbl> <int>   <dbl> <chr>               
##  1 0.00001        0 accuracy multiclass 0.370     5  0.0209 Preprocessor1_Model~
##  2 0.00001        0 roc_auc  hand_till  0.694     5  0.0141 Preprocessor1_Model~
##  3 0.000129       0 accuracy multiclass 0.370     5  0.0209 Preprocessor1_Model~
##  4 0.000129       0 roc_auc  hand_till  0.694     5  0.0141 Preprocessor1_Model~
##  5 0.00167        0 accuracy multiclass 0.370     5  0.0209 Preprocessor1_Model~
##  6 0.00167        0 roc_auc  hand_till  0.694     5  0.0141 Preprocessor1_Model~
##  7 0.0215         0 accuracy multiclass 0.363     5  0.0266 Preprocessor1_Model~
##  8 0.0215         0 roc_auc  hand_till  0.690     5  0.0147 Preprocessor1_Model~
##  9 0.278          0 accuracy multiclass 0.336     5  0.0173 Preprocessor1_Model~
## 10 0.278          0 roc_auc  hand_till  0.652     5  0.0126 Preprocessor1_Model~
## # ... with 190 more rows
```

```r
best_penalty <- select_best(tune_res, metric = "roc_auc")
best_penalty
```

```
## # A tibble: 1 x 3
##   penalty mixture .config               
##     <dbl>   <dbl> <chr>                 
## 1 0.00001   0.889 Preprocessor1_Model081
```

```r
elastic_final <- finalize_workflow(elastic_workflow, best_penalty)

elastic_final_fit <- fit(elastic_final, data = pokemon_train)

augment(elastic_final_fit, new_data = pokemon_test) 
```

```
## # A tibble: 140 x 20
##    number name      type_1 type_2 total    hp attack defense sp_atk sp_def speed
##     <dbl> <chr>     <fct>  <chr>  <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <dbl>
##  1      1 Bulbasaur Grass  Poison   318    45     49      49     65     65    45
##  2      3 Venusaur  Grass  Poison   525    80     82      83    100    100    80
##  3      5 Charmele~ Fire   <NA>     405    58     64      58     80     65    80
##  4     10 Caterpie  Bug    <NA>     195    45     30      35     20     20    45
##  5     12 Butterfr~ Bug    Flying   395    60     45      50     90     80    70
##  6     15 Beedrill~ Bug    Poison   495    65    150      40     15     80   145
##  7     18 PidgeotM~ Normal Flying   579    83     80      80    135     80   121
##  8     39 Jigglypu~ Normal Fairy    270   115     45      20     45     25    20
##  9     44 Gloom     Grass  Poison   395    60     65      70     85     75    40
## 10     45 Vileplume Grass  Poison   490    75     80      85    110     90    50
## # ... with 130 more rows, and 9 more variables: generation <fct>,
## #   legendary <fct>, .pred_class <fct>, .pred_Bug <dbl>, .pred_Fire <dbl>,
## #   .pred_Grass <dbl>, .pred_Normal <dbl>, .pred_Psychic <dbl>,
## #   .pred_Water <dbl>
```

```r
augment(elastic_final_fit, new_data = pokemon_test) %>%
  accuracy(truth = type_1, estimate = .pred_class
          )
```

```
## # A tibble: 1 x 3
##   .metric  .estimator .estimate
##   <chr>    <chr>          <dbl>
## 1 accuracy multiclass     0.393
```

  
### Exercise 8

Calculate the overall ROC AUC on the testing set.

Then create plots of the different ROC curves, one per level of the outcome. Also make a heat map of the confusion matrix.

What do you notice? How did your model do? Which Pokemon types is the model best at predicting, and which is it worst at? Do you have any ideas why this might be?



```r
augment(elastic_final_fit, new_data = pokemon_test) %>%
  roc_auc(truth = type_1, estimate = c(.pred_Bug, .pred_Fire, 
                                       .pred_Grass, .pred_Normal, 
                                       .pred_Psychic, .pred_Water
                                       ))
```

```
## # A tibble: 1 x 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 roc_auc hand_till      0.702
```
collect_metrics(final_fit)
test_acc <- augment(final_fit, new_data = titanic_train) %>%
  roc_auc(truth = survived, estimate = .pred_class)


```r
augment(elastic_final_fit, new_data = pokemon_test) %>%
  roc_curve(truth = type_1, estimate = c(.pred_Bug, .pred_Fire, 
                                       .pred_Grass, .pred_Normal, 
                                       .pred_Psychic, .pred_Water
                                       ))%>%
              autoplot()
```

![](PSTAT131HW05_files/figure-latex/unnamed-chunk-16-1.pdf)<!-- --> 

```r
augment(elastic_final_fit, new_data = pokemon_test) %>%
  conf_mat(truth = type_1, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")
```

![](PSTAT131HW05_files/figure-latex/unnamed-chunk-17-1.pdf)<!-- --> 

It is hard to predict. The accuracy is lower than roc_auc score.It did better than I expected. Since the accuracy is low on the testing set for the selected best model. Pokemon types that is the model best at predicting is normal, Its worst at predicting water/ grass. They do not have big enough number of them to begin with, so the estimations are way off.
