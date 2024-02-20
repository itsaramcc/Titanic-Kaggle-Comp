# Titanic Challenge Kaggle
Itsara McCarthy

- [The Challenge](#the-challenge)
- [Loading Data](#loading-data)
  - [Prepping Training Data](#prepping-training-data)
  - [Prepping Testing Data](#prepping-testing-data)
- [Models](#models)
  - [Random Forest](#random-forest)
  - [Neural Net](#neural-net)
- [Result](#result)

## The Challenge

The sinking of the Titanic is one of the most infamous shipwrecks in
history.

On April 15, 1912, during her maiden voyage, the widely considered
“unsinkable” RMS Titanic sank after colliding with an iceberg.
Unfortunately, there weren’t enough lifeboats for everyone onboard,
resulting in the death of 1502 out of 2224 passengers and crew.

While there was some element of luck involved in surviving, it seems
some groups of people were more likely to survive than others.

In this challenge, we ask you to build a predictive model that answers
the question: “what sorts of people were more likely to survive?” using
passenger data (ie name, age, gender, socio-economic class, etc).
(Cukierski 2012)

## Loading Data

``` r
train <- read_csv("./data/train.csv")
test  <- read_csv("./data/test.csv")
```

| PassengerId | Survived | Pclass | Name                                                | Sex    | Age | SibSp | Parch | Ticket           |    Fare | Cabin | Embarked |
|------------:|---------:|-------:|:----------------------------------------------------|:-------|----:|------:|------:|:-----------------|--------:|:------|:---------|
|           1 |        0 |      3 | Braund, Mr. Owen Harris                             | male   |  22 |     1 |     0 | A/5 21171        |  7.2500 | NA    | S        |
|           2 |        1 |      1 | Cumings, Mrs. John Bradley (Florence Briggs Thayer) | female |  38 |     1 |     0 | PC 17599         | 71.2833 | C85   | C        |
|           3 |        1 |      3 | Heikkinen, Miss. Laina                              | female |  26 |     0 |     0 | STON/O2. 3101282 |  7.9250 | NA    | S        |
|           4 |        1 |      1 | Futrelle, Mrs. Jacques Heath (Lily May Peel)        | female |  35 |     1 |     0 | 113803           | 53.1000 | C123  | S        |
|           5 |        0 |      3 | Allen, Mr. William Henry                            | male   |  35 |     0 |     0 | 373450           |  8.0500 | NA    | S        |
|           6 |        0 |      3 | Moran, Mr. James                                    | male   |  NA |     0 |     0 | 330877           |  8.4583 | NA    | Q        |

### Prepping Training Data

``` r
titanic.training <- train %>% select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Cabin, Embarked) %>% mutate(
  FirstClass = as.integer(Pclass == 1),
  SecondClass = as.integer(Pclass == 2),
  ThirdClass = as.integer(Pclass == 3),
  
  Male = as.integer(Sex == "male"),
  Female = as.integer(Sex == "female"),
  
  Cabins = case_when(
    is.na(Cabin) ~ 0,
    .default = unlist(str_split(Cabin, " ") %>% map(~ length(.x)))
  ),
  
  Cherbourg = as.integer(Embarked == "C"),
  Queenstown = as.integer(Embarked == "Q"),
  Southampton = as.integer(Embarked == "S"),
  
  Age = recode(Age, .missing = median(train$Age, na.rm=TRUE)),
  Fare = recode(Fare, .missing = median(train$Fare, na.rm=TRUE)),
  
  Survived = as.factor(Survived),
  
.keep="unused")
```

| PassengerId | Survived | Age | SibSp | Parch |    Fare | FirstClass | SecondClass | ThirdClass | Male | Female | Cabins | Cherbourg | Queenstown | Southampton |
|------------:|:---------|----:|------:|------:|--------:|-----------:|------------:|-----------:|-----:|-------:|-------:|----------:|-----------:|------------:|
|           1 | 0        |  22 |     1 |     0 |  7.2500 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          0 |           1 |
|           2 | 1        |  38 |     1 |     0 | 71.2833 |          1 |           0 |          0 |    0 |      1 |      1 |         1 |          0 |           0 |
|           3 | 1        |  26 |     0 |     0 |  7.9250 |          0 |           0 |          1 |    0 |      1 |      0 |         0 |          0 |           1 |
|           4 | 1        |  35 |     1 |     0 | 53.1000 |          1 |           0 |          0 |    0 |      1 |      1 |         0 |          0 |           1 |
|           5 | 0        |  35 |     0 |     0 |  8.0500 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          0 |           1 |
|           6 | 0        |  28 |     0 |     0 |  8.4583 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          1 |           0 |

### Prepping Testing Data

``` r
titanic.testing <- test %>% select(PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Cabin, Embarked) %>% mutate(
  FirstClass = as.integer(Pclass == 1),
  SecondClass = as.integer(Pclass == 2),
  ThirdClass = as.integer(Pclass == 3),
  
  Male = as.integer(Sex == "male"),
  Female = as.integer(Sex == "female"),
  
  Cabins = case_when(
    is.na(Cabin) ~ 0,
    .default = unlist(str_split(Cabin, " ") %>% map(~ length(.x)))
  ),
  
  Cherbourg = as.integer(Embarked == "C"),
  Queenstown = as.integer(Embarked == "Q"),
  Southampton = as.integer(Embarked == "S"),
  
  Age = recode(Age, .missing = median(train$Age, na.rm=TRUE)),
  Fare = recode(Fare, .missing = median(train$Fare, na.rm=TRUE)),
  
  Survived = NA,
  
.keep="unused") %>% relocate(Survived, .after = PassengerId)
```

| PassengerId | Survived |  Age | SibSp | Parch |    Fare | FirstClass | SecondClass | ThirdClass | Male | Female | Cabins | Cherbourg | Queenstown | Southampton |
|------------:|:---------|-----:|------:|------:|--------:|-----------:|------------:|-----------:|-----:|-------:|-------:|----------:|-----------:|------------:|
|         892 | NA       | 34.5 |     0 |     0 |  7.8292 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          1 |           0 |
|         893 | NA       | 47.0 |     1 |     0 |  7.0000 |          0 |           0 |          1 |    0 |      1 |      0 |         0 |          0 |           1 |
|         894 | NA       | 62.0 |     0 |     0 |  9.6875 |          0 |           1 |          0 |    1 |      0 |      0 |         0 |          1 |           0 |
|         895 | NA       | 27.0 |     0 |     0 |  8.6625 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          0 |           1 |
|         896 | NA       | 22.0 |     1 |     1 | 12.2875 |          0 |           0 |          1 |    0 |      1 |      0 |         0 |          0 |           1 |
|         897 | NA       | 14.0 |     0 |     0 |  9.2250 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          0 |           1 |

## Models

### Random Forest

``` r
rf <- randomForest(Survived~., data=titanic.training[,-1], na.action=na.roughfix)
rf.pred_training = rf %>% predict(titanic.training)
```

|     |   0 |   1 |
|:----|----:|----:|
| 0   | 534 |  15 |
| 1   |  83 | 257 |

### Neural Net

``` r
nn <- nnet(Survived~., data=titanic.training[,-1], size=13, na.action=na.roughfix)
nn.pred_training = nn %>% predict(titanic.training, type="class")
```

|     |   0 |   1 |
|:----|----:|----:|
| 0   | 510 |  39 |
| 1   | 101 | 239 |

## Result

``` r
titanic.testing$Survived = nn %>% predict(titanic.testing, type="class")
```

| PassengerId | Survived |  Age | SibSp | Parch |    Fare | FirstClass | SecondClass | ThirdClass | Male | Female | Cabins | Cherbourg | Queenstown | Southampton |
|------------:|:---------|-----:|------:|------:|--------:|-----------:|------------:|-----------:|-----:|-------:|-------:|----------:|-----------:|------------:|
|         892 | 0        | 34.5 |     0 |     0 |  7.8292 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          1 |           0 |
|         893 | 0        | 47.0 |     1 |     0 |  7.0000 |          0 |           0 |          1 |    0 |      1 |      0 |         0 |          0 |           1 |
|         894 | 0        | 62.0 |     0 |     0 |  9.6875 |          0 |           1 |          0 |    1 |      0 |      0 |         0 |          1 |           0 |
|         895 | 0        | 27.0 |     0 |     0 |  8.6625 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          0 |           1 |
|         896 | 0        | 22.0 |     1 |     1 | 12.2875 |          0 |           0 |          1 |    0 |      1 |      0 |         0 |          0 |           1 |
|         897 | 0        | 14.0 |     0 |     0 |  9.2250 |          0 |           0 |          1 |    1 |      0 |      0 |         0 |          0 |           1 |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-titanic" class="csl-entry">

Cukierski, Will. 2012. “Titanic - Machine Learning from Disaster.”
Kaggle. <https://kaggle.com/competitions/titanic>.

</div>

</div>
