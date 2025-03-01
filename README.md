# esvm

[![Build Status](https://app.travis-ci.com/silviucpp/esvm.svg?branch=main)](https://travis-ci.com/github/silviucpp/esvm)
[![GitHub](https://img.shields.io/github/license/silviucpp/esvm)](https://github.com/silviucpp/esvm/blob/main/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/esvm)](https://hex.pm/packages/esvm)

`esvm` is a simple, easy-to-use, and efficient Erlang library for **Support Vector Machine (SVM)** classification and regression, based on [libsvm](https://github.com/cjlin1/libsvm). It supports:

- C-SVM classification
- Nu-SVM classification
- One-class SVM
- Epsilon-SVM regression
- Nu-SVM regression

## Overview

The **Support Vector Machine (SVM)** is a widely recognized technique for classifying large feature spaces reliably. It is a statistical model that leverages machine learning to capture complex relationships between variables.

The core idea of SVM is to find an optimal hyperplane that distinguishes between different classes. This hyperplane is selected to maximize the margin, ensuring superior generalization. SVMs perform exceptionally well in high-dimensional spaces and use memory efficiently by relying on a subset of training data points for decision-making.

However, SVMs may become inefficient when the number of features exceeds the number of samples.

## Quick Start

### Compile the project

```sh
rebar3 compile
```

### Create a model

```erlang
% Features should be a list of tuples where the first element is the class
% and the second is the feature vector.

Features = [
    {1, [1, 3, 4, 5]},
    {0, [0, 2, 4, 6]},
    {1, [0, 2, 4, 6]}
],

FeaturesCount = length(Features),

{ok, Model} = esvm:model_create(Features, FeaturesCount, [
    {<<"svm_type">>, ?SVM_TYPE_C_SVC},
    {<<"kernel_type">>, ?KERNEL_TYPE_RBF}
]).
```

### Model parameters

When creating a model, the following parameters can be tuned:

- **`svm_type`**: One of the `SVM_TYPE_*` values from `esvm.hrl`. Default is `SVM_TYPE_C_SVC`.
- **`kernel_type`**: One of the `KERNEL_TYPE_*` values from `esvm.hrl`. Default is `KERNEL_TYPE_RBF`.
- **`degree`**: Degree in the kernel function (default `3`).
- **`gamma`**: Gamma in the kernel function (default is `1 / max feature length`).
- **`coef0`**: Coefficient for the kernel function (default `0`).
- **`cache_size`**: Cache memory size in MB (default `100`).
- **`eps`**: Tolerance for the termination criterion (default `0.001`).
- **`C`**: The `C` (cost) parameter for C-SVC, epsilon-SVR, and nu-SVR (default `1`).
- **`nu`**: The `nu` parameter for nu-SVC, one-class SVM, and nu-SVR (default `0.5`).
- **`p`**: Epsilon in the loss function of epsilon-SVR (default `0.1`).
- **`shrinking`**: Whether to use shrinking heuristics, 0 or 1 (default `1`).
- **`probability`**: Whether to train a model for probability estimates, 0 or 1 (default `0`).

### Save a model

```erlang
true = esvm:model_save(Model, <<"path/file.model">>).
```

### Load an existing model

```erlang
{ok, Model} = esvm:model_load(<<"path/file.model">>).
```

### Make a prediction

```erlang
{ok, PredictedClass} = esvm:model_predict(Model, Feature).
```

## Tests

Inside the `classification_test.erl` file in the `test` folder, you will find an example of creating a model to classify SMS messages as spam or not using SVM.

The dataset used to train the model can be downloaded from [here](https://archive.ics.uci.edu/dataset/228/sms+spam+collection).

To run the tests, execute the following command from the project root:

```sh
rebar3 eunit
```
