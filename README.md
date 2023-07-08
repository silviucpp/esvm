esvm
================

[![Build Status](https://travis-ci.com/silviucpp/esvm.svg?branch=master)](https://travis-ci.com/github/silviucpp/esvm)
[![GitHub](https://img.shields.io/github/license/silviucpp/esvm)](https://github.com/silviucpp/esvm/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/esvm)](https://hex.pm/packages/esvm)

A simple, easy-to-use, and efficient erlang library for Support Vector Machine (SVM) classification and regression based on [libsvm](https://github.com/cjlin1/libsvm). It solves C-SVM classification, nu-SVM
classification, one-class-SVM, epsilon-SVM regression, and nu-SVM regression. 

The Support Vector Machine (SVM) is a widely recognized technique used for classifying large feature spaces reliably. It is a statistical model that employs machine learning approaches to capture complex relationships between variables.
The primary principle underlying the Support Vector Machine revolves around distinguishing information between different classes by identifying an optimal hyperplane. This hyperplane is chosen to have the maximum margin or distance 
to the nearest training data points of any class, ensuring superior generalization capabilities. 

This method offers several advantages. It exhibits remarkable performance in high-dimensional spaces and effectively manages memory usage by utilizing a subset of training points in its decision function. However, it may not be as efficient when the number of features exceeds the number of samples. 


Quick start
-----------

Compile:

```sh
rebar3 compile
```

### Create a model

```erlang

% features should be a list with tuples where first element is the item class and the second one the feature vector.

Features = [
    {1, [1 ,3, 4, 5]},
    {0, [0 ,2, 4, 6]},
    {1, [0 ,2, 4, 6]}
],

FeaturesCount = length(Features),

{ok, Model} = esvm:model_create(Features, FeaturesCount, [
    {<<"svm_type">>, ?SVM_TYPE_C_SVC},
    {<<"kernel_type">>, ?KERNEL_TYPE_RBF}
]).
```

### Save a model

```erl
true = esvm:model_save(Model, <<"path/file.model">>).
```

### Load an existing model

```erl
{ok, Model} = esvm:model_load(<<"path/file.model">>).
```

### Prediction

```erl
{ok, PredictedClass} = esvm:model_predict(Model, Feature).
```

Tests
------------

Inside `classification_test.erl` from `test` folder you can find an example on how you can create a model that classify if 
a sms message is spam or not using SVM.

The data source used to train the model can be downloaded from [here](https://archive.ics.uci.edu/dataset/228/sms+spam+collection).  

In order to run the tests execute `rebar3 eunit` from project root.
