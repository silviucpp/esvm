esvm
================

[![Build Status](https://travis-ci.com/silviucpp/esvm.svg?branch=master)](https://travis-ci.com/github/silviucpp/esvm)
[![GitHub](https://img.shields.io/github/license/silviucpp/esvm)](https://github.com/silviucpp/esvm/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/esvm)](https://hex.pm/packages/esvm)

A simple, easy-to-use, and efficient erlang library for SVM classification and regression based on [libsvm](https://github.com/cjlin1/libsvm). It solves C-SVM classification, nu-SVM
classification, one-class-SVM, epsilon-SVM regression, and nu-SVM regression. 

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
an sms message is spam or not using SVM.

In order to run the tests run `rebar3 eunit` from project root.

