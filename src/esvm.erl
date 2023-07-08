-module(esvm).

-include("esvm.hrl").

-export([
    model_create/3,
    model_load/1,
    model_save/2,
    model_predict/2
]).

-spec model_create([{label(), features()}], non_neg_integer(), training_params()) ->
    {ok, model()} | {error, any()}.

model_create(Features, EstimatedFeatureLength, TrainingParams) ->
    esvm_nif:model_create(Features, EstimatedFeatureLength, TrainingParams).

-spec model_load(binary()) ->
    {ok, model()} | {error, any()}.

model_load(FilePath) ->
    esvm_nif:model_load(FilePath).

-spec model_save(model(), binary()) ->
    boolean() | {error, any()}.

model_save(ModelRef, FilePath) ->
    esvm_nif:model_save(ModelRef, FilePath).

-spec model_predict(model(), features()) ->
    {ok, label()} | {error, any()}.

model_predict(ModelRef, Features) ->
    esvm_nif:model_predict(ModelRef, Features).
