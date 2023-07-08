-module(classification_test).

-include("esvm.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CLASS_HAM, 0).
-define(CLASS_SPAM, 1).

-record(stats, {
    count = 0,
    true_positive = 0,
    false_positive = 0,
    true_negative = 0,
    false_negative = 0
}).

predict_created_model_test() ->
    {ok, Features, FeaturesCount, MaxFeatureSize} = read_file(<<"test/spam_collection.txt">>),
    {ok, Model} = esvm:model_create(Features, FeaturesCount, [
        {<<"svm_type">>, ?SVM_TYPE_C_SVC},
        {<<"kernel_type">>, ?KERNEL_TYPE_RBF},
        {<<"gamma">>, 1/MaxFeatureSize}
    ]),

    ?assertEqual(true, esvm:model_save(Model, <<"model.data">>)),

    {ok, Result} = check_prediction(Features, Model, #stats{}),
    io:format(standard_error, "created model result: ~p ~n", [Result]),
    ok.

predict_loaded_model_test() ->
    {ok, Model} = esvm:model_load(<<"model.data">>),

    {ok, Features, _FeaturesCount, _MaxFeatureSize} = read_file(<<"test/spam_collection.txt">>),
    {ok, Result} = check_prediction(Features, Model, #stats{}),
    io:format(standard_error, "loaded model result: ~p ~n", [Result]),
    ok.

% internals

check_prediction([{Class, Feature}|T], Model, #stats{count = Count} = Stats) ->
    {ok, PredictedClass0} = esvm:model_predict(Model, Feature),
    PredictedClass = trunc(PredictedClass0),

    case PredictedClass of
        ?CLASS_SPAM ->
            case PredictedClass == Class of
                true ->
                    check_prediction(T, Model, Stats#stats{count = Count+1, true_positive = Stats#stats.true_positive+1});
                _ ->
                    check_prediction(T, Model, Stats#stats{count = Count+1, false_positive = Stats#stats.false_positive+1})
            end;
        _ ->
            case PredictedClass == Class of
                true ->
                    check_prediction(T, Model, Stats#stats{count = Count+1, true_negative = Stats#stats.true_negative+1});
                _ ->
                    check_prediction(T, Model, Stats#stats{count = Count+1, false_negative = Stats#stats.false_negative+1})
            end
    end;
check_prediction([], _Model, Stats) ->
    Accuracy = compute_accuracy(Stats),
    Precision = compute_precision(Stats) ,
    Recall = compute_recall(Stats),
    FScore = compute_fscore(Precision, Recall),
    {ok, [{total, Stats#stats.count}, {accuracy, Accuracy}, {precision, Precision}, {recall, Recall}, {fscore, FScore}]}.

read_file(FilePath) ->
    {ok, File} = file:open(FilePath, [read, raw, binary, {read_ahead, 64000}, {encoding, utf8}]),
    R = read_lines(File, 0, maps:new(), [], 0, 0),
    file:close(File),
    R.

read_lines(File, WordIndex, UniqueWordMap, FeaturesAcc, FeaturesCount, MaxFeatureSize) ->
    case file:read_line(File) of
        {ok, Line} ->
            [ClassBin, Text] = binary:split(Line, <<"\t">>),
            ClassInt = class2int(ClassBin),

            Words = binary:split(Text, <<" ">>, [global]),

            {NewWordIndex, NewUniqueWordMap, Feature0, FeatureSize} = lists:foldl(fun(Word, {WordIndexAcc, WordMapAcc, FeatureAcc, FSize}) ->
                case maps:find(Word, WordMapAcc) of
                    error ->
                        {WordIndexAcc +1, maps:put(Word, WordIndexAcc, WordMapAcc), [{WordIndexAcc, 1}| FeatureAcc], FSize+1};
                    {ok, ExistingWordIndex} ->
                        {WordIndexAcc, WordMapAcc, [{ExistingWordIndex, 1}| FeatureAcc], FSize+1}
                end
            end, {WordIndex, UniqueWordMap, [], 0}, Words),

            read_lines(File, NewWordIndex, NewUniqueWordMap, [{ClassInt, lists:reverse(Feature0)}|FeaturesAcc], FeaturesCount+1, erlang:max(FeatureSize, MaxFeatureSize));
        eof ->
            {ok, FeaturesAcc, FeaturesCount, MaxFeatureSize}
    end.

class2int(<<"ham">>) ->
    ?CLASS_HAM;
class2int(<<"spam">>) ->
    ?CLASS_SPAM.

compute_accuracy(#stats{true_positive = Tp, true_negative = Tn, count = Count}) ->
    round((Tp+Tn)/Count, 4).

compute_precision(#stats{true_positive = Tp, false_positive = Fp}) ->
    case Tp+Fp of
        0 ->
            0;
        PSum ->
            round(Tp/PSum, 4)
    end.

compute_recall(#stats{true_positive = Tp, false_negative = Fn}) ->
    case Tp+Fn of
        0 ->
            0;
        Sum ->
            round(Tp/Sum, 4)
    end.

compute_fscore(Precision, Recall) ->
    case Precision+Recall of
        0 ->
            0;
        _ ->
            round((2*Precision*Recall)/(Precision+Recall), 4)
    end.

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.
