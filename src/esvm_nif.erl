-module(esvm_nif).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(load_nif/0).

-export([
    model_create/3,
    model_load/1,
    model_save/2,
    model_predict/2
]).

% nif functions

load_nif() ->
    SoName = get_priv_path(?MODULE),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

model_create(_Features, _EstimatedFeatureLength, _TrainingParams) ->
    ?NOT_LOADED.

model_load(_FilePath) ->
    ?NOT_LOADED.

model_save(_ModelRef, _FilePath) ->
    ?NOT_LOADED.

model_predict(_ModelRef, _Features) ->
    ?NOT_LOADED.

% internals

get_priv_path(File) ->
    get_priv_path(esvm, ?MODULE, File).

get_priv_path(AppName, Module, File) ->
    case code:priv_dir(AppName) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Module)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.
