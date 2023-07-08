-module(esvm_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 0, period => 1}, []}}.
