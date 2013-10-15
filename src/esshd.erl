%%
%% Application bootstrap routines.

-module(esshd).
-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

%%

-spec start() -> ok.

start() ->
    start(?MODULE).

-spec stop() -> ok | {error, term()}.

stop() ->
    application:stop(?MODULE).

%%

-spec start(StartType, term()) -> {ok, pid()} when
    StartType :: normal | {takeover, node()} | {failover, node()}.

start(_StartType, _StartArgs) ->
    esshd_sup:start_link().

-spec stop(term()) -> ok.

stop(_State) ->
    ok.

%%

start(App) ->
    do_start(App, application:start(App, permanent)).

do_start(_, ok) ->
    ok;

do_start(_, {error, {already_started, _App}}) ->
    ok;

do_start(App, {error, {not_started, Dep}}) when App =/= Dep ->
    ok = start(Dep),
    start(App);

do_start(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
