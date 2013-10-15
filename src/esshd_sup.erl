%%
%% Root application supervisor.

-module(esshd_sup).
-behaviour(supervisor).

%%

-export([start_link/0, init/1]).

%%

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {Strategy, Specs}} | ignore when
    Strategy :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()},
    Specs :: list(supervisor:child_spec()).

init([]) ->
    Options = supstance:options(global),
    Config = deepprops:get(config, Options, "./priv/config"),
    _Pid = ssh:daemon({127,0,0,1}, 11122, [
        %% server configuration directory
        %% including the host keys
        {system_dir, Config},
        %% note: public user key (authorized_keys) must exist
        %% in the following *user* directory for public-key auth
        {user_dir, Config},
        %%
        %% for subsystem test
        {subsystems, [esshd_subsystem:subsystem_spec([])]},
        %%
        %% the following user/password pair list of
        %% user_password needed for
        %% plain password-based authentication
        {user_passwords, deepprops:get(passwords, Options, [])},
        %%
        %% nodelay option required for faster immediate response!
        %%
        {nodelay, true}
    ]),
    {ok, {{one_for_one, 6, 30}, []}}.
