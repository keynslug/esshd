%%
%% Subsystem

-module(esshd_subsystem).
-behaviour(ssh_daemon_channel).

-export([subsystem_spec/1]).
-export([
    init/1,
    handle_ssh_msg/2,
    handle_msg/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    cm,
    channel,
    pending    % binary()
}).

-define(INTRO, <<"> ">>).

%% subsystem name conforms the private method and algorithm
%% convention on RFC4251 Section 6.

subsystem_spec(Options) ->
    {"esshd@encube.ul", {?MODULE, Options}}.

init(_Options) ->
    {ok, #state{pending = <<>>}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Type 0: RPC messages
%% other types: reserved
handle_ssh_msg({ssh_cm, _ConnectionManager, {data, _ChannelId, 0, Data}}, State) ->
    lager:info("Connection received portion of data: ~p", [Data]),
    FinalState = handle_data(Data, State),
    {ok, FinalState};

handle_ssh_msg({ssh_cm, _, {eof, ChannelId}}, State) ->
    lager:info("Connection received eof"),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {signal, _ChannelId, Code}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    lager:info("Connection received signal: ~p", [Code]),
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    lager:error("Connection closed by peer, error: ~p", [Error]),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    lager:info("Connection closed by peer, status: ~p", [Status]),
    {stop, ChannelId, State}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    lager:info("Connection channel up"),
    _ = ssh_connection:send(ConnectionManager, ChannelId, ?INTRO),
    {ok, State#state{channel = ChannelId, cm = ConnectionManager}}.

terminate(_Reason, _State) ->
    ok.

%% handling received data
%% see ssh_sftpd.erl

-define(CRLF, "\r\n").
-define(LF, "\n").

handle_data(Piece, State = #state{cm = ConnectionManager, channel = ChannelId, pending = Pending}) ->
    Data = <<Pending/binary, Piece/binary>>,
    case binary:split(Data, [<<?CRLF>>, <<?LF>>]) of
        [Line, Rest] ->
            Reply = execute_line(trim_ws(Line)),
            _ = ssh_connection:send(ConnectionManager, ChannelId, Reply),
            _ = ssh_connection:send(ConnectionManager, ChannelId, ?INTRO),
            handle_data(<<>>, State#state{pending = Rest});
        _E ->
            State#state{pending = Data}
    end.

execute_line(<<>>) ->
    <<>>;

execute_line(Line) ->
    normalize_output(os:cmd(normalize_input(Line))).

normalize_input(C) ->
    binary_to_list(C).

normalize_output(C) ->
    iolist_to_binary(normalize_lf(C)).

%%

trim_ws(<<$\s, Rest/binary>>) ->
    trim_ws(Rest);

trim_ws(<<$\t, Rest/binary>>) ->
    trim_ws(Rest);

trim_ws(Rest) ->
    Rest.

%%

normalize_lf([]) ->
    [];

normalize_lf(?CRLF ++ Rest) ->
    ?CRLF ++ normalize_lf(Rest);

normalize_lf("\n" ++ Rest) ->
    ?CRLF ++ normalize_lf(Rest);

normalize_lf([C | Rest]) ->
    [C | normalize_lf(Rest)].
