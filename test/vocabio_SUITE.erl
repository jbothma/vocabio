%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, JD Bothma
%%% @doc Common Test test suite for general Vocabio functionality via HTTP
%%%
%%% @end
%%% Created : 15 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_SUITE).

%% Common Test interface
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Common Test cases
-export([get_root/1]).

%%==============================================================================
%% CT API
%%==============================================================================

all() ->
    [get_root].

init_per_suite(Config) ->
    inets:start(),
    %% Let httpc handle session cookies for us with the default node-wide profile
    httpc:set_options([{cookies, enabled}]),
    StartOut = os:cmd("../../init.sh start"),
    ct:log(StartOut),
    wait_until_root_up(),
    Config.

end_per_suite(_Config) ->
    ibrowse:stop(),
    os:cmd("../../init.sh stop"),
    void.


%%==============================================================================
%% Test cases
%%==============================================================================

%% This sets up the session cookie and whatnot in the httpc profile.
get_root(_Config) ->
    {ok, {{_,200,"OK"},_,_}} = httpc:request("http://localhost:8001/").


%%==============================================================================
%% Utilities
%%==============================================================================

%% Hackedly poll server until it's alive. Trust common test to eventually
%% give up and kill this process on some timeout.
wait_until_root_up() ->
    case httpc:request("http://localhost:8001/") of
        {ok, {{_,200,"OK"},_,_}} ->
            ok;
        {error, {failed_connect,
                 [{to_address,{"localhost",8001}},
                  {inet,[inet],econnrefused}]}} ->
            receive after 500 -> ok end,
            wait_until_root_up()
    end.
