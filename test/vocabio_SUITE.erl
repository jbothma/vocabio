%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Common Test test suite for general Vocabio functionality via HTTP
%%%
%%% @end
%%% Created : 15 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_SUITE).

%% Common Test interface
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Common Test cases
-export([
         get_root/1
         ,sign_up/1
        ]).

%%==============================================================================
%% CT API
%%==============================================================================

all() ->
    [
     get_root
     ,sign_up
    ].

init_per_suite(Config) ->
    try
        ok = vocabio_conf:add_paths("../../"),
        ok = vocabio_conf:merge_conf_file("../../boss.config"),
        ok = application:start(crypto),
        ok = application:start(boss),
        ok = application:start(inets),
        RootURL = "http://localhost:8001/",
        %% Let httpc handle session cookies for us with the default node-wide
        %% profile
        ok = httpc:set_options([{cookies, enabled}]),
        ok = wait_until_root_up(RootURL),
        ok = vocabio_ct_meck:mock_openid(),
        [{root_url, RootURL} | Config]
    catch
        Class:Exception ->
            ct:log("~p:~p",[Class,Exception]),
            ct:log("~p", [erlang:get_stacktrace()]),
            application:stop(boss),
            {skip, {Class, Exception}}
    end.

end_per_suite(_Config) ->
    vocabio_ct_meck:unmock_openid(),
    ok = application:stop(boss),
    void.


%%==============================================================================
%% Test cases
%%==============================================================================

%% This sets up the session cookie and whatnot in the httpc profile.
get_root(Config) ->
    RootURL = proplists:get_value(root_url, Config),
    {ok, {{_,200,"OK"},_,_}} = httpc:request(RootURL).

sign_up(Config) ->
    RootURL = proplists:get_value(root_url, Config),
    {ok, {{_,302,"Moved Temporarily"},SignupHeads,_}} =
        httpc:request(
          get, {RootURL ++ "/user/signup", []},
          [{autoredirect, false}], []),
    RelOIDStartURL = proplists:get_value("location", SignupHeads),
    {ok, {{_,302,"Moved Temporarily"},OIDStartHeads,_}} =
        httpc:request(
          get, {RootURL ++ RelOIDStartURL, []},
          [{autoredirect, false}], []),
    OIDOutURL = proplists:get_value("location", OIDStartHeads),
    {ok, {_,_,_,_,_,GETVarPart}} = http_uri:parse(OIDOutURL),
    GETVarString = http_uri:decode(GETVarPart),
    GETKeyEqVal = re:split(GETVarString, "[?&]"),
    true = lists:member(
             <<"openid.return_to=http://localhost:8001/user/openid/return">>,
             GETKeyEqVal),
    {ok, {{_,302,"Moved Temporarily"},OIDReturnHeads,_}} =
        httpc:request(
          get, {RootURL ++ "/user/openid/return", []},
          [{autoredirect, false}], []),
    "/user/create" = proplists:get_value("location", OIDReturnHeads).


%%==============================================================================
%% Utilities
%%==============================================================================

%% Hackedly poll server until it's alive. Trust common test to eventually
%% give up and kill this process on some timeout.
wait_until_root_up(RootURL) ->
    case httpc:request(RootURL) of
        {ok, {{_,200,"OK"},_,_}} ->
            ok;
        {error, {failed_connect,
                 [{to_address,{"localhost",8001}},
                  {inet,[inet],econnrefused}]}} ->
            receive after 500 -> ok end,
            wait_until_root_up(RootURL)
    end.
