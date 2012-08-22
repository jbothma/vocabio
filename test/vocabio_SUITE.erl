%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Common Test test suite for general Vocabio functionality via HTTP
%%%
%%% @end
%%% Created : 15 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_SUITE).

-define(ROOT_URL, proplists:get_value(root_url, Config)).
-define(POST_CONT_TYPE, "application/x-www-form-urlencoded").

%% Common Test interface
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Common Test cases
-export([
         get_root/1
         ,get_user_signup/1
         ,get_user_openid_choose/1
         ,get_user_openid_start/1
         ,get_user_openid_return/1
         ,get_user_create/1
         ,post_user_create/1
         ,get_user_do__signin/1
         ,get_word_list_before_submit/1
         ,post_word_list/1
         ,get_word_list_after_submit/1
        ]).

%%==============================================================================
%% CT API
%%==============================================================================

all() ->
    [
     get_root
     ,get_user_signup
     ,get_user_openid_choose
     ,get_user_openid_start
     %% ... then skip the OpenID provider part since openid is mocked ...
     ,get_user_openid_return
     ,get_user_create
     ,post_user_create
     ,get_user_do__signin
     ,get_word_list_before_submit
     ,post_word_list
     ,get_word_list_after_submit
    ].

init_per_suite(Config) ->
    try
        ok = vocabio_conf:merge_conf_file("../../boss.config"),
        ok = application:start(crypto),
        ok = application:start(boss),
        ok = application:start(inets),
        {ok, RootURL} = application:get_env(vocabio, vocabio_url),
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
    {ok, {{_,200,"OK"},_,_}} = httpc:request(?ROOT_URL).

get_user_signup(Config) ->
    {ok, {{_,302,"Moved Temporarily"},SignupHeads,_}} =
        httpc:request(
          get, {?ROOT_URL ++ "/user/signup", []},
          [{autoredirect, false}], []),
    "/user/openid/choose" = proplists:get_value("location", SignupHeads).

get_user_openid_choose(Config) ->
    {ok, {{_,200,"OK"},_,_}} = httpc:request(?ROOT_URL++"/user/openid/choose").

get_user_openid_start(Config) ->
    {ok, {{_,302,"Moved Temporarily"},_OIDStartHeads,_}} =
        httpc:request(
          get, {?ROOT_URL ++ "/user/openid/start?endpoint=untested", []},
          [{autoredirect, false}], []).

get_user_openid_return(Config) ->
    {ok, {{_,302,"Moved Temporarily"},OIDReturnHeads,_}} =
        httpc:request(
          get, {?ROOT_URL ++ "/user/openid/return", []},
          [{autoredirect, false}], []),
    "/user/create" = proplists:get_value("location", OIDReturnHeads).

get_user_create(Config) ->
    {ok, {{_, 200, "OK"}, _,_}} = httpc:request(?ROOT_URL++"/user/create").

post_user_create(Config) ->
    CreatePOSTBody = "display_name=Fred&email=fred@bloggs.com",
    {ok, {{_, 302, "Moved Temporarily"},CreateRespHeads,_}} =
        httpc:request(
          post, {?ROOT_URL ++ "/user/create", [], ?POST_CONT_TYPE, CreatePOSTBody},
          [{autoredirect, false}], []),
    "/user/do_signin" = proplists:get_value("location", CreateRespHeads).

get_user_do__signin(Config) ->
    {ok, {{_, 302, "Moved Temporarily"},DoSigninRespHeads,_}} =
        httpc:request(
          get, {?ROOT_URL ++ "/user/do_signin", []},
          [{autoredirect, false}], []),
    "/" = proplists:get_value("location", DoSigninRespHeads).

get_word_list_before_submit(Config) ->
    {ok, {{_, 200, "OK"}, _,NewListBody}} =
        httpc:request(?ROOT_URL++"/word/list"),
    nomatch = re:run(NewListBody, "\bsomenewword\b", [{capture, none}]).

post_word_list(Config) ->
    ReqHeads = [{"Referer", ?ROOT_URL++"/word/list"}],
    POSTBody = "new_word=somenewword",
    {ok, {{_, 302, "Moved Temporarily"},WordPostRespHeads,_}} =
        httpc:request(
          post, {?ROOT_URL ++ "/word/list", ReqHeads, ?POST_CONT_TYPE, POSTBody},
          [], []),
    "/word/list" = proplists:get_value("location", WordPostRespHeads).

get_word_list_after_submit(Config) ->
    {ok, {{_, 200, "OK"},_,NewListBody}} = httpc:request(?ROOT_URL++"/word/list"),
    match = re:run(NewListBody, "\\bsomenewword\\b", [{capture, none}]).


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
