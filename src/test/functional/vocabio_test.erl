%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @doc boss_web_test-based testing of the web frontend
%%%
%%% Tests to make sure the website is functionally correct, i.e. pages load,
%%% things get saved, users can register, etc.
%%%
%%% @end
%%% Created : 10 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_test).

-export([start/0]).

start() ->
    boss_web_test:get_request("/", [],
        [
         fun boss_assert:http_ok/1
         ,fun(Res) -> boss_assert:link_with_text("Sign in", Res) end
         ,fun(Res) -> boss_assert:link_with_text("Sign up", Res) end
        ],
        [
         "Follow link sign in",
         fun(Res) ->
             boss_web_test:follow_link("Sign in", Res,
                 [fun boss_assert:http_ok/1 ], [])
         end,
         "Follow link sign up",
         fun(Res) ->
             boss_web_test:follow_link("Sign up", Res,
                 [fun boss_assert:http_ok/1], [])
         end
        ]).
