%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, JD Bothma
%%% @doc This does the mocking for ct suites to call remotely.
%%%
%%% @end
%%% Created : 16 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_ct_meck).

-export([mock_openid/0, unmock_openid/0]).

mock_openid() ->
    ok = meck:new(openid_srv, [no_link]),
    ok = meck:expect(
           openid_srv, handle_call,
           fun({prepare, _, _, _}, _, State) ->
                   {reply, {ok, stuff}, State};
              ({verify, _, _, _}, _, State) ->
                   {reply, {ok, "http://mocked.open/id"}, State}
           end),
    ok = meck:new(openid, [no_link]),
    ok = meck:expect(
           openid, authentication_url,
           fun(_,_,_) ->
                   "http://fake.openid.out/url"
           end),
    ok.

unmock_openid() ->
    meck:unload(openid_srv),
    meck:unload(openid).
