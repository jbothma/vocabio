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
-export([all/0]).

%% Common Test cases
-export([just_fail/1]).

all() ->
    [just_fail].

just_fail(Config) ->
    %%exit("because").
    ok.
