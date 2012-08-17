%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc boss app config utils that should prb be merged with boss_env
%%% if it helps anyone else.
%%%
%%% @end
%%% Created : 17 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_conf).

-export([merge_conf_file/0, merge_conf_file/1, add_paths/1]).

merge_conf_file() ->
    merge_conf_file("boss.config").

merge_conf_file(Filename) ->
    {ok, Terms} = file:consult(Filename),
    [AppConfs] = Terms,
    ok = merge_app(AppConfs).

merge_app([]) -> ok;
merge_app([{App, Confs}|Rest]) ->
    ok = merge_confs(App, Confs),
    ok = merge_app(Rest).

merge_confs(_, []) -> ok;
merge_confs(App, [{Key, Value}|Rest]) ->
    ok = application:set_env(App, Key, Value),
    ok = merge_confs(App, Rest).

add_paths(VocabioRoot) ->
    ok = code:add_pathsa(filelib:wildcard(VocabioRoot++"/deps/*/ebin")),
    true = code:add_patha(VocabioRoot++"../vocabio/ebin"),
    ok.
