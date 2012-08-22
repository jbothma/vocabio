-module(word, [Id, JuserId, Word, ModDatetime]).
-belongs_to(juser).
-has({word_instances, many, [{order_by, datetime}, descending]}).

-export([before_delete/0]).

before_delete() ->
    delete_instances(word_instances()).

delete_instances([]) ->
    ok;
delete_instances([Instance|Rest]) ->
    boss_db:delete(Instance:id()),
    delete_instances(Rest).
