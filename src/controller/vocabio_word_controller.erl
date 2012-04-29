-module(vocabio_word_controller, [Req, SessionID]).
-compile(export_all).

list('GET', []) ->
    case boss_session:get_session_data(SessionID, user) of
        undefined ->
            ok;
        User ->
            io:format("~p~n", [User]),
            Words = boss_db:find(word, [{user_id, equals, User:id()}]),
            {ok, [{words, Words},
                  {user, User}]}
    end;

list('POST', []) ->
    User = boss_session:get_session_data(SessionID, user),
    Word = Req:post_param("new_word"),
    WordRec = word:new(id, User:id(), Word),
    {ok, SavedWord} = WordRec:save(),
    {redirect, "/word/list"}.
