-module(vocabio_word_controller, [Req, SessionID]).
-compile(export_all).

list('GET', []) ->
    case boss_session:get_session_data(SessionID, user) of
        undefined ->
            ok;
        User ->
            Words = boss_db:find(word, [{juser_id, equals, User:id()}]),
            {ok, [{words, Words},
                  {user, User}]}
    end;

list('POST', []) ->
    User = boss_session:get_session_data(SessionID, user),
    %% looks like post_param/2 gives a list utf-8 bytes as integers.
    Word = list_to_binary(Req:post_param("new_word")),
    %% TODO: This searches on the utf-8 binary representation of the word
    %% so unless the input is reduced to a canonical form, this won't match
    %% different unicode ways of representing the same character (unless
    %% utf-8 does this already, but I doubt that)
    WordRec = case boss_db:find(word, [{juser_id, equals, User:id()},
                                       {word, equals, Word}]) of
                  [] -> word:new(id, User:id(), Word);
                  [Existing] -> Existing
              end,
    %% Store POST URL without scheme for now since I'm not sure how to get it.
    PostedTo = "//" ++ Req:header(host) ++ "/user/list",
    SourceTerm = {post, PostedTo, Req:header(referer)},
    Source = term_to_binary(SourceTerm),
    {ok, SavedWord} = WordRec:save(),
    InstanceRec = word_instance:new(id, SavedWord:id(), erlang:universaltime(),
                                    Source),
    {ok, _SavedInstance} = InstanceRec:save(),
    {redirect, "/word/list"}.
