-module(vocabio_word_controller, [Req, SessionID]).
-compile(export_all).

list('GET', []) ->
    case boss_session:get_session_data(SessionID, user) of
        undefined ->
            ok;
        User ->
            {ok, [{user, User}]}
    end;

list('POST', []) ->
    User = boss_session:get_session_data(SessionID, user),
    %% looks like post_param/2 gives a list utf-8 bytes as integers.
    WordAsUTF8ByteList = Req:post_param("new_word"),
    WordAsNFCUTF8Binary = vocabio_unicode:utf8bytelist_to_nfc_utf8_binary(
                      WordAsUTF8ByteList),
    WordRec = case boss_db:find(word, [{juser_id, equals, User:id()},
                                       {word, equals, WordAsNFCUTF8Binary}]) of
                  [] -> word:new(id, User:id(), WordAsNFCUTF8Binary,
                                 erlang:universaltime());
                  [Existing] -> Existing:set(mod_datetime, erlang:universaltime())
              end,
    %% Store POST URL without scheme for now since I'm not sure how to get it.
    PostedTo = "//" ++ Req:header(host) ++ "/word/list",
    SourceTerm = {post, PostedTo, Req:header(referer)},
    Source = term_to_binary(SourceTerm),
    {ok, SavedWord} = WordRec:save(),
    InstanceRec = word_instance:new(id, SavedWord:id(), erlang:universaltime(),
                                    Source),
    {ok, _SavedInstance} = InstanceRec:save(),
    {redirect, "/word/list"}.
