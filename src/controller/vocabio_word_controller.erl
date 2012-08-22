-module(vocabio_word_controller, [Req, SessionID]).

-export([list/2]).

list('GET', []) ->
    case boss_session:get_session_data(SessionID, user) of
        undefined ->
            ok;
        User ->
            {ok, [{user, User}]}
    end;

list('POST', []) ->
    User = boss_session:get_session_data(SessionID, user),
    ok = case Req:post_param("new_word") of
             undefined -> ok;
             "" -> ok;
             WordParam -> new_word(User, WordParam)
         end,
    ok = case Req:post_param("delete") of
             undefined -> ok;
             _ -> delete(User, Req:deep_post_param(["ids"]))
         end,
    {redirect, "/word/list"}.

new_word(User, WordAsUTF8ByteList) ->
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
    ok.

delete(_, []) -> ok;
delete(User, [{TypeStr, XIdInts}|Rest]) ->
    ok = delete(User, TypeStr, XIdInts),
    delete(User, Rest).

delete(_, _, []) -> ok;
delete(User, TypeStr, [{XIdInt, _}|Rest])
  when TypeStr =:= "word" orelse
       TypeStr =:= "word_instance" ->
    [$x|IdIntStr] = XIdInt,
    Id = TypeStr ++ "-" ++ IdIntStr,
    Thing = boss_db:find(Id),
    case belongs_to(User, Thing) of
        true ->
            ok = boss_db:delete(Thing:id());
        false ->
            ok
    end,
    delete(User, TypeStr, Rest).


belongs_to(User, Thing) ->
    belongs_to(User, boss_db:type(Thing:id()), Thing).

belongs_to(User, word, Word) ->
    Owner = Word:juser(),
    Owner:id() =:= User:id();
belongs_to(User, word_instance, WordInstance) ->
    belongs_to(User, WordInstance:word()).
