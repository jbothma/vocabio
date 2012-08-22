-module(word_instance, [Id, WordId, Datetime, Source]).
-belongs_to(word).

-export([validation_tests/0]).

-export_type([source_term/0]).

%%
%%--------------------------------------------------------------------
%% @doc
%% Source is a binary_to_term(source_term()).
%%
%% @end
%%--------------------------------------------------------------------
-type source_term() :: {post, post_url(), referer()}.
-type post_url() :: http:url().
-type referer() :: http:url().


validation_tests() ->
    [{fun validate_source/0, "Invalid word source"}
    ].

validate_source() ->
    validate_source(binary_to_term(Source)).

validate_source({post, POST_URL, Referer}) ->
    case
        try
            {ok, _} = http_uri:parse(unicode:characters_to_list(Referer)),
            {ok, BaseURL} = application:get_env(vocabio, vocabio_url),
            {ok, {_, _, Host, Port, Path, _}} = http_uri:parse(BaseURL),
            ExpectedPOSTURL = lists:flatten(
                                ["//", Host, $:,integer_to_list(Port),
                                 Path, "word/list"]),
            ExpectedPOSTURL = unicode:characters_to_list(POST_URL),
            pass
        catch
            _ -> fail
        end
    of
        pass -> true;
        fail -> false
    end.
