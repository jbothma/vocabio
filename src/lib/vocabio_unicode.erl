%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc This is currently a dumping ground for unicode convenience utils.
%%%
%%% @end
%%% Created : 15 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(vocabio_unicode).

-export([utf8bytelist_to_nfc_utf8_binary/1]).

-type utf8bytelist() :: [0..255].

-spec utf8bytelist_to_nfc_utf8_binary(utf8bytelist()) -> binary().
utf8bytelist_to_nfc_utf8_binary(UTF8ByteList) ->
    UTF8Binary = list_to_binary(UTF8ByteList),
    CodepointList = unicode:characters_to_list(UTF8Binary),
    NFCCodepointList = ux_string:to_nfc(CodepointList),
    _NFCUTF8Binary = unicode:characters_to_binary(NFCCodepointList).
