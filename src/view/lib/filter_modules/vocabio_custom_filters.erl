-module(vocabio_custom_filters).
-compile(export_all).

%% hack off the type- part of CB IDs to leave only the numeric part.
%% This is intended to be a temporary hack until simple_bridge supports CDATA
%% in <input name= fields.
id_int(Id) when is_binary(Id) ->
    id_int(binary_to_list);
id_int(Id) when is_list(Id) ->
    string:substr(Id, string:chr(Id, $-)+1, string:len(Id)).
