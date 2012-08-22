%%% unauthed -> sign up -> openid start -> openid return -> create user -> login -> authed
%%% unauthed -> sign in -> openid start -> openid return -> login -> authed
-module(vocabio_user_controller, [Req, SessionID]).

%% controller web actions
-export([create/2, do_signin/2, signin/2, signup/2, signout/2, openid/2]).


signin('GET', []) ->
    ok = boss_session:set_session_data(SessionID, authstate, signin),
    {redirect, "/user/openid/choose"}.


signup('GET', []) ->
    case boss_session:get_session_data(SessionID, openid) of
        undefined ->
            ok = boss_session:set_session_data(SessionID, authstate, signup),
            {redirect, "/user/openid/choose"};
        _ ->
            {redirect, "/user/create"}
    end.


signout('GET', []) ->
    ok = boss_session:delete_session(SessionID),
    {redirect, "/"}.


create('GET', []) ->
    ok;

create('POST', []) ->
    DisplayName = vocabio_unicode:utf8bytelist_to_nfc_utf8_binary(
                    Req:post_param("display_name")),
    Email = vocabio_unicode:utf8bytelist_to_nfc_utf8_binary(
              Req:post_param("email")),
    OpenID = boss_session:get_session_data(SessionID, openid),
    User = juser:new(id, DisplayName, Email),
    {ok, SavedUser} = User:save(),
    UserOpenID = user_openid:new(id, SavedUser:id(), OpenID),
    {ok, SavedUserOpenID} = UserOpenID:save(),
    ok = boss_session:remove_session_data(SessionID, openid),
    ok = boss_session:set_session_data(SessionID, user_openid, SavedUserOpenID),
    {redirect, "/user/do_signin"}.


do_signin('GET', []) ->
    ok = boss_session:remove_session_data(SessionID, authstate),
    UserOpenID = boss_session:get_session_data(SessionID, user_openid),
    User = boss_db:find(UserOpenID:user_id()),
    ok = boss_session:set_session_data(SessionID, user, User),
    {redirect, "/"}.


openid('GET', ["choose"]) ->
    case boss_db:find(openid_provider, []) of
        Providers when is_list(Providers) ->
            {ok, [{providers, Providers}]}
    end;

%% GET handler for /openid/start
openid('GET', ["start"]) ->
    Endpoint = Req:query_param("endpoint"),
    Prepare = {prepare, SessionID, Endpoint, true},
    {ok, AuthReq} = gen_server:call(openid_srv, Prepare),
    {ok, BaseUrl} =  application:get_env(vocabio, vocabio_url),
    ReturnUrl = BaseUrl ++ "user/openid/return",
    Url = openid:authentication_url(AuthReq, ReturnUrl, BaseUrl),
    {redirect, Url};

%% GET handler for /openid/return
openid('GET', ["return"]) ->
    openid_return();
openid('POST', ["return"]) ->
    openid_return().

openid_return() ->
    {ok, BaseUrl} =  application:get_env(vocabio, vocabio_url),
    ReturnUrl = BaseUrl ++ "user/openid/return",
    AuthState = boss_session:get_session_data(SessionID, authstate),
    Verify = {verify, SessionID, ReturnUrl, Req:query_params()},
    {ok, OpenID} = gen_server:call(openid_srv, Verify),
    OpenIDBin = vocabio_unicode:utf8bytelist_to_nfc_utf8_binary(OpenID),
    OpenIDSearch = boss_db:find(user_openid, [{open_id, equals, OpenIDBin}]),
    case OpenIDSearch of
        [UserOpenID] when AuthState == signin ->
            ok = boss_session:set_session_data(SessionID, user_openid, UserOpenID),
            {redirect, "/user/do_signin"};
        [] when AuthState == signup ->
            ok = boss_session:set_session_data(SessionID, openid, OpenIDBin),
            {redirect, "/user/create"};
        [] when AuthState == signin ->
            ok = boss_session:set_session_data(SessionID, openid, OpenIDBin),
            FlashMsg = "Your Google ID isn't known to us. Please sign up now.",
            boss_flash:add(SessionID, notice, "Please sign up", FlashMsg),
            {redirect, "/user/signup"}
    end.
