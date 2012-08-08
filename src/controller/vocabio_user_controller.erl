%%% unauthed -> sign up -> openid start -> openid return -> create user -> login -> authed
%%% unauthed -> sign in -> openid start -> openid return -> login -> authed
-module(vocabio_user_controller, [Req, SessionID]).

%% controller web actions
-export([create/2, do_signin/2, signin/2, signup/2, signout/2, openid/2]).

signin('GET', []) ->
    ok = boss_session:set_session_data(SessionID, authstate, signin),
    {redirect, "/user/openid/start"}.

signup('GET', []) ->
    ok = boss_session:set_session_data(SessionID, authstate, signup),
    {redirect, "/user/openid/start"}.

signout('GET', []) ->
    ok = boss_session:delete_session(SessionID),
    {redirect, "/"}.


create('GET', []) ->
    ok;

create('POST', []) ->
    DisplayName = Req:post_param("display_name"),
    Email = Req:post_param("email"),
    OpenID = boss_session:get_session_data(SessionID, openid),
    User = juser:new(id, DisplayName, Email),
    {ok, SavedUser} = User:save(),
    io:format("~p~n", [SavedUser]),
    UserOpenID = user_openid:new(id, SavedUser:id(), OpenID),
    {ok, SavedUserOpenID} = UserOpenID:save(),
    io:format("~p~n", [SavedUserOpenID]),
    ok = boss_session:remove_session_data(SessionID, openid),
    ok = boss_session:set_session_data(SessionID, user_openid, SavedUserOpenID),
    {redirect, "/user/do_signin"}.

do_signin('GET', []) ->
    ok = boss_session:remove_session_data(SessionID, authstate),
    UserOpenID = boss_session:get_session_data(SessionID, user_openid),
    User = boss_db:find(UserOpenID:user_id()),
    ok = boss_session:set_session_data(SessionID, user, User),
    {redirect, "/"}.


%% GET handler for /openid/start
openid('GET', ["start"]) ->
    Prepare = {prepare, SessionID, "https://www.google.com/accounts/o8/id", true},
    {ok, AuthReq} = gen_server:call(openid_srv, Prepare),
    BaseUrl =  "http://localhost:8001/",
    ReturnUrl = BaseUrl ++ "user/openid/return",
    Url = openid:authentication_url(AuthReq, ReturnUrl, BaseUrl),
    {redirect, Url};

%% GET handler for /openid/return
openid('GET', ["return"]) ->
    openid_return();
openid('POST', ["return"]) ->
    openid_return().

openid_return() ->
    BaseUrl =  "http://localhost:8001/",
    ReturnUrl = BaseUrl ++ "user/openid/return",
    AuthState = boss_session:get_session_data(SessionID, authstate),
    io:format("~p~n", [AuthState]),
    Verify = {verify, SessionID, ReturnUrl, Req:query_params()},
    {ok, OpenID} = gen_server:call(openid_srv, Verify),
    OpenIDSearch = boss_db:find(user_openid, [{open_id, equals, OpenID}]),
    io:format("~p~n", [OpenIDSearch]),
    case OpenIDSearch of
        [UserOpenID] when AuthState == signin ->
            ok = boss_session:set_session_data(SessionID, user_openid, UserOpenID),
            {redirect, "/user/do_signin"};
        [] when AuthState == signup ->
            ok = boss_session:set_session_data(SessionID, openid, OpenID),
            {redirect, "/user/create"};
        [] when AuthState == signin ->
            FlashMsg = "Your Google ID isn't known to us. Please sign up now.",
            boss_flash:add(SessionID, notice, "Please sign up", FlashMsg),
            {redirect, "/user/signup"}
    end.
