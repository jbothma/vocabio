-- module(juser, [Id, DisplayName, Email]).

create table if not exists jusers (
    id INTEGER AUTO_INCREMENT PRIMARY KEY,
    display_name varchar(100),
    email varchar(30)
);

-- module(openid_provider, [Id, Endpoint, FriendlyName]).

create table if not exists openid_providers (
    id INTEGER AUTO_INCREMENT PRIMARY KEY,
    endpoint varchar(100),
    friendly_name varchar(100)
);

-- module(user_openid, [Id, UserID, OpenIDIdentifier]).

create table if not exists user_openids (
       id INTEGER AUTO_INCREMENT PRIMARY KEY,
       user_id integer,
       open_id_identifier varchar(100)
);

-- module(word, [Id, JuserId, Word, ModDatetime]).

create table if not exists words (
       id INTEGER AUTO_INCREMENT PRIMARY KEY,
       juser_id integer,
       word varchar(30),
       mod_datetime datetime
);

-- module(word_instance, [Id, WordId, Datetime, Source]).

create table if not exists word_instances (
       id INTEGER AUTO_INCREMENT PRIMARY KEY,
       word_id integer,
       `datetime` datetime,
       source varchar(100)
);
