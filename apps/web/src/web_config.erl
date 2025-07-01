-module(web_config).

%% Exported functions

-export([
    bind_ip/0,
    bind_port/0,
    acceptors/0,
    max_upload_size/0,
    proxy_enabled/0,
    secure/0,
    cacertfile/0,
    certfile/0,
    keyfile/0,
    url/0,
    tmp_dir/0,
    uploads_dir/0,
    api_key/0,
    auth_realm/0
]).

%% API

-spec bind_ip() ->
    {'ok', inet:ip4_address()} | 'undefined'.

bind_ip() ->
    case application:get_env(web, bind_ip, {0, 0, 0, 0}) of
        {_A, _B, _C, _D} = IpAddressV4 -> {ok, IpAddressV4};
        _ -> undefined
    end.

-spec bind_port() ->
    {'ok', inet:port_number()} | 'undefined'.

bind_port() ->
    case application:get_env(web, bind_port, 8080) of
        BindPort when is_integer(BindPort) -> {ok, BindPort};
        _ -> undefined
    end.

-spec acceptors() ->
    {'ok', non_neg_integer()} | 'undefined'.

acceptors() ->
    case application:get_env(web, acceptors, 5) of
        Acceptors when is_integer(Acceptors) -> {ok, Acceptors};
        _ -> undefined
    end.

-spec max_upload_size() ->
    {'ok', non_neg_integer()} | 'undefined'.

max_upload_size() ->
    case application:get_env(web, max_upload_size, 32 * 1024 * 1024) of
        MaxUploadSize when is_integer(MaxUploadSize) -> {ok, MaxUploadSize};
        _ -> undefined
    end.

-spec proxy_enabled() ->
    {'ok', boolean()} | 'undefined'.

proxy_enabled() ->
    case application:get_env(web, proxy_enabled, false) of
        ProxyEnabled when is_boolean(ProxyEnabled) -> {ok, ProxyEnabled};
        _ -> undefined
    end.

-spec secure() ->
    {'ok', boolean()} | 'undefined'.

secure() ->
    case application:get_env(web, secure, false) of
        Secure when is_boolean(Secure) -> {ok, Secure};
        _ -> undefined
    end.

-spec cacertfile() ->
    {'ok', binary()} | 'undefined'.

cacertfile() ->
    case application:get_env(web, cacertfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec certfile() ->
    {'ok', binary()} | 'undefined'.

certfile() ->
    case application:get_env(web, certfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec keyfile() ->
    {'ok', binary()} | 'undefined'.

keyfile() ->
    case application:get_env(web, keyfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec url() ->
    {'ok', binary()} | 'undefined'.

url() ->
    case application:get_env(web, url, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec tmp_dir() ->
    {'ok', binary()} | 'undefined'.

tmp_dir() ->
    case application:get_env(web, tmp_dir, undefined) of
        Str when is_list(Str) -> {ok, iolist_to_binary(Str)};
        BinStr when is_binary(BinStr) -> {ok, BinStr};
        _ -> undefined
    end.

-spec uploads_dir() ->
    {'ok', binary()} | 'undefined'.

uploads_dir() ->
    case application:get_env(web, uploads_dir, undefined) of
        Str when is_list(Str) -> {ok, iolist_to_binary(Str)};
        BinStr when is_binary(BinStr) -> {ok, BinStr};
        _ -> undefined
    end.

-spec api_key() ->
    {'ok', binary()} | 'undefined'.

api_key() ->
    case application:get_env(web, api_key, undefined) of
        Str when is_list(Str) -> {ok, iolist_to_binary(Str)};
        BinStr when is_binary(BinStr) -> {ok, BinStr};
        _ -> undefined
    end.

-spec auth_realm() ->
    {'ok', atom()} | 'undefined'.

auth_realm() ->
    case application:get_env(web, auth_realm, undefined) of
        undefined -> undefined;
        AuthRealm -> {ok, AuthRealm}
    end.

%% Local functions
