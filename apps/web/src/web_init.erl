-module(web_init).

%% Include files

-include_lib("aplib/include/apmacros.hrl").

%% Exported functions

-export([
    start_cowboy/5
]).

%% API

start_cowboy(BindIp, BindPort, Acceptors, Secure, Production) ->
    % Get paths, make up index and favicon
    WebDir = filename:join([filename:absname(""), "web"]),
    SslDir = filename:join([WebDir, "ssl"]),
    {ok, UploadsDir} = web_config:uploads_dir(),
    {ok, ApiKey} = web_config:api_key(),

    % Compile route dispatcher
    Dispatch = cowboy_router:compile([
        {'_', [
            % Ingestion
            {"/ingest/:app/:component/:branch/:version", web_check_api_key, #{
                api_key => ApiKey,
                next => web_rest_ingest
            }},

            % Ingestion: NDJSON
            {"/ingest-ndjson/:app/:component/:branch/:version", web_check_api_key, #{
                api_key => ApiKey,
                next => web_rest_ingest_ndjson
            }},

            % Auth API
            {"/api/auth/login", web_rest_auth_login, []},
            {"/api/auth/logout", web_rest_auth_logout, []},
            {"/api/auth/profile", web_rest_auth_profile, []},

            % Common API
            {"/api/apps", web_rest_apps, []},
            {"/api/:app/:component/:branch/logs", web_rest_logs, []},
            {"/api/:app/:component/:branch/logs/metadata", web_rest_logs_metadata, []},
            {"/api/:app/:component/:branch/logs/versions", web_rest_logs_versions, []},
            {"/api/:app/:component/:branch/logs/timeline", web_rest_logs_timeline, []},
            {"/api/:app/:component/:branch/errors", web_rest_errors, []},
            {"/api/:app/:component/:branch/crash-reports", web_rest_crash_reports, []},
            {"/api/:app/:component/:branch/crash-reports/metadata", web_rest_crash_reports_metadata, []},
            {"/api/:app/:component/:branch/crash-reports/versions", web_rest_crash_reports_versions, []},
            {"/api/:app/:component/:branch/crash-reports/pdb/:version", web_rest_crash_reports_pdb, []},

            % Uploads
            {"/:app/:component/:branch/crash-reports/upload", web_crash_report_upload, []},

            % Downloads
            {"/download/crash-reports/:app/:filename/logs", web_crash_report_log_download, []},
            {"/download/crash-reports/[...]", cowboy_static, {dir, UploadsDir}},

            % WebSocket
            {"/ws", web_ws, #{api_key => ApiKey}}
        ]}
    ]),

    % Define middlewares
    Middlewares = [
        web_mw_cors,
        web_mw_authenticate,
        web_mw_authorize,
        cowboy_router,
        cowboy_handler
    ],

    % Define SSL options
    SslOpts = get_ssl_opt(cacertfile, SslDir) ++ get_ssl_opt(certfile, SslDir) ++ get_ssl_opt(keyfile, SslDir),

    % Define server starter, options and environment
    ServerName = iolist_to_binary(io_lib:format("~s_~s", [chronos_web, ?yesno(Secure, https, http)])),
    StarterFun = case Secure of true -> fun cowboy:start_tls/3; false -> fun cowboy:start_clear/3 end,
    Opts = [{ip, BindIp}, {port, BindPort}],
    OptsMaybeWithSsl = ?yesno(Secure, Opts ++ SslOpts, Opts),

    % Define path regex
    SkipAuthenticateRegex = [
        <<"^/ingest/">>,
        <<"^/ingest-ndjson/">>,
        <<"^/ws">>,
        <<"^/[^/]*/[^/]*/[^/]*/crash-reports/upload$">>
    ],

    SkipAuthorizeRegex = [<<"^/api/auth/">> | SkipAuthenticateRegex],

    % Start server
    ProtocolOpts = #{
        env => #{dispatch => Dispatch, skip_authenticate_regex => SkipAuthenticateRegex, skip_authorize_regex => SkipAuthorizeRegex},
        middlewares => Middlewares
    },

    StarterFun(ServerName, OptsMaybeWithSsl, ProtocolOpts).

%% Local functions

absolute_or_local(FilePath, LocalSslDir) ->
    case filelib:is_regular(FilePath) of
        true -> FilePath;
        false -> filename:join([LocalSslDir, FilePath])
    end.

get_ssl_opt(Param, LocalSslDir) ->
    Result = case Param of
        cacertfile -> web_config:cacertfile();
        certfile -> web_config:certfile();
        keyfile -> web_config:keyfile();
        _ -> undefined
    end,
    case Result of
        {ok, Value} -> [{Param, absolute_or_local(Value, LocalSslDir)}];
        undefined -> []
    end.
