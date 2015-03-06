-module(crell_web).
-export([start/0,
         stop/1
    ]).

start() ->
    {ok,Port} = port(),
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    {ok, Pid} = cowboy:start_http(http,
                                _ConnectionPoolSize=10,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]},
                                 {max_keepalive, 50},
                                 %% {onrequest, fun timely_session:on_request/1},
                                 {timeout, 500}
                                ]
                               ),
    {ok,Pid}.

routes() ->
    [
     {'_',
        [
            {"/crell_proc/data", crell_proc, []},
            {"/crell_proc/apps", crell_apps, []},
            {"/", cowboy_static, {priv_file, crell, "www/index.html"}},
            {"/[...]",           cowboy_static, {priv_dir, crell, "/www"}}
        ]
     }
    ].

port() ->
    Port = 9876,
    {ok,application:get_env(crell,http_port, Port)}.

stop(Pid) ->
    cowboy:stop_listener(Pid).


% TODO: maybe just show interesting apps,

 % rp ( [ crell_server:calc_proc(pid(0,X,0)) || X <- lists:seq(1,32767) ] ).

%% and filter out these ( NON interesting apps )

 %  {ok,{"<0.32496.0>",[{<0.32496.0>,"<0.32496.0>"}],[],[]}},
 % {ok,{"<0.32497.0>",[{<0.32497.0>,"<0.32497.0>"}],[],[]}},