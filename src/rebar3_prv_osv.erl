-module(rebar3_prv_osv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, generate).
-define(NAMESPACE, osv).
-define(DEPS, [{default, app_discovery}, {default, release}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 osv generate"},
            {opts, []},
            {short_desc, "An OSv image generator"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    exec:start([]),
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    %% io:format("Apps: ~p~n", [Apps]),
    [begin
         %% Setup stuff
         App = erlang:binary_to_list(rebar_app_info:name(AppInfo)),
         OutDir = rebar_dir:base_dir(State),
         ReleaseDir = filename:join([rebar_dir:base_dir(State), "rel", App]),
         PrivDir = code:priv_dir(rebar3_osv),
         Params = rebar_state:get(State, relx, []),
         Verbose =  proplists:get_value(verbose, Params, false),

         %% Select an ERTS to use, so we can symlink it to /otp/erts later...
         {ok, Files} = file:list_dir(ReleaseDir),
         ERTSdirs = lists:filter(fun(F) -> lists:prefix("erts", F) end, Files),
         ERTS = "/otp/" ++ hd(ERTSdirs),

         %% Step 1 - make a copy of the OSv image we're using..
         BaseImage = filename:join([PrivDir, "OSv.img"]),
         OrigImage = proplists:get_value(osv_image, Params, BaseImage),
         NewImage = filename:join([OutDir, App ++ ".img"]),
         rebar_log:log(info, "Using base OSv Image ~s", [OrigImage]),
         {ok, _Size} = file:copy(OrigImage, NewImage),

         %% Step 1a - alter command line
         osv_tools:set_cmdline(NewImage,
                               "--norandom --noinit /tools/cpiod.so; /zfs.so set compression=off osv"),

         %% Step 2 - Start the qemu system with the NewImage...
         QemuCmd = "qemu-system-x86_64 -m 512 -smp 1 -vnc none "
             %% ++ "-gdb tcp::1234,server,nowait "
             ++ "-device virtio-blk-pci,id=blk0,bootindex=0,drive=hd0,scsi=off "
             ++ "-drive file=" ++ NewImage ++ ",if=none,id=hd0,cache=unsafe,aio=threads "
             ++ "-netdev user,id=un0,net=192.168.122.0/24,host=192.168.122.1 "
             ++ "-device virtio-net-pci,netdev=un0 -redir tcp:10000::10000 -device virtio-rng-pci "
             ++ "-enable-kvm -cpu host,+x2apic -chardev stdio,mux=on,id=stdio,signal=on "
             ++ "-mon chardev=stdio,mode=readline,default -device isa-serial,chardev=stdio",
         {ok, QPid, QOSPid} = exec:run(QemuCmd, [monitor
                                                 %%, {stdout, "/tmp/osv-cpio-output"}
                                                ]),

         %% Step 3 - CPIO in the release into the OTP directory
         {ok, S} = osv_tools:cpio_start(),
         ok = osv_tools:cpio(S, ReleaseDir, "/otp"),
         ok = osv_tools:cpio_link(S, ERTS, "/otp/erts"),
         ok = osv_tools:cpio_file(S, filename:join([PrivDir, "start-otp.so"]), "start-otp.so"),
         ok = osv_tools:cpio_end(S),

         %% Wait on the Qemu image to terminate
         receive
             {'DOWN', QOSPid, process, QPid, normal} ->
                 %% Step 4 - set the command line...
                 Vsn = rebar_app_info:original_vsn(AppInfo),
                 CmdLine = lists:flatten(
                             io_lib:format("~s/start-otp.so /otp/releases/~s/~s "
                                           ++ "/otp/releases/~s/vm.args /otp/releases/~s/sys.config",
                                           [case Verbose of true -> "--verbose "; _ -> "" end,
                                            Vsn, App, Vsn, Vsn])),
                 ok = osv_tools:set_cmdline(NewImage, CmdLine),
                 rebar_log:log(info, "OSv image built: ~s", [NewImage]),
                 rebar_log:log(info, "Command Line: ~s", [CmdLine]);
             {'DOWN', QOSPid, process, QPid, FailReason} ->
                 rebar_log:log(error, "Qemu failed: ~p~n", [FailReason]),
                 throw(qemu_error)
         after 10000 ->
                 rebar_log:log(error, "Qemu failed"),
                 throw(qemu_timeout)
         end
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% exc_compile(_Opts, Source, OutDir) ->
%%     {ok, Binary} = file:read_file(Source),
%%     OutFile = filename:join([OutDir, "priv", filename:basename(Source)]),
%%     filelib:ensure_dir(OutFile),
%%     rebar_api:info("Writing out ~s", [OutFile]),
%%     file:write_file(OutFile, Binary).
