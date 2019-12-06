%%%-------------------------------------------------------------------
%%% @author Rick Payne <rickp@OtoloNetworks.com>
%%% @copyright (C) 2017, Otolo Networks
%%% @doc

%%%

%%% @end
%%% Created :  6 May 2015 by Rick Payne <rickp@otolonetworks.com>
%%%-------------------------------------------------------------------
-module(nbd_client).

-export([start/1,
         read/3, write/3,
         disconnect/1]).

-define(CONNECT_RETRY, 10).  %% 10 attempts
-define(CONNECT_WAIT, 100).  %% Wait 100ms each time

-record(nbd, 
        { file
        , port
        , nbd_pid
        , socket
        , style
        , flags
        , request = 0
        }).

start(File) ->
    %% Pick a random port and start the qemu-nbd process on the file
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> =
        crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    Port = 10000 + rand:uniform(60000-10000),
    Cmd = lists:flatten(io_lib:format("qemu-nbd -p ~p ~s", [Port, File])),
    rebar_log:log(debug, "Starting qemu-nbd", []),
    {ok, QPid, _OSPid} = exec:run(Cmd, [monitor]),
    
    %% Now to connect to the process...
    {ok, Sock} = connect(Port),

    NBD = #nbd{file = File, port = Port, nbd_pid = QPid,
               socket = Sock},
    rebar_log:log(debug, "Connected...", []),
    {ok, handle_initial_handshake(NBD)}.

handle_initial_handshake(#nbd{socket = Sock} = State) ->
    rebar_log:log(debug, "Receiving initial handshake", []),
    {ok, Bytes} = gen_tcp:recv(Sock, 16),
    handle_initial_handshake_1(State, Bytes).

handle_initial_handshake_1(#nbd{socket = Sock} = State,
                           <<"NBDMAGIC", 16#420281861253:64>>) ->
    rebar_log:log(debug, "Parsing initial handoshake (old style)", []),
    {ok, <<_Size:64, Flags:32>>} = gen_tcp:recv(12),
    {ok, _} = gen_tcp:recv(Sock, 124),
    State#nbd{flags = Flags, style = old};
handle_initial_handshake_1(#nbd{socket = Sock} = State,
                           <<"NBDMAGIC", 16#49484156454F5054:64>>) ->
    rebar_log:log(debug, "Parsing initial handshake (new style)", []),
    {ok, Flags} = gen_tcp:recv(Sock, 2),
    %% Send flags and then the 'EXPORT_NAME' option with no name...
    ok = gen_tcp:send(Sock, <<0:32, 16#49484156454F5054:64, 1:32, 0:32>>),
    {ok, _} = gen_tcp:recv(Sock, 134),
    State#nbd{flags = Flags, style = new}.


write(Data, Offset, #nbd{} = State) ->
    %% First step is to read in the sectors that include our target block...
    Len = byte_size(Data),
    {Sector, SOffset} = map_offset(Offset),
    {0, SectorData, State2} = read(Sector, Len, State),
    %% Now splice out the start and end parts
    <<First:SOffset/binary, _:Len/binary, Second/binary>> = SectorData,
    %% Write the new sectors out with our new data spliced in...
    send_request(build_header(write, Sector,
                              <<First/binary, Data/binary, Second/binary>>,
                              State2), State2),
    %% Not expecting anything back, just assert errno is 0
    {0, _, State3} = parse_reply(0, State2),
    {0, State3}.

disconnect(#nbd{socket = Sock} = State) ->
    send_request(build_header(disconnect, State), State),
    gen_tcp:close(Sock),
    ok.

read(Offset, Len, #nbd{} = State) ->
    {_Sector, SOffset} = map_offset(Offset),
    ToRead = map_round(Len + SOffset),
    send_request(build_header(read, Offset, ToRead, State), State),
    parse_reply(ToRead, State).

send_request(Request, #nbd{socket = Sock}) ->
    gen_tcp:send(Sock, Request).

connect(Port) ->
    connect(Port, ?CONNECT_RETRY).

connect(_Port, 0) ->
    connection_failed;
connect(Port, Count) ->
    case gen_tcp:connect("localhost", Port,
                         [binary, {active, false}, {packet, raw}]) of
        {ok, Sock} ->
            {ok, Sock};
        _Error ->
            timer:sleep(?CONNECT_WAIT),
            connect(Port, Count-1)
    end.

map_offset(Offset) ->
    Sectors = 512 * (Offset div 512),
    {Sectors, Offset - Sectors}.

map_round(Count) ->
    ((Count div 512)+1) * 512.

build_header(read, Offset, Len, #nbd{request = RQ}) ->
    <<16#25609513:32, 0:32, RQ:64, Offset:64, Len:32>>;
build_header(write, Offset, Data, #nbd{request = RQ}) ->
    <<16#25609513:32, 1:32, RQ:64, Offset:64, (byte_size(Data)):32, Data/binary>>.

build_header(disconnect, #nbd{request = RQ}) ->
    <<16#25609513:32, 2:32, RQ:64, 0:64, 0:32>>;
build_header(flush, #nbd{request = RQ}) ->
    <<16#25609513:32, 3:32, RQ:64, 0:64, 0:32>>.


parse_reply(Len, #nbd{request = RQ, socket = Sock} = NBD) ->
    {ok, <<16#67446698:32, Errno:32, RQ:64, Data:Len/binary>>}
        = gen_tcp:recv(Sock, Len + 4 + 4 +8),
    {Errno, Data, NBD#nbd{request = RQ+1}}.
