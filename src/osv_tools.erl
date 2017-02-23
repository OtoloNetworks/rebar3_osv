-module(osv_tools).

%% API exports
-export([cpio_start/0, cpio/3, cpio_end/1,
         cpio_link/3, cpio_file/3,
         set_cmdline/2]).

-include_lib("kernel/include/file.hrl").

-define(CONNECT_RETRY, 10).  %% 10 attempts
-define(CONNECT_WAIT, 100).  %% Wait 100ms each time

%% Lifted from OSv source...
-define(C_ISDIR, 8#40000).
-define(C_ISREG, 8#100000).
-define(C_ISLNK, 8#120000).
-define(ARGS_OFFSET, 512).

-record(state, {
          base_path = "",
          destination = "",
          socket
}).

%%====================================================================
%% API functions
%%====================================================================
cpio_start() ->
    connect().

cpio(Sock, Src, Dest) ->
    State = #state{
               base_path = Src,
               destination = Dest,
               socket = Sock
              },
    %% Now list dirs...
    upload_dir(Src, State),
    ok.

cpio_end(Sock)->
    %% End of list is just indicated with an empty file called 'TRAILER!!!'
    gen_tcp:send(Sock, pad(cpio_header("TRAILER!!!",  ?C_ISREG bor 16#777, 0, #state{}))),
    ok.

cpio_link(Sock, Orig, Link) ->
    gen_tcp:send(Sock, pad(cpio_header(Link, ?C_ISLNK bor (8#777), length(Orig), #state{}))),
    gen_tcp:send(Sock, pad(erlang:list_to_binary(Orig))),
    ok.

cpio_file(Sock, File, Name) ->
    io:format("File: ~p Name ~p~n", [File, Name]),
    {ok, Bytes} = file:read_file(File),
    gen_tcp:send(Sock, pad(cpio_header(Name, ?C_ISREG bor (8#777),
                                       byte_size(Bytes), #state{}))),
    gen_tcp:send(Sock, pad(Bytes)),
    ok.

set_cmdline(Image, Cmdline) ->
    {ok, FH} = file:open(Image, [raw,read,write]),
    {ok, ?ARGS_OFFSET} = file:position(FH, ?ARGS_OFFSET),
    file:write(FH, Cmdline),
    file:write(FH, <<0:8>>),
    file:close(FH),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
connect() ->
    connect(?CONNECT_RETRY).
connect(0) ->
    connection_failed;
connect(Count) ->
    case gen_tcp:connect("localhost", 10000,
                         [binary, {packet, raw}]) of
        {ok, Sock} ->
            {ok, Sock};
        Error ->
            timer:sleep(?CONNECT_WAIT),
            connect(Count-1)
    end.

%% CPIO Format:
%%
%%   hdr := fmt.Sprintf("%s%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%s\u0000",
%%        "070701",        // magic
%%        0,               // inode
%%        mode,            // mode
%%        0,               // uid
%%        0,               // gid
%%        0,               // nlink
%%        0,               // mtime
%%        filesize,        // filesize
%%        0,               // devmajor
%%        0,               // devminor
%%        0,               // rdevmajor
%%        0,               // rdevminor
%%        len(filename)+1, // namesize
%%        0,               // check
%%        filename)
%%
%% Writes are padded to 4 byte boundaries
%%
%% Directories created by:
%%    cpio.WritePadded(conn, cpio.ToWireFormat(dst, cpio.C_ISDIR|perm, 0))
%%
%% Files:
%%    cpio.WritePadded(conn, cpio.ToWireFormat(dst, cpio.C_ISREG|perm, fi.Size()))
%%    cpio.WritePadded(conn, contents)
%%
%% To finish:
%%    cpio.WritePadded(conn, cpio.ToWireFormat("TRAILER!!!", 0, 0)

convert(N) -> erlang:list_to_binary(io_lib:format("~8.16.0B", [N])).

cpio_header(Filename, Mode, Length, #state{base_path = BP, destination = Destination}) ->
    Pad = <<"00000000">>,
    DestName = Destination ++ lists:subtract(Filename, BP),
    BinFilename = erlang:list_to_binary(DestName),
    ModeBin = convert(Mode),
    LenBin = convert(Length),
    FileNameLenBin = convert(byte_size(BinFilename)+1),
    <<"070701", Pad/binary, ModeBin/binary, Pad/binary, Pad/binary, Pad/binary, Pad/binary,
      LenBin/binary, Pad/binary, Pad/binary, Pad/binary, Pad/binary,
      FileNameLenBin/binary, Pad/binary, BinFilename/binary, 0:8>>.

pad(Binary) ->
    pad(Binary, byte_size(Binary) rem 4).
pad(Binary, 0) -> Binary;
pad(Binary, 1) -> <<Binary/binary, 0:24>>;
pad(Binary, 2) -> <<Binary/binary, 0:16>>;
pad(Binary, 3) -> <<Binary/binary, 0:8>>.

uploader(F, #file_info{type = directory, mode = Mode}, #state{socket = Sock} = State) ->
    gen_tcp:send(Sock, pad(cpio_header(F, ?C_ISDIR bor (Mode band 8#777), 0, State))),
    upload_dir(F, State);
uploader(F, #file_info{type = symlink, mode = Mode}, #state{socket = Sock} = State) ->
   {ok, Link} = file:read_link(F),
   gen_tcp:send(Sock, pad(cpio_header(F, ?C_ISLNK bor (Mode band 8#777), length(Link), State))),
   gen_tco:send(Sock, pad(erlang:list_to_binary(Link)));
uploader(F, #file_info{type = regular, mode = Mode}, #state{socket = Sock} = State) ->
    {ok, Bytes} = file:read_file(F),
    gen_tcp:send(Sock, pad(cpio_header(F, ?C_ISREG bor (Mode band 8#777),
                                       byte_size(Bytes), State))),
    gen_tcp:send(Sock, pad(Bytes));
uploader(F, _Sock, FI) ->
    io:format("Ignoring F: ~p ~p~n", [F, FI]).

upload_dir(Path, State) ->
    {ok, Filenames} = file:list_dir(Path),
    lists:map(fun(F) ->
                      D = Path ++ "/" ++ F,
                      {ok, FileInfo} = file:read_link_info(D),
                      uploader(D, FileInfo, State) end, Filenames).
