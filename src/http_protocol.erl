-module(http_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
	ok = ranch:accept_ack(Ref),
    % io:fwrite(Opts, []),
	loop(Socket, Transport, Opts).

loop(Socket, Transport, Dir) ->
    case Transport:recv(Socket, 0, 5000) of
		{ok, Data} when Data =/= <<4>> ->
            % io:fwrite("~n***********~n", []),
			% io:fwrite(Data, []),
			% io:fwrite("~n***********~n", []),
            protocol(Data, Socket, Transport, Dir),
            ok = Transport:close(Socket);
			% loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.

protocol(<<"HEAD " , RawData/binary>>, Socket, Transport, Dir) ->
    % io:fwrite("~n***********GOT HEAD~n", []),
    send_file_header(RawData, Socket, Transport, Dir);
protocol(<<"GET " , RawData/binary>>, Socket, Transport, Dir) ->
    % io:fwrite("~n***********GOT GET~n", []),
    File = send_file_header(RawData, Socket, Transport, Dir),
    file:sendfile(File, Socket);
protocol(<<_ , _/binary>>, Socket, Transport, _) ->
    % io:fwrite("~n*********** IGNORE~n", []),
    Transport:send(Socket, header_compile()).

send_file_header(RawData, Socket, Transport, Dir) ->
    [Doc, _] = binary:split(RawData,<<" ">>),
    % Check = binary:split(Doc,<<"/">>),
    % io:fwrite(handle_hack(Check), []),
    BinDir = unicode:characters_to_binary(Dir),
    % DocNoPers = unicode:characters_to_binary(http_uri:decode(binary_to_list(Doc))),
    [DocNoParam | _] = binary:split(Doc,<<"?">>),
    FileToCheck = <<BinDir/binary, DocNoParam/binary>>,
    File = handle_directory(filename:extension(FileToCheck), FileToCheck),
    % io:fwrite(File, []),
    Transport:send(Socket, header_compile(File)),
    File.

header_compile(File) ->
    FileSize = filelib:file_size(File),
    Code = case FileSize of
        0 -> not_found(File);
        _ -> success_info(File, FileSize)
    end,
    % Start = <<"HTTP/1.1 ">>,
    % io:fwrite("~n/*****/~n", []),
    % io:fwrite(filename:extension(File), []),
    ["HTTP/1.1 ", Code, general_info()].
header_compile() ->
    ["HTTP/1.1 405 Method not allowed", general_info()].

not_found(File) ->
    MargToIndex = byte_size(File) - 10,
    <<_:MargToIndex/binary, F/binary>> = File,
    case F of
        <<"index.html">> -> <<"403 Forbidden">>;
        _ -> <<"404 Not found">>
    end.

general_info() ->
    ["\r\nServer: Ermakov httpd\r\nDate: ", date_for_header(), "\r\nConnection: close\r\n\r\n"].

success_info(File, Size) ->
    Size_str = integer_to_list(Size),
    ["200 OK\r\nContent-Type: ", mime_type(File), "\r\nContent-Length: ", Size_str].

mime_type(File) ->
    case filename:extension(File) of
        <<".html">> -> <<"text/html">>;
        <<".css">>  -> <<"text/css">>;
        <<".js">>   -> <<"application/x-javascript">>;
        <<".jpg">>  -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".png">>  -> <<"image/png">>;
        <<".gif">>  -> <<"image/gif">>;
        <<".swf">>  -> <<"application/x-shockwave-flash">>;
        <<".", _/binary>> -> <<"undefined">>;
        <<".">> -> <<"undefined">>;
        _ ->
            <<"directory">>
    end.

handle_directory(<<".", _/binary>>, File) ->
    File;
handle_directory(_, File) ->
    Fin1 = <<"index.html">>,
    Fin2 = <<"/index.html">>,
    Marg = byte_size(File) - 1,
    <<_:Marg/binary, L/binary>> = File,
    case L of
        <<"/">> -> <<File/binary, Fin1/binary>>;
        _ -> <<File/binary, Fin2/binary>>
    end.

date_for_header() ->
    {Date, {Hours, Minutes, Seconds}} = calendar:universal_time(),
    DayOfWeek = element(calendar:day_of_the_week(Date), {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}),
    {Year, MonthNumber, Day} = Date,
    Month = element(MonthNumber, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}),
    io_lib:format("~s, ~B ~s ~B ~2..0B:~2..0B:~2..0B GMT", [DayOfWeek, Day, Month, Year, Hours, Minutes, Seconds]).

% handle_hack([Check | Rest]) ->
%     case Check of
%         <<"..">> -> <<"hack">>;
%         _ -> handle_hack(Rest)
%     end;
% handle_hack(Check) ->
%     case Check of
%         <<"..">> -> <<"hack">>;
%         _ -> <<"OK">>
%     end.
