%% Copyright (c) 2011, Wes James <comptekki@gmail.com>
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of "ECSMCons" nor the names of its contributors may be
%%       used to endorse or promote products derived from this software without
%%       specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%% 
%%

-module(ecom).

-export([start/0, send_com/2, send_com/4, rec_com/0]).

-include("ecom.hrl").

start() ->
    register(rec_com, spawn(ecom, rec_com, [])).

send_com(Box, Com, Args, Rec_Node) ->
    {rec_com, Rec_Node} ! {Box, Com, Args, self()},
    send_com(Box, Com).

send_com(Box, Com) ->
    receive
        {ok, Z} ->
            io:format("Com received on server....: ~s~n", [Z]),
			send_com(Box, Com);
		{ok, done, Msg} ->
			Msg;
		ok ->
			io:format("Com done receiving....~n")
        after 1000 ->
			io:format("~n box: ~p - com: ~p - Didn't receive message back from server after 1 second...! ~n",[Box,Com]),
			{Box,Com,"command not processed after 1 second...!"}
    end.

rec_com() ->
    receive
        finished ->
            io:format("finished~n", []);
        {Box, Com, Args, Msg_PID} ->
            process_msg(Box, Com, Args, Msg_PID),
            rec_com()
		after 60000 ->
				{hanwebs, ?NODE_AT_HOST} ! {comp_name()++?DOMAIN++"/pong",self()},
				{hanwebs, ?NODE_AT_HOST} ! {comp_name()++?DOMAIN++"/loggedon/"++logged_on(),self()},
				rec_com()
    end.

process_msg(Box, Com, Args, Msg_PID) ->
    case Com of
%        "ls" ->
%           [Msg_PID ! {ok, F} || F <- filelib:wildcard("/erl/*")];
		"com" ->
			case Args of
				"mkuploads" ->
					os:cmd("mkdir "++?UPLOADS_DIR),
					Msg_PID ! {ok, done, {Box, "mkdir "++?UPLOADS_DIR}};
				"anycmd" ->
					os:cmd(?UPLOADS_DIR++"any.cmd");
				"ninitecmd" ->
					os:cmd(?UPLOADS_DIR++"ninite.cmd");
				"ninite" ->
					{Year,Month,Day}=date(),
					Date=lists:flatten(io_lib:format("~p~2..0B~2..0B",[Year,Month,Day])),
					os:cmd("c:/erl/uploads/NiniteOne.exe /updateonly /exclude Python /silent "++?UPLOADS_DIR++"\\"++Date++"log.txt");
				"ninitemlog" ->
					{Year,Month,Day}=date(),
					Date=lists:flatten(io_lib:format("~p~2..0B~2..0B",[Year,Month,Day])),
					{ok, Log}=file:read_file(?UPLOADS_DIR++"\\"++Date++"log.txt"),
					Msg_PID ! {ok, done, {Box, "ninitemlog", Log}};		
				"listupfls" ->
					Msg_PID ! {ok, done, {Box, "listupfls", list_up_fls()}};		
				Unsupported -> Unsupported
			end,
            Msg_PID ! {ok, done, {Box, "com: "++Args}};
		"loggedon" ->
			Msg_PID ! {ok, done, {Box, "loggedon", logged_on()}};
		"copy" ->
			{FileName, Data} = Args,
			case FileName of
				"ecom.beam" ->
									{ok, File} = file:open(?ERL_DIR++FileName, [write]); 
					      _ ->
									{ok, File} = file:open(?UPLOADS_DIR++FileName, [write])
			end,
			file:write(File,Data), 
			file:close(File),
            Msg_PID ! {ok, done, {Box, "copied: "++ FileName}};
        "dffreeze" ->
            os:cmd(?DFC_DIR++" "++?DFC_PASSWD++" /BOOTFROZEN"),
            Msg_PID ! {ok, done, {Box, "dffreeze"}};
        "dfthaw" ->
            os:cmd(?DFC_DIR++" "++?DFC_PASSWD++" /BOOTTHAWED"),
            Msg_PID ! {ok, done, {Box, "dfthaw"}};
        "dfstatus" ->
            Output=os:cmd("C:/erl/df-status.cmd"),
            Msg_PID ! {ok, done, {Box, "dfstatus", string:left(Output,length(Output)-2)}};
        "ping" ->
%            Msg_PID ! {ok, pong},
			Msg_PID ! {ok, done, {Box, "pong"}};
		"net_stop" ->
			init:stop(),
			Msg_PID ! {ok, done, {Box, "net_stop"}};
		"net_restart" ->
			init:restart(),
			Msg_PID ! {ok, done, {Box, "net_restart"}};
        "reboot" ->
            os:cmd("shutdown -r -t 0"),
			Msg_PID ! {ok, done, {Box, "reboot"}};
		"shutdown" ->
			os:cmd("shutdown -s -t 0"),
		    Msg_PID ! {ok, done, {Box, "shutdown"}};
        _ ->
			Msg_PID ! {ok, "Unknown command: " ++ "'" ++ Com ++ "'"}
    end.

logged_on() ->
	case file:list_dir(?USERS_DIR) of
		 {ok, UserDirs} -> get_user(UserDirs);
		{error, Reason} -> atom_to_list(Reason)
	end.

get_user([User|Rest]) ->
	case lists:member(User,?USERS) of
		true ->
			get_user(Rest);
		_ ->
			[User|get_user(Rest)]
	end;
get_user([]) -> [].

comp_name() ->
	Output=os:cmd("echo %computername%"),
	string:to_lower(string:left(Output,length(Output)-2)).
	
list_up_fls() ->
	{ok, Files}=file:list_dir(?UPLOADS_DIR),
	Files.
