%% Copyright (c) 2012, Wes James <comptekki@gmail.com>
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

-module(ecsmcons_callback).

%% Export for websocket callbacks
-export([handle_message/1]).

-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-include("ecsmcons.hrl").

handle_message({text, <<"client-connected">>}) ->
    case lists:member(ywebs,registered()) of
        true -> ok;
        false ->
            register(ywebs,self())
    end,
    io:format("basic echo handler got ~p pid: ~p~n", ["client-connected",self()]),
    {reply, {text, <<<<"client-connected">>/binary>>}};

handle_message({text, <<"close">>}) ->
    io:format("basic echo handler got ~p~n", ["close"]),
    {close, normal};

handle_message({text, Message}) ->
    io:format("basic echo handler got ~p~n", [Message]),
	pd(binary_to_list(Message));

handle_message({binary, Message}) ->
    {reply, {binary, Message}};

handle_message({close, _Status, _Reason}) ->
    {close, normal}.

pd(Data) ->
			Ldata = string:tokens(Data, ":"),
			case Ldata of
				_ ->
					[Box,Com,Args]=Ldata,
					case Com of
						"com" ->
							Res=ecomsrv:send_com(Box, Com,Args,list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done com: ~p - args: ~p~n",[Box,Args]);
						"loggedon" ->
							Res=ecomsrv:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done loggedon ~p - data2: ~p ~n",[Box, Data2]);
						"copy" ->
							case file:read_file(?UPLOADS++Args) of
								{ok, DataBin} ->
									Res=ecomsrv:send_com(Box,Com,{Args,DataBin},list_to_atom(?NODE_NAME++Box)),
									Data2=recData(Res),
									io:format("~n done copy - ~p ~n",[Box]);
								{error, Reason} ->
									Data2=Box++":copy error-"++atom_to_list(Reason),
									io:format("~n done copy - ~p - error: ~p~n",[Box, Reason])
							end;
						"dffreeze" ->
							Res=ecomsrv:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done dffreeze ~p - data2: ~p ~n",[Box, Data2]);
						"dfthaw" ->
							Res=ecomsrv:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done dfthaw ~p - data2: ~p ~n",[Box, Data2]);
						"dfstatus" ->
							Res=ecomsrv:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done dfstatus ~p - data2: ~p ~n",[Box, Data2]);
						"net_restart" ->
							Res=ecomsrv:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done net_restart ~p - data2: ~p ~n",[Box, Data2]);
						"net_stop" ->
							Res=ecomsrv:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done net_stop ~p - data2: ~p ~n",[Box, Data2]);
						"reboot" ->
							Res=ecomsrv:send_com(Box,Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done reboot ~p - data2: ~p ~n",[Box, Data2]);
						"shutdown" ->
							Res=ecomsrv:send_com(Box,Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done shutdown ~p - data2: ~p ~n",[Box, Data2]);
						"wol" ->
							MacAddrList=string:tokens(Args,"-"),
							MacAddr=string:join(MacAddrList,":"),
							io:format("~n in wol .... args: ~p ~n",[MacAddr]),
							MacAddrBin= <<<<(list_to_integer(X, 16))>> || X <- string:tokens(MacAddr, ":")>>,
							io:format("~n macbin: ~p ~n",[MacAddrBin]),
							MagicPacket= << (dup(<<16#FF>>, 6))/binary, (dup(MacAddrBin, 16))/binary >>,
							{ok,S} = gen_udp:open(0, [{broadcast, true}]),
							gen_udp:send(S, ?BROADCAST_ADDR, 9, MagicPacket),
							gen_udp:close(S),
							Data2=list_to_binary("done wol: "++ Box ++ "....!"),
							io:format("~n done wol - ~p ~n",[Box]);
						"ping" ->
							Res=ecomsrv:send_com(Box,Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done ping ~p - data2: ~p ~n",[Box, Data2]);
						_ ->
							Data2="unsupported command"
					end
			  end,
    {reply, {text, <<Data2/binary>>}}.

recData(Res) ->
	case size(Res) of
		2 ->
			{Box, Res2}=Res,
			list_to_binary(string:join([Box,Res2],":"));
		3 ->
			{Box, Com, Res2}=Res,
			list_to_binary(string:join([Box,Com,Res2],":"))
	end.

dup(B,Acc) when Acc > 1 ->	
    B2=dup(B, Acc-1),
	<< B/binary,  B2/binary >>;
dup(B,1) ->
    B.
