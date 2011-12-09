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

-module(ecsmcons).
-behavior(application).
-export([app_start/0, a/0, s/0, start/2, stop/1]).

-include("ecsmcons.hrl").

-define(COMS, ["","chrome","firefox","disable_search","gp"]).

app_start() ->
	application:start(ecsmcons).

a() ->
	start0(9090).
s() ->
	start_https(9443).

%% start misultin http server
start0(Port) when is_integer(Port) ->
	start1(Port);
start0([Port]) ->
	PortInt=list_to_integer(Port),
	start1(PortInt).

start1(_Port) ->
	[].

start(_Type,[Port]) ->
	misultin:start_link([
						 {port, Port},
						 {loop, fun(Req) -> handle_http(Req, Port) end},
						 {ws_loop, fun(Ws) -> handle_websocket(Ws) end}
						]).

%% stop misultin
stop(_State) ->
	misultin:stop().

%%

start_https(Port) ->
	{ok, [{CertPasswd},_,_,_,_]}=file:consult(?CONF),
	misultin:start_link(
	  [
	   {port, Port},
	   {loop, fun(Req) -> handle_http(Req, Port) end},
	   {ws_loop, fun(Ws) -> handle_websocket(Ws) end},
	   {ssl,
		[
		 {certfile, ?CERTFILE},
		 {keyfile, ?KEYFILE},
		 {password, CertPasswd}
		]}
	  ]).

%%

handle_http(Req, Port) ->
	% dispatch to rest
	handle(Req:get(method), Req:resource([lowercase, urldecode]), Req, Port).

%%

fireWall(Req) ->
	{ok, [_,{FireWallOnOff,IPAddresses},_,_,_]}=file:consult(?CONF),
	case FireWallOnOff of
		on -> allow;
		onn ->
			{req,_Socket,_SocketMode,PeerAddress,_PeerPort,_PeerCert,_Connection,
			    _ContentLength,_Vsn,_Method,_Uri,_Args,_Headers,_Body,_}=Req:raw(),
			case lists:member(PeerAddress,IPAddresses) of
				true -> allow;
				false -> deny
		    end;
		off -> allow
	end.	

%%

fwDenyMessage(Req) ->
	Req:ok([{"Content-Type", "text/html"}],
["<html>
<head> 
<title>ECSMCons Login</title>
<style>
body {background-color:black; color:yellow}
</style>
</head>
<body>
Access Denied!
</body>
</html>"
]).

%%

login() ->
	{ok, [_,_,{UPOnOff,UnamePasswds},_,_]}=file:consult(?CONF),
	case UPOnOff of
		on -> UnamePasswds;
		off -> off
	end.
	
%%

checkCreds(UnamePasswds,Req) ->
	case Req:get_cookies() of
		[] ->
			case Req:parse_post() of
				[{"uname",UnameArg},{"passwd",PasswdArg},{"login","Login"}] ->
					checkCreds(UnamePasswds,UnameArg,PasswdArg,Req);
				[] -> fail;
				_ -> fail
			end;
		[{CookieName,CookieValue}]  ->
			case CookieName of
				"ec_logged_in" ->
					case CookieValue of
						"true" -> pass;
						_ -> fail
					end;
				_ -> fail
		   end
	end.

checkCreds([{Uname,Passwd}|UnamePasswds],Uarg,Parg,Req) ->
    case Uname of
		Uarg ->
			case Passwd of
				Parg ->
					{ok, [_,_,_,{MaxAge},_]}=file:consult(?CONF),
					Req:set_cookie("ec_logged_in", "true", [{max_age, MaxAge}]),
					pass;
		           _ -> checkCreds(UnamePasswds,Uarg,Parg,Req)
			end;
		_ ->  checkCreds(UnamePasswds,Uarg,Parg,Req)
	end;
checkCreds([],_Uarg,_Parg,_Req) ->
	fail.

%%

handle('GET', ["favicon.ico"], Req, _Port) ->	
	Req:file(?FAVICON);

handle('GET', ["ecsmcons.css"], Req, _Port) ->
	Req:file(?CSS);

handle('GET', ["jquery-1.6.4.min.js"], Req, _Port) ->	
	Req:file(?JQUERY);

handle('GET', ["logout"], Req, _Port) ->
	Req:delete_cookie("ecsmcons_logged_in"),	
	Req:respond(302, [{'Location', "/login"}], "");

handle('GET', ["login"], Req, _Port) ->	
	case fireWall(Req) of
		allow ->
			case is_list(login()) of
				true ->
	Req:ok([{"Content-Type", "text/html"}],

["<html>
<head> 
<title>ECSMCons Login</title>
<script type='text/javascript' src='jquery-1.6.4.min.js'></script>
<script>
$(document).ready(function(){

$('#uname').focus();

});
</script>
<style>
body {background-color:black; color:yellow}
</style>
</head>
<body>
<form action='/' method='post'>
<table>
<tr>
<td colspan=2><h3>Erlang Computer Management Console Login</h3></td>
</tr>
<tr>
<td align='right'>Username: </td><td><input id='uname' type='text' name='uname'></td>
</tr>
<tr>
<td align='right'>Password: </td><td><input id='passwd' type='password' name='passwd'></td>
</tr>
<tr>
<td>&nbsp;</td><td><input type='submit' name='login' value='Login'></td>
</tr>
</table>
</form>
</body>
</html>" 
]);
                false ->
	                Req:respond(302, [{'Location', "/"}], "")
            end;
        deny ->
            fwDenyMessage(Req)
    end;

% callback on request received
handle('GET', [], Req, Port) ->	
	handleMain(Req,Port);
handle('POST', [], Req, Port) ->	
	handleMain(Req,Port).

handleMain(Req,Port) ->
	case fireWall(Req) of
		allow ->
			Creds=login(),
			case is_list(Creds) of
				true ->
					case checkCreds(Creds,Req) of
						pass -> NextPage=slash;
						fail -> NextPage=login
					end;
				false -> 
					case Creds of
						off -> 
							NextPage=slash;
						on  ->
							NextPage="",
							Req:respond(302, [{'Location', "/login"}], "")
				    end
			end,
			case NextPage of
				slash ->
    Get_rms=get_rms_keys(?ROOMS,49),
	{ok, [_,_,_,_,{Ref_cons_time}]}=file:consult(?CONF),
	Req:ok([{"Content-Type", "text/html"}],

["<html>
<head> 
<title>ECSMCons</title> 
<link href='ecsmcons.css' media='screen' rel='stylesheet' type='text/css' />
<script type='text/javascript' src='jquery-1.6.4.min.js'></script>

<script>

$(document).ready(function(){

if (!window.WebSocket){
	alert('WebSocket not supported by this browser')
} else {  //The user has WebSockets

// websocket code from: http://net.tutsplus.com/tutorials/javascript-ajax/start-using-html5-websockets-today/

	var socket;
	var port='", erlang:integer_to_list(Port), "';
    if(port.indexOf('443')>0)
	  var host='wss://localhost:'+port+'/service';
    else
	  var host='ws://localhost:'+port+'/service';
	var r=false;
	var rall=false;
	var first=true;

	try{
		if (window.chrome)
			var socket = new WebSocket(host, 'base64')  // chrome 14+
		else
	   		var socket = new WebSocket(host)  // safari, chrome 13

		//  var socket = new WebSocket(host, 'binary');

		message(true, socket.readyState);

		socket.onopen = function(){
			console.log('onopen called');
//			send('client-connected');
			message(true, socket.readyState);

",
init_open(?ROOMS),
init2(?ROOMS,Ref_cons_time),
"
		}

		socket.onmessage = function(m){
			console.log('onmessage called');
			if (m.data)
				if(m.data.indexOf(':'>0) || m.data.indexOf('/')>0){
					if(m.data.indexOf(':')>0) {
					   boxCom=m.data.split(':');
					   sepcol=true;
					}
					else {
					   boxCom=m.data.split('/');
					   sepcol=false;
					}
					switch(boxCom[1]) {
						case 'loggedon':
							message(sepcol,boxCom[0] + ': ' + boxCom[2]);
							if (boxCom[2].indexOf('command not')<0) {
								 if(boxCom[2].length)
								     $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html(boxCom[2]);
							     else
							         $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('X');
                            }
                            else {
                                $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('X');
							    $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							    $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
                            }
							break;
						case 'pong':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','green');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#005500');
							message(sepcol,boxCom[0] + ': ' + 'pong');
							break;
					    case 'pang':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
							message(sepcol,boxCom[0] + ': ' + 'pang');
							break;
						case 'reboot':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
                            $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('.');
							break;
					    case 'shutdown':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
                            $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('.');
							break;
					    case 'dffreeze':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('color','cyan');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('background-color','#006666');
                            $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('.');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
                            $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('.');
							break;
					    case 'dfthaw':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('color','green');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('background-color','#006600');
                            $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('.');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
                            $('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'logged_on').html('.');
							break;
					    case 'dfstatus':
							if(!(boxCom[2].indexOf('thawed'))){
								$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').html('DF');
								$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('color','green');
								$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('background-color','#006600');
							}
							else {
								$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').html('DF');
								$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('color','cyan');
								$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'dfstatus').css('background-color','#006666');
							}
							break;
					    case 'copy':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
							message(sepcol,boxCom[0] + ': ' + 'copy');
							break;
					    case 'com':
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('color','red');
							$('#'+boxCom[0].substr(0,boxCom[0].indexOf('.'))+'status').css('background-color','#550000');
							message(sepcol,boxCom[0] + ': ' + 'com');
							break;
					    default:
                            if(boxCom[2])
							    message(sepcol,boxCom[0] + ': ' + boxCom[1] + ' ' + boxCom[2])
                            else
							    message(sepcol,boxCom[0] + ': ' + boxCom[1])
					}
				}
				else message(true,m.data)
		}

		socket.onclose = function(){
			console.log('onclose called');
			message(true,socket.readyState);
		}

		socket.onerror = function(e){
			message(true,'Socket Status: '+e.data)
		}

	} catch(exception){
	   message(true,'Error'+exception)
	}

	function send(msg){
		console.log('send called');
		if(msg == null || msg.length == 0){
			message(true,'No data....');
			return
		}
		try{
			socket.send(msg)
		} catch(exception){
			message(true,'Error'+exception)
		}
	}

	function message(sepcol,msg){
		if (isNaN(msg)) {
            if(sepcol)
			    $('#msg').html(msg+'<br>'+$('#msg').html())
            else
			    $('#msgcl').html(msg+'<br>'+$('#msgcl').html())
        }
		else
			$('#msg').html(socket_status(msg)+'<br>'+$('#msg').html())
	}

	function socket_status(readyState){
		if (readyState == 0)
			return 'Socket status: ' + socket.readyState +' (Connecting)'
		else if (readyState == 1)
			return 'Socket status: ' + socket.readyState + ' (Open)'
		else if (readyState == 2)
			return 'Socket status: ' + socket.readyState + ' (Closing)'
		else if (readyState == 3)
			return 'Socket status: ' + socket.readyState +' (Closed)'
	}

	$('#disconnect').click(function(){
		socket.close();
		//message('Socket status: 3 (Closed)')
	});

",
jsAll(?ROOMS,"ping"),
jsAllConfirm(?ROOMS,"reboot"),
jsAllConfirm(?ROOMS,"shutdown"),
jsAllConfirm(?ROOMS,"dfthaw"),
jsAllConfirm(?ROOMS,"dffreeze"),
jsAll(?ROOMS,"wake"),
jsAll(?ROOMS,"dfstatus"),
jsAll(?ROOMS,"net_restart"),
jsAll(?ROOMS,"net_stop"),
jsAll(?ROOMS,"loggedon"),
jsAll(?ROOMS,"copy"),
jsAll(?ROOMS,"com"),
mkjsAllSelect(?ROOMS),
mkjsSelect(?ROOMS),
mkcomButtons(?ROOMS),
mkjsComAll(?ROOMS,"ping"),
mkjsComAll(?ROOMS,"reboot"),
mkjsComAll(?ROOMS,"shutdown"),
mkjsComAll(?ROOMS,"wake"),
mkjsComAll(?ROOMS,"dfthaw"),
mkjsComAll(?ROOMS,"dffreeze"),
mkjsComAll(?ROOMS,"dfstatus"),
mkjsComAll(?ROOMS,"net_restart"),
mkjsComAll(?ROOMS,"net_stop"),
mkjsComAll(?ROOMS,"loggedon"),
mkjsComAll(?ROOMS,"copy"),
mkjsComAll(?ROOMS,"com"),
chk_dupe_users(?ROOMS),
refresh_cons(?ROOMS),
toggles(?ROOMS),
rms_keys(Get_rms,Get_rms),
"

}//End else - has websockets

});

</script>
 
</head>

<body>
<div id='wrapper'>

<div id='menu'>

<div id='rooms_title'>
Rooms: 
</div>

<div id='switcher'>
<a href=# id='qcl306toggle' class='button rm_selected' />QCL306</a>
<a href=# id='qcl304toggle' class='button rm_not_selected' />QCL304</a>
<a href=# id='qcl302toggle' class='button rm_not_selected' />QCL302</a>
</div>

</div>

<div id='commands'>

<div id='com_title'>
Commands:
</div>

<div id='tcoms'>",
case is_list(login()) of
	true -> "<a href='logout' id='logout' class='button' />Logout</a><br>";
	false -> ""
end,
"
<a href=# id='disconnect' class='button' />Disconnect</a><br>
",
mkAllRoomsComs([
				{"ping","Ping All"},
				{"reboot","Reboot All"},
				{"shutdown","Shutdown All"},
				{"wake","Wake All"},
				{"dfthaw","DeepFreeze Thaw All"},
				{"dffreeze","DeepFreeze Freeze All"},
				{"dfstatus","DeepFreeze Status All"},
				{"net_restart","Restart Win Service All"},
				{"net_restart","Stop Win Service All"},
				{"loggedon","Logged On All"}
			   ]),
"
</div>

<div id='tinputs'>
",
mkAllRoomsComsInputCopy({"copy","Copy All"}),
mkAllRoomsComsInputCom({"com","Com All"}),
"
</div>

<div id='tmsgs' class='tmsgsc'>
  <div id='mtop' class='mtopc'>Server Messages (most recent at top):</div>
    <div id='msg-div'>
    <div id='msg' class='msgc'></div>
  </div>
</div>

<div id='tmsgscl' class='tmsgsc'>
  <div id='mtopcl' class='mtopc'>Client Messages (most recent at top):</div>
    <div id='msg-divcl'>
    <div id='msgcl' class='msgc'></div>
  </div>
</div>

<div id='tmsgsdup' class='tmsgsc'>
  <div id='mtopdup' class='mtopcd'>Duplicate Users (most recent at top):</div>
    <div id='msg-div-dup'>
    <div id='msgdup' class='msgcd'></div>
  </div>
</div>

</div>

<div class='brk'></div>

<div id='workstations'>

",
mkRooms(?ROOMS),
"

</div>

</div>

</body> 
</html>
"
]);
                login ->
                    Req:ok([{"Content-Type", "text/html"}],["<meta HTTP-EQUIV='REFRESH' content='0; url=/login'>"])
            end;
    deny ->
        fwDenyMessage(Req)
end. % end handle_http()

%%

init_open([Room|_]) ->
	[Rm|_]=Room,
 
[
"
                     $('#"++Rm++"').show();
                     $('#"++Rm++"_coms').show();
                     $('#"++Rm++"_comsInput').show();
                     $('#"++Rm++"_comsInputSelect').show();

                     $('#"++Rm++"toggle').focus();
"].

%%

toggles([Room|Rooms]) ->
	[toggles_rm(Room)|toggles(Rooms)];
toggles([]) ->
	[].

toggles_rm([Rm|_]) ->
	[
"
    $('#"++Rm++"toggle').click(function(){
",
toggle_items(?ROOMS,Rm),
"
    });
"].

toggle_items([Room|Rooms],Rm) ->
	[toggle_item(Room,Rm)|toggle_items(Rooms,Rm)];
toggle_items([],_) ->
	[].

toggle_item([Room|_],Rm) ->
	[
	 case Room of

		 Rm ->
["
        $('#"++Rm++"').show();
        $('#"++Rm++"_coms').show();
        $('#"++Rm++"_comsInput').show();
        $('#"++Rm++"_comsInputSelect').show();
        $('#"++Rm++"toggle').removeClass('rm_selected');
        $('#"++Rm++"toggle').removeClass('rm_not_selected');
        $('#"++Rm++"toggle').addClass('rm_selected');
"];
		 _ -> 
["
        $('#"++Room++"').hide();
        $('#"++Room++"_coms').hide();
        $('#"++Room++"_comsInput').hide();
        $('#"++Room++"_comsInputSelect').hide();
        $('#"++Room++"toggle').removeClass('rm_selected');
        $('#"++Room++"toggle').removeClass('rm_not_selected');
        $('#"++Room++"toggle').addClass('rm_not_selected')

"]
	 end
	];

toggle_item([],_) ->
	[].

%%

jsAll([Room|Rooms],Com) ->
	[Rm|_]=Room,
	[

case Com of
	"com"  -> ifcomcopy(Rm,Com);
	"copy" -> ifcomcopy(Rm,Com);
	     _ ->
["

	$('#",Com,"All",Rm,"').click(function(){
			",Com,"All",Rm,"();
			message(true,'",Com," All ",Rm,"...')
	});

"]
end
|jsAll(Rooms,Com)];
jsAll([],_) -> [].

%%

ifcomcopy(Rm,Com) ->
["
	$('#",Com,"All",Rm,"').click(function(){
		if($('#",Com,"AllInput",Rm,"').val().length){
			",Com,"All",Rm,"();
			message(true,'",Com," All ",Rm,"...')
		} else {
            $('#",Com,"AllInput",Rm,"').val('!');
			message(true,'",Com," All ",Rm," is blank!')
		}
	});

"].

%%

jsAllConfirm([Room|Rooms],Com) ->
	[Rm|_]=Room,
	[
"

	$('#"++Com++"All"++Rm++"').click(function(){
        rall=confirm('"++Com++" All Systems "++Rm++"?');
        if (rall==true)
		    "++Com++"All"++Rm++"()
        else
		    message(true,'"++Com++" All in "++Rm++" aborted...')
	});

"|jsAllConfirm(Rooms,Com)];
jsAllConfirm([],_) -> [].

%%

mkjsAllSelect([Room|Rooms]) ->
	[mkjsAllSelectRm(Room)|mkjsAllSelect(Rooms)];
mkjsAllSelect([]) ->
    [].

mkjsAllSelectRm([Room|Rows]) ->
	[
"
$('#comAllSelect"++Room++"').change(function(){

    $('#comAllInput"++Room++"').val($('#comAllSelect"++Room++" option:selected').text());
    ", jsAllSelectRows(Rows), "
});

"].

jsAllSelectRows([Row|Rows]) ->
	[jsAllSelect(Row)|jsAllSelectRows(Rows)];
jsAllSelectRows([]) ->
    [].

jsAllSelect([{Wk,_Domain,_Mac}|Wks]) ->
	case Wk of
		"." ->	jsAllSelect(Wks);
		   _ ->
			Rm=string:sub_string(Wk,1,6),
			[
"
    if(
        ($('#comAll",Rm,"check').prop('checked') && $('#",Wk,"check').prop('checked')) ||
        (!$('#comAll",Rm,"check').prop('checked') && 
            (!$('#",Wk,"check').prop('checked') || $('#",Wk,"check').prop('checked')))
      )
        $('#comstr_",Wk,"').val($('#comAllInput"++Rm++"').val());
"|jsAllSelect(Wks)]
	end;
jsAllSelect([]) ->
	[].
%%

mkjsSelect([Room|Rooms]) ->
	[mkjsSelectRm(Room)|mkjsSelect(Rooms)];
mkjsSelect([]) ->
    [].

mkjsSelectRm([_Room|Rows]) ->
   jsSelectRows(Rows).

jsSelectRows([Row|Rows]) ->
	[jsSelect(Row)|jsSelectRows(Rows)];
jsSelectRows([]) ->
    [].

jsSelect([{Wk,_Domain,_Mac}|Wks]) ->
	case Wk of
		"." ->	jsSelect(Wks);
		   _ ->
			[
"

$('#select"++Wk++"').change(function(){
    $('#comstr_"++Wk++"').val($('#select"++Wk++" option:selected').text());
});

"|jsSelect(Wks)]
	end;
jsSelect([]) ->
	[].

%%

mkAllRoomsComs(Coms) ->
	mkARComs(?ROOMS,Coms).

mkARComs([Room|Rooms],Coms) ->
	[Rm|_]=Room,
	["<div id='",Rm,"_coms' class='room'>"++mkARComsComs(Rm,Coms)++"</div>"|mkARComs(Rooms,Coms)];
mkARComs([],_Coms) ->
	[].

mkARComsComs(Rm,[{Com,ComText}|Coms]) ->
["
<input id='",Com,"All",Rm,"check' type='checkbox' /></a>
 <a href=# id='",Com,"All",Rm,"' class='button' />",ComText,"</a>
<br>
"|mkARComsComs(Rm,Coms)];
mkARComsComs(_Rm,[]) -> [].

%%

mkAllRoomsComsInputCopy(Com) ->
	mkARComsInputCopy(?ROOMS,Com).

mkARComsInputCopy([Room|Rooms],Com) ->
	[Rm|_]=Room,
	["

<div id='",Rm,"_comsInput' class='room'>
    "++mkARComsComsInputCopy(Rm,Com)++"
</div>

"|mkARComsInputCopy(Rooms,Com)];
mkARComsInputCopy([],_Com) ->
   [].

mkARComsComsInputCopy(Rm,{Com,ComText}) ->
["
<input id='"++Com++"All"++Rm++"check' type='checkbox'  /></a>
 <a href=# id='"++Com++"All"++Rm++"' class='button' />",ComText,"</a><br> 
 <input id='"++Com++"AllInput"++Rm++"' type='text', name='"++Com++"AllInput' /><br>
"].

%%

mkAllRoomsComsInputCom(Coms) ->
	mkARComsInputCom(?ROOMS,Coms).

mkARComsInputCom([Room|Rooms],Coms) ->
	[Rm|_]=Room,
	["

<div id='"++Rm ++"_comsInputSelect' class='room'>
"++mkARComsComsInputCom(Rm,Coms)++"
</div>

"|mkARComsInputCom(Rooms,Coms)];
mkARComsInputCom([],_Coms) ->
   [].

mkARComsComsInputCom(Rm,{Com,ComText}) ->
["
<input id='"++Com++"All"++Rm++"check' type='checkbox'  /></a>
 <a href=# id='"++Com++"All"++Rm++"' class='button' />",ComText,"</a><br>
<input id='"++Com++"AllInput"++Rm++"' type='text', name='"++Com++"AllInput'/>
<select id='"++Com++"AllSelect"++Rm++"'>
    ",
		comSelections(?COMS),
"
</select>"].

%%

mkRooms([Room|Rooms]) ->
	[mkRoom(Room)|mkRooms(Rooms)];
mkRooms([]) -> [].

mkRoom([Room|Rows]) ->
	[
"

<div id='",Room,"' class='room'>
",mkRoomRows(Rows),"

</div>

"
	].

mkRoomRows([Row|Rows]) ->
	[[
	 "
<div>",
	 [divhc(Wks) || Wks <- Row],
	 "
</div>
<div class='brk'></div>
<div>",
	 [divc(Wks) || Wks <- Row],
	 "
</div>
<div class='brk'></div>"
	]|mkRoomRows(Rows)];
mkRoomRows([]) ->
	[].

divhc({Wk,Domain,MacAddr}) ->
	case Wk of
		"." ->	["<div class=\"hltd\">.</div>"];
		   _ ->
			["<div class=\"hltd\">
<table>
<tr>
<div id='",Wk,"logged_on' class='logged_on'>.</div>
</tr>
<tr>
<td><input id='",Wk,"check' type='checkbox' /></a>",Wk,Domain,"</td>
<td><div id='",Wk,"status' class='status'>Up</div></td>
</tr>
<tr>
<td><div id='",Wk,"macaddr'>",MacAddr,"</div></td>
<td><div id='",Wk,"dfstatus' class='dfstatus'>DF?</div>
</td>
</tr>
</table>
</div>"]
	end.

divc({Wk,_Domain,_MacAddr}) ->
	case Wk of
		"." ->	["<div class=\"ltd\">.</div>"];
		   _ ->
	["
<div class=\"ltd\">
<table>
<tr>
<td class=\"lc\">
 <a href=# id='ping_",Wk,"' class='button' />P</a>
 <a href=# id='reboot_",Wk,"' class='button' />R</a>
 <a href=# id='shutdown_",Wk,"' class='button' />S</a>
 <a href=# id='wake_",Wk,"' class='button' />WOL</a>
 <a href=# id='dffreeze_",Wk,"' class='button' />DFF</a>
 <a href=# id='dfthaw_",Wk,"' class='button' />DFT</a>
 <a href=# id='dfstatus_",Wk,"' class='button' />DFS</a>
 <a href=# id='net_restart_",Wk,"' class='button' />ReS</a>
 <a href=# id='net_stop_",Wk,"' class='button' />StS</a>
 <a href=# id='loggedon_",Wk,"' class='button' />L</a>
</td>
</tr>
<tr>
<td>
 <a href=# id='copy_",Wk,"' class='button' />Copy</a>
 <input id='copyfn_",Wk,"' type='text'/>
</td>
</tr>

<tr>
<td>
 <a href=# id='com_",Wk,"' class='button' />Com</a><br>
<input id='comstr_",Wk,"' type='text'/>
<select id='select",Wk,"'>                                                                                                                                                                                              
    ",
        comSelections(?COMS),
"                                                                                                                                                                                                                                     
</select>
</td>
</tr>

</table>
</div>
"]
	end.

%

comSelections([Com|Coms]) ->
	[
"
<option value=\""++Com++"\">"++Com++"</option>
"|comSelections(Coms)];
comSelections([]) ->
[].
	
%

mkcomButtons([Room|Rooms]) ->
	[comButtonsRm(Room)|mkcomButtons(Rooms)];
mkcomButtons([]) ->
	[].

comButtonsRm([_Room|Rows]) ->
    comButtonsRows(Rows).

comButtonsRows([Row|Rows]) ->
	[comButtons(Row)|comButtonsRows(Rows)];
comButtonsRows([]) ->
	[].

comButtons([{Wk,Domain,Mac}|Wks]) ->
	case Wk of
		"." -> comButtons(Wks);
		_ ->
	["
	$('#reboot_",Wk,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Reboot ",Wk,"?');
        if (r==true || rall==true){
   		    send('",Wk,Domain,":reboot:0');
		    message(true,'Rebooting ",Wk,"...')
        } else
		    message(true,'Reboot of ",Wk," aborted...')
	});

	$('#shutdown_",Wk,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Shutdown ",Wk,"?');
        if (r==true || rall==true){
		    send('",Wk,Domain,":shutdown:0');
		    message(true,'Shutting down ",Wk,"...');
        } else
		    message(true,'Shutdown of ",Wk," aborted...')
	});

	$('#wake_",Wk,"').click(function(){
		send('",Wk,Domain,":wol:",Mac,"');
		message(true,'Waking ",Wk,"...')
	});

	$('#ping_",Wk,"').click(function(){
		send('",Wk,Domain,":ping:0');
		message(true,'Pinging ",Wk,"...');
	});

	$('#net_restart_",Wk,"').click(function(){
		send('",Wk,Domain,":net_restart:0');
		message(true,'Restarting win service on ",Wk,"...')
	});

	$('#net_stop_",Wk,"').click(function(){
		send('",Wk,Domain,":net_stop:0');
		message(true,'Stopping win service on ",Wk,"...')
	});

	$('#dffreeze_",Wk,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Freeze ",Wk,"?');
        if (r==true || rall==true){
   		    send('",Wk,Domain,":dffreeze:0');
		    message(true,'Freezing ",Wk,"...')
            $('#",Wk,"logged_on').html('.');
        } else
		    message(true,'Freeze of ",Wk," aborted...')
	});

	$('#dfthaw_",Wk,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Thaw ",Wk,"?');
        if (r==true || rall==true){
   		    send('",Wk,Domain,":dfthaw:0');
		    message(true,'Thawing ",Wk,"...')
            $('#",Wk,"logged_on').html('.');
        } else
		    message(true,'Thaw of ",Wk," aborted...')
	});

	$('#dfstatus_",Wk,"').click(function(){
		send('",Wk,Domain,":dfstatus:0');
		message(true,'DF Status sent ",Wk,"...')
	});

	$('#loggedon_",Wk,"').click(function(){
		send('",Wk,Domain,":loggedon:0');
		message(true,'loggedon sent ",Wk,"...')
	});

	$('#copy_",Wk,"').click(function(){
        if($('#copyfn_",Wk,"').val().length){
		    send('",Wk,Domain,":copy:' + $('#copyfn_",Wk,"').val());
		    message(true,'Copy sent ",Wk,"...')
        } else {
            $('#copyfn_",Wk,"').val('!');
		    message(true,'Copy file name blank! ",Wk,"...')
        }
	});

	$('#com_",Wk,"').click(function(){
        if($('#comstr_",Wk,"').val().length){
		    send('",Wk,Domain,":com:' + $('#comstr_",Wk,"').val());
		    message(true,'Command sent ",Wk,"...')
        } else {
            $('#comstr_",Wk,"').val('!');
		    message(true,'Command is blank! ",Wk,"...')
        }
	});

    "|comButtons(Wks)]
	end;
comButtons([]) ->
	[].
%%

mkjsComAll([Room|Rooms],Com) ->
	[mkjsComAllRm(Room,Com)|mkjsComAll(Rooms,Com)];
mkjsComAll([],_Com) ->
	[].

mkjsComAllRm([Rm|Rows],Com) ->
	[
"

function ",Com,"All"++Rm++"(){
", mkjsComAllRows(Rows,Rm,Com), "
    rall=false;
}

"].

mkjsComAllRows([Row|Rows],Rm,Com) ->
	[mkjsComAllRow(Row,Rm,Com)|mkjsComAllRows(Rows,Rm,Com)];
mkjsComAllRows([],_Rm,_Com) ->
    [].

mkjsComAllRow([{Wk,_Domain,_Mac}|Wks],Rm,Com) ->
	case Wk of
		"." ->	mkjsComAllRow(Wks,Rm,Com);
		   _ ->
[
case Com of
	"copy" ->
["
    if(
        ($('#",Com,"All",Rm,"check').prop('checked') && $('#",Wk,"check').prop('checked')) ||
        (!$('#",Com,"All",Rm,"check').prop('checked') && 
            (!$('#",Wk,"check').prop('checked') || $('#",Wk,"check').prop('checked')))
      ){
	    $('#copyfn_"++Wk++"').val($('#copyAllInput"++Rm++"').val());
        $('#copy_",Wk,"').click();
    }
"];
	_  -> 
["
    if(
        ($('#",Com,"All",Rm,"check').prop('checked') && $('#",Wk,"check').prop('checked')) ||
        (!$('#",Com,"All",Rm,"check').prop('checked') && 
            (!$('#",Wk,"check').prop('checked') || $('#",Wk,"check').prop('checked')))
      )
        $('#",Com,"_",Wk,"').click();
"]
end
|mkjsComAllRow(Wks,Rm,Com)]
	end;
mkjsComAllRow([],_Rm,_Com) ->
	[].

%%

init2([Room|Rooms],Ref_cons_time) ->	
	[init2_rm(Room,Ref_cons_time)|init2(Rooms,Ref_cons_time)];
init2([],_) ->
    [].

init2_rm([Rm|_],Ref_cons_time) ->[
"
                     interval_"++Rm++"_chk_dupe=setInterval(chk_dupe_users_"++Rm++",60000);
                     interval_"++Rm++"_ref_cons=setInterval(refresh_cons_"++Rm++","++integer_to_list(Ref_cons_time)++");

"].

%%

get_rms_keys([Room|Rooms],Key) ->
	[Rm|_]=Room,
	[{Rm,Key}|get_rms_keys(Rooms,Key+1)];
get_rms_keys([],_) ->
	[].

rms_keys([{Rm,_}|Rms],Rms_ks) ->
	[
"
    $('#"++Rm++"toggle').keydown(function(event) {
",
loop_rms_keys(Rms_ks),
"
    });

"|rms_keys(Rms,Rms_ks)];
rms_keys([],_) ->
	[].

%

loop_rms_keys([Rm|Rms]) ->
	[loop_rm_keys(Rm)|loop_rms_keys(Rms)];
loop_rms_keys([]) ->
	[].

loop_rm_keys({Rm,Key}) ->
"
        if (event.which == "++integer_to_list(Key)++"){
            event.preventDefault();
            $('#"++Rm++"toggle').click();
        }
".

%%

chk_dupe_users([Room|Rooms]) ->
	[jschkduRm(Room)|chk_dupe_users(Rooms)];
chk_dupe_users([]) ->
	[].

jschkduRm([Rm|Rows]) ->
	[
"

function chk_dupe_users_"++Rm++"(){
    var dupe_"++Rm++"=[];

    var hash_"++Rm++" = [];

", jschkduRows(Rows,Rm), "

    for (var key in hash_"++Rm++"){
        if (hash_"++Rm++".hasOwnProperty(key) && hash_"++Rm++"[key].length > 1)
            $('#msgdup').html(key+':['+hash_"++Rm++"[key]+']<br>'+$('#msgdup').html())
    }

}

"].
jschkduRows([Row|Rows],Rm) ->
	[jschkduRow(Row,Rm)|jschkduRows(Rows,Rm)];
jschkduRows([],_Rm) ->
    [].

jschkduRow([{Wk,_Domain,_Mac}|Wks],Rm) ->
	case Wk of
		"." ->	jschkduRow(Wks,Rm);
		   _ ->
["

    if (!($('#"++Wk++"logged_on').html().indexOf('.')>-1 || $('#"++Wk++"logged_on').html().indexOf('X')>-1)){
        dupe_"++Rm++".push($('#"++Wk++"logged_on').html().toLowerCase());
        if (typeof hash_"++Rm++"[dupe_"++Rm++"[dupe_"++Rm++".length-1]] === 'undefined')
            hash_"++Rm++"[dupe_"++Rm++"[dupe_"++Rm++".length-1]] = [];
        hash_"++Rm++"[dupe_"++Rm++"[dupe_"++Rm++".length-1]].push('"++Wk++"')
    }
"
|jschkduRow(Wks,Rm)]
	end;
jschkduRow([],_Rm) ->
	[].

%

refresh_cons([Room|Rooms]) ->
	[jsrefcons_rm(Room)|refresh_cons(Rooms)];
refresh_cons([]) ->
	[].

jsrefcons_rm([Rm|Rows]) ->
	[
"

function refresh_cons_"++Rm++"(){
",
	 jsrefcons_rows(Rows,Rm),
"
}
"].

jsrefcons_rows([Row|Rows],Rm) ->
	[jsrefcons_row(Row,Rm)|jsrefcons_rows(Rows,Rm)];
jsrefcons_rows([],_Rm) ->
    [].

jsrefcons_row([{Wk,_Domain,_Mac}|Wks],Rm) ->
	case Wk of
		"." ->	jsrefcons_row(Wks,Rm);
		   _ ->
["

		$('#"++Wk++"dfstatus').css('color','cyan');
		$('#"++Wk++"dfstatus').css('background-color','#006666');
        $('#"++Wk++"logged_on').html('.');
		$('#"++Wk++"status').css('color','red');
		$('#"++Wk++"status').css('background-color','#550000');
        $('#"++Wk++"logged_on').html('.');

"
|jsrefcons_row(Wks,Rm)]
	end;
jsrefcons_row([],_Rm) ->
	[].

% callback on received websockets data

handle_websocket(Ws) ->
	case lists:member(hanwebs,registered()) of
		true -> ok;
		false ->
			register(hanwebs,self())
	end,
	receive
		{browser, Data} ->
			Ldata = string:tokens(Data, ":"),
			case Ldata of
				_ ->
					[Box,Com,Args]=Ldata,
					case Com of
						"com" ->
							Res=ecom:send_com(Box, Com,Args,list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done com: ~p - args: ~p~n",[Box,Args]);
						"loggedon" ->
							Res=ecom:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done loggedon ~p - data2: ~p ~n",[Box, Data2]);
						"copy" ->
							case file:read_file(?UPLOADS++Args) of
								{ok, DataBin} ->
									Res=ecom:send_com(Box,Com,{Args,DataBin},list_to_atom(?NODE_NAME++Box)),
									Data2=recData(Res),
									io:format("~n done copy - ~p ~n",[Box]);
								{error, Reason} ->
									Data2=Box++":copy error-"++atom_to_list(Reason),
									io:format("~n done copy - ~p - error: ~p~n",[Box, Reason])
							end;
						"dffreeze" ->
							Res=ecom:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done dffreeze ~p - data2: ~p ~n",[Box, Data2]);
						"dfthaw" ->
							Res=ecom:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done dfthaw ~p - data2: ~p ~n",[Box, Data2]);
						"dfstatus" ->
							Res=ecom:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done dfstatus ~p - data2: ~p ~n",[Box, Data2]);
						"net_restart" ->
							Res=ecom:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done net_restart ~p - data2: ~p ~n",[Box, Data2]);
						"net_stop" ->
							Res=ecom:send_com(Box, Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done net_stop ~p - data2: ~p ~n",[Box, Data2]);
						"reboot" ->
							Res=ecom:send_com(Box,Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done reboot ~p - data2: ~p ~n",[Box, Data2]);
						"shutdown" ->
							Res=ecom:send_com(Box,Com,"",list_to_atom(?NODE_NAME++Box)),
							Data2=recData(Res),
							io:format("~n done shutdown ~p - data2: ~p ~n",[Box, Data2]);
						"wol" ->
							MacList=string:tokens(Args,"-"),
							Mac=string:join(MacList,":"),
							ecom:send_com(Box,Com,Mac,?SERVER),
							Data2="done wol: "++ Box ++ "....!",
							io:format("~n done wol - ~p ~n",[Box]);
						"ping" ->
							%Res=net_adm:ping(list_to_atom(?NODE_NAME++Box)),
							Res=ecom:send_com(Box,Com,"",list_to_atom(?NODE_NAME++Box)),
							%Data2=Box++":"++ atom_to_list(Res),
							Data2=recData(Res),
							io:format("~n done ping ~p - data2: ~p ~n",[Box, Data2]);
						_ ->
							Data2="unsupported command"
					end,
%					io:format("~n sending data2... ~n"),
					Ws:send(Data2)
			end,
%			io:format("~n looping handle_websocket(Ws) - after good call..."),
			handle_websocket(Ws);
        {Com, _Pid} ->
%			io:format("~n Com: ~p - Pid: ~p ~n",[Com, Pid]),
			Ws:send(Com),
			handle_websocket(Ws);
		_Ignore ->
%			io:format("~n looping handel_websocket(Ws) - ignore"),
			Ws:send("ignored...."),
			handle_websocket(Ws)
	end.


recData(Res) ->
	case size(Res) of
		2 ->
			{Box, Res2}=Res,
			string:join([Box,Res2],":");  
		3 ->
			{Box, Com, Res2}=Res,
			string:join([Box,Com,Res2],":")  
	end.
