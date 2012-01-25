{application, ecsmcons,
 [{description, "Erlang Computer Systems Management CONSole"},
  {vsn, "1.5"},
  {modules, [
  			ecsmcons,
  			misultin,
			misultin_acceptor,
			misultin_acceptors_sup,
			misultin_http,
			misultin_req,
			misultin_server,
			misultin_socket,
			misultin_utility,
			misultin_websocket,
			misultin_ws
  			]},
  {registered, [ecsmcons]},
  {applications, [kernel, stdlib]},
  {mod, {ecsmcons, [9090]}}
 ]}.