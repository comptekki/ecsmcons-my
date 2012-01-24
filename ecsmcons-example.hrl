-define(BROADCAST_ADDR, {192,168,0,255}).
-define(SERVER, 'ecom@host').
-define(NODE_NAME, "ecom@").
-define(ROOMS,
[
 ["room1-",
   [{".","",""},{".","",""},{"room1-01",".domain","00-00-00-00-00-00"}],
   [{"room1-02",".domain","00-00-00-00-00-00"},{"room1-03",".domain","00-00-00-00-00-00"},{"room1-04",".domain","00-00-00-00-00-00"}]
 ],
 ["room2-",
   [{".","",""},{"room2-01",".domain","00-00-00-00-00-00"},{"room2-02",".domain","00-00-00-00-00-00"}]
   [{".","",""},{".","",""},{".","",""},{".","",""},{".","",""},{".","",""}],
   [{"room2-03",".domain","00-00-00-00-00-00"}],
   [{"room2-04",".domain","00-00-00-00-00-00"},{".","",""},{"room2-05",".domain","00-00-00-00-00-00"}]
 ],
]).
-define(FAVICON,"/path/to/ecsmcons/favicon.ico").
-define(CERTFILE,"/path/to/cert.pem").
-define(KEYFILE,"/path/to/key.pem").
-define(CONF,"/path/to/ecsmcons/ecsmcons.conf").
-define(UPLOADS,"/path/to/ecsmcons/uploads/").
-define(STATIC,"/path/to/ecsmcons/static").
