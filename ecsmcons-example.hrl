-define(BROADCAST_ADDR, {192,168,0,255}).
-define(SERVER, 'node@host').
-define(NODE_NAME, "node@").
-define(ROOMS,
[
 ["room1",
   [{".","","",""},{".","","",""},{"room1-01","fqdn","00-00-00-00-00-00",""}],
   [{"room1-02","fqdn","00-00-00-00-00-00"},{"room1-03","fqdn","00-00-00-00-00-00,"""},{"room1-04","fqdn","00-00-00-00-00-00",""}]
 ],
 ["room2",
   [{".","","",""},{"room2-01","fqdn","00-00-00-00-00-00",""},{"room2-02","fqdn","00-00-00-00-00-00",""}]
   [{".","","",""},{".","","",""},{".","","",""},{".","","",""},{".","","",""},{".","","",""}],
   [{"room2-03","fqdn","00-00-00-00-00-00",""}],
   [{"room2-04","fqdn","00-00-00-00-00-00"},{".","","",""},{"room2-05","fqdn","00-00-00-00-00-00,""}]
 ],
]).
-define(APPS, ["","any.cmd","any.exe","any.msi","any.msp","any.reg","ecom.beam","NiniteOne.exe","ninite.cmd"]).
-define(COMS, ["","anycmd","listupfls","mkuploads","ninite","ninitecmd","ninitelog","wuinstall"]).
-define(FAVICON,"/path/to/ecsmcons/favicon.ico").
-define(CERTFILE,"/path/to/cert.pem").
-define(KEYFILE,"/path/to/key.pem").
-define(CONF,"/path/to/ecsmcons/ecsmcons.conf").
-define(UPLOADS,"/path/to/ecsmcons/uploads/").
-define(STATIC,"/path/to/ecsmcons/static").
