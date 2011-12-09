{application, ecsmcons,
 [{description, "Erlang Computer Systems Management CONSole"},
  {vsn, "1.0"},
  {modules, [ecsmcons, ecom]},
  {registered, [ecsmcons]},
  {applications, [kernel, stdlib]},
  {mod, {ecsmcons, [9090]}}
 ]}.