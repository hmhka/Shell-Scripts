define variable l_connect-name   like _connect-name no-undo.
define variable l_connect-usr    like _connect-usr no-undo.
define variable l_connect-pid    like _connect-pid no-undo.
define variable l_connect-device like _connect-device no-undo.
define variable l_answer         as logical no-undo.

assign l_connect-device = entry(1,session:parameter)
       l_connect-pid    = integer(entry(2,session:parameter))
       l_connect-name   = entry(3,session:parameter)
       l_connect-usr    = integer(entry(4,session:parameter)).

define stream a.
output stream a to value(os-getenv("tmpfile2")) append.

for each _connect where _connect-usr <> ?
                    and _connect-name <> "progress"
                    and _connect-name = (if l_connect-name = "" then
                                            _connect-name
                                         else
                                            l_connect-name)
                    and _connect-usr  = (if l_connect-usr = 0 then
                                            _connect-usr
                                         else
                                            l_connect-usr)
                    and _connect-pid = (if l_connect-pid = 0 then
                                           _connect-pid
                                        else
                                           l_connect-pid)
                    and _connect-device = (if l_connect-device = "" then
                                           _connect-device
                                        else
                                           l_connect-device)
                    no-lock:
    display _connect-usr _connect-name format "x(12)" _connect-pid 
            _connect-device.
    put stream a unformatted pdbname(1) " " _connect-usr skip.
end.

output stream a close.
quit.
