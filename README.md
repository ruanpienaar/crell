Crawl Erlang Source Code
===

1) clone repo

2) make rel

3) edit rel/crell/releases/1/sys.config
Under the crell application:
- enter a remote node
- enter the cookie for the remote node.
( The only currently implemented feature is remote loading the 
appmon code, option "1" in the sys.config )
- choose port that's open on your firewall under "http_port"
 ( 8080 is normally ok )

4) rel/crell/bin/crell console

5) Visit
http://{MACHINE-IP}:8080/ 
OR 
http://localhost:8080/

Create graphs of Erlang callgraph using xref.
===

./extract.sh $DIR
* $DIR containing (src/*.erl) folders.

Future work
===

- extending the crawling with either etdd and/or RefactorErl, or some smart tool.
