(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){

    $scope.nodes = {};
    var url = window.location.href;
    var arr = url.split("/");
    var ws_url = "ws://"+arr[2]+"/crell/ws";
    var ws = new WebSocket(ws_url);

    ws.onopen = function(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'nodes',
                                'args':
                                    []
                               })
        );
    }

    ws.onmessage = function(message){
        handle_message(message);
    }

    ws.onclose = function(){
        alert('closed');
    }

    function handle_message(msg){
        var json_data = JSON.parse(msg.data);

        if(json_data.hasOwnProperty('nodes')) {
            if(json_data.nodes.length == 0){
                alert('First add a node.');
                window.location.href = 'index.html'
            } else {
                $('#nodes').empty();
                for(var n in json_data.nodes){
                    var node = json_data.nodes[n];
                    $('#nodes').append('<option value='+node+' >'+node+'</option>');
                }
                // get the first Node's db tables'
                if(json_data.nodes.length>0){
                    get_db_tables(json_data.nodes[0]);
                }
            }
        } else if(json_data.hasOwnProperty('db_tables')) {
            // handle ets
            $('#ets_db_tables').empty();
            var db_tables = json_data.db_tables;
            for(var e in db_tables.ets_tables){
                etbl = db_tables.ets_tables[e];
                $('#ets_db_tables').append('<tr><td>'+etbl+'</td></tr>');
            }

            // handle mnesia, if mnesia is started.
            $('#mnesia_db_tables').empty();
            if(db_tables.hasOwnProperty('mnesia_tables')){
                //alert(' has mnesia tables');
                var db_tables = json_data.db_tables;
                for(var m in db_tables.mnesia_tables){
                    mtbl = db_tables.mnesia_tables[m];
                    $('#mnesia_db_tables').append('<tr><td>'+mtbl+'</td></tr>');
                }
            } else {
                $('#mnesia_db_tables').append('<tr><td>N/A</td></tr>');
            }
        }
    }

    function get_db_tables(node){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'get_db_tables',
                                'args':
                                    [node]
                               })
        );
    }

    $('#get_db_tables').click(function(){
        get_db_tables($('#nodes').val());
    });

  });
})();