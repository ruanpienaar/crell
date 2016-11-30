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
    };
    ws.onmessage = function(message) {
        handle_message(message);
    };
    ws.onclose = function(){
        alert('closed');
    };

    function handle_message(msg) {
        var json_data = JSON.parse(msg.data);
        if(json_data.hasOwnProperty('nodes')) {
            $('#nodes').empty();
            for(var n in json_data.nodes){
                var node = json_data.nodes[n];
                $('#nodes').append('<option value='+node+' >'+node+'</option>');
            }
            if(json_data.nodes.length>0){
                get_applications(json_data.nodes[0]);
            }else{
                  alert('no nodes added');
            }
        } else if (json_data.hasOwnProperty('node_apps')) {
            if ( $( "#apps_table" ).length ) {
                application_table(json_data.node_apps);
            } else if( $( "#apps_select" ).length ) {
                application_select(json_data.node_apps);
            }
        }
    }

    function get_applications(node){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'remote_which_applications',
                                'args':
                                    [node]
                               })
        );
    };

    function application_table(apps){
        $('#apps_table').empty();
        for(var n in apps){
            var napp = json_data.node_apps[n];
            $('#apps_table').append('<tr><td>'+
                napp.name+'</td><td>'+
                napp.erts_vsn+'</td><td>'+
                napp.vsn+'</td></tr>');
        }
    }

    function application_select(apps){
        $('#apps_select').empty();
        for(var n in apps){
            var napp = apps[n];
            $('#apps_select').append('<option value='+napp.name+'>'+napp.name+'</option>');
        }
    }

    $('#nodes').change(function(){
        get_applications($('#nodes').val());
    });

  });
})();