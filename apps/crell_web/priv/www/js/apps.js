(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){

    $scope.nodes = {};
    $scope.app_name = {}; 

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
            if(json_data.nodes.length == 0){
                alert('First add a node.');
                window.location.href = 'index.html'
            } else {
                $('#nodes').empty();
                for(var n in json_data.nodes){
                    var node = json_data.nodes[n];
                    $('#nodes').append('<option value='+node+' >'+node+'</option>');
                }
                // get the first Node's apps'
                if(json_data.nodes.length>0){
                    get_application(json_data.nodes[0]);
                }
            }
        } else if (json_data.hasOwnProperty('node_apps')) {
            $('#apps_table').empty();
            for(var n in json_data.node_apps){
                var napp = json_data.node_apps[n];
                $('#apps_table').append('<tr><td>'+
                    napp.name+'</td><td>'+
                    napp.erts_vsn+'</td><td>'+
                    napp.vsn+'</td><td>'+
                    // '<button onclick="get_app_env(\''+napp.name+'\')">Get App Env</button></td></tr>');

                    '<button onclick="get_app_env(\''+napp.name+'\')">Get App Env</button></td>'+
                    '<td id="'+napp.name+'">&nbsp;</td></tr>');
            }
        } else if (json_data.hasOwnProperty('app_envs')) {
            var a = json_data.app_name;
            alert(a);
            $('#'+a).empty();
            $('#'+a).append(json_data.app_envs);
        }
    }

    function get_application(node){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'remote_which_applications',
                                'args':
                                    [node]
                               })
        );
    };

    // function get_app_env(app_name){
    //     ws.send(JSON.stringify({'module':'crell_server',
    //                             'function':'calc_app_env',
    //                             'args':
    //                                 [$('#nodes').val(), app_name]
    //                            })
    //     );
    // }

    $('#nodes').change(function(){
        get_application($('#nodes').val());
    });

    $scope.get_app_env = function(app_name){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'calc_app_env',
                                'args':
                                    [$('#nodes').val(), app_name]
                               })
        );
    }

  });
})();

var app_name;

function get_app_env(app_name) {
    angular.element(document.getElementsByTagName('body')).scope().get_app_env(app_name);
}