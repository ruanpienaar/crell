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
        console.log('Websocket closed !');
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
                // get the first Node's apps'
                if(json_data.nodes.length>0){
                    get_mods(json_data.nodes[0]);
                }
            }
        } else if(json_data.hasOwnProperty('mods')) {
            $('#mods_table').empty();
            for(var n in json_data.mods){
                $('#mods_table').append('<tr>'+
                    '<td><button onclick="get_mod_code(\''+n+'\')">Code</button></td>'+
                    '<td>'+n+'</td>'+
                    '<td>'+json_data.mods[n]+'</td></tr>');
            }
        } else if(json_data.hasOwnProperty('code')){
            var code = json_data.code
            $('#module_source_code').empty();
            $('#module_source_code').append(code);
        }
    }

    function get_mods(node){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'runtime_modules',
                                'args':
                                    [node]
                               })
        );
    };

    $scope.get_mod_code = function(mod) {
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'module_source',
                                'args':
                                    [$('#nodes').val(), mod]
                              })
        );
    }

    $('#nodes').change(function(){
        get_mods($('#nodes').val());
    });

    $('#clear_code').click(function(){
        console.log('clear code');
        clear_code();
    });

    function clear_code() {
        $('#module_source_code').empty();
    }

  });

})();

var module;
function get_mod_code(module) {
    angular.element(document.getElementsByTagName('body')).scope().get_mod_code(module);
}