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
            if(json_data.nodes.length==0){
                $('#nodes_info_label').html('No nodes added. Add a node on this page.');
            } else {
                $('#nodes_info_label').html('');
            }
        } else if(json_data.hasOwnProperty('node_connecting')) {
            $('#loaderImg').attr("class", "visible");
        } else if(json_data.hasOwnProperty('node_connected')) {
            $('#loaderImg').attr("class", "invisible");
            var node = json_data.node_connected;
            $('#nodes').append('<option value='+node+' >'+node+'</option>');
            $('#nodes_info_label').html('');
        } else if(json_data.hasOwnProperty('node_disconnected')) {
            var node = json_data.node_disconnected;
            $("#nodes option[value='"+node+"']").remove();
            if( $('#nodes').has('option').length == 0 ) {
                $('#nodes_info_label').html('No nodes added. Add a node on this page.');
            }
        }
    }

    $('#save_node').click(function(){
        save_node();
    });

    $scope.save_node = function(){
        save_node();
    }

    function save_node(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'add_node',
                                'args':
                                    [$('#node').val(),
                                     $('#cookie').val()]
                               })
        );
        $('#node').val("");
        $('#cookie').val("");
    }

    $('#del_node').click(function(){
        if ( confirm("Remove node "+$('#nodes').val()+" ?") ){
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'del_node',
                                    'args':
                                        [$('#nodes').val()]
                                   })
            );
        }
    });

    $('#edit_node').click(function(){
        alert('Not implemented yet.');
    });

  });
})();

$(document).keypress(function(e) {
    if(e.which == 13) {
        //alert('You pressed enter!');
        angular.element(document.getElementsByTagName('body')).scope().save_node();
    }
});