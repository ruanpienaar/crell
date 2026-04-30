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
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'connecting_nodes',
                                'args':
                                    []
                               })
        );
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'clusters',
                                'args':
                                    []
                               })
        );
    };
    ws.onmessage = function(message) {
        handle_message(message);
    };
    ws.onclose = function(){
        console.log('Websocket closed !');
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
                $('#nodes_info_label').html('&nbsp;');
            }
        } else if(json_data.hasOwnProperty('connecting_nodes')) {
            $('#conn_nodes').empty();
            for(var n in json_data.connecting_nodes){
                var node = json_data.connecting_nodes[n];
                $('#conn_nodes').append('<option value='+node+' >'+node+'</option>');
            }
            if(json_data.connecting_nodes.length==0){
                $('#conn_nodes_info_label').html('No nodes Connecting.');
            } else {
                $('#conn_nodes_info_label').html('&nbsp;');
            }
        } else if(json_data.hasOwnProperty('node_connecting')) {
            //$('#loaderImg').attr("class", "visible");
            var node = json_data.node_connecting;
            if( $("#conn_nodes option[value='"+node+"']").length == 0 ){
                $('#conn_nodes').append('<option value='+node+' >'+node+'</option>');
            }
        } else if(json_data.hasOwnProperty('node_connected')) {
            var node = json_data.node_connected;
            $("#conn_nodes option[value='"+node+"']").remove();
            if( $("#nodes option[value='"+node+"']").length == 0 ){
                $('#nodes').append('<option value='+node+' >'+node+'</option>');
                $('#nodes_info_label').html('&nbsp;');
            }
        } else if(json_data.hasOwnProperty('node_disconnected')) {
            var node = json_data.node_disconnected;
            $("#nodes option[value='"+node+"']").remove();
            if( $('#nodes').has('option').length == 0 ) {
                $('#nodes_info_label').html('No nodes added. Add a node on this page.');
            }
            $('#conn_nodes').append('<option value='+node+' >'+node+'</option>');
        } else if(json_data.hasOwnProperty('clusters')) {
            $('#existing_cluster').empty();
            $('#existing_cluster').append('<option value="">-- select existing --</option>');
            for(var c in json_data.clusters){
                var cluster = json_data.clusters[c];
                $('#existing_cluster').append('<option value="'+cluster+'">'+cluster+'</option>');
            }
        } else if(json_data.hasOwnProperty('node_info')) {
            var info = json_data.node_info;
            $('#original_node').val(info.node);
            $('#node').val(info.node);
            $('#cookie').val(info.cookie);
            $('#new_cluster').val(info.cluster_name);
            $('#existing_cluster').val('');
            $('#node_form_heading').text('Edit node');
            $('#save_node').text('Save changes');
        } else if(json_data.hasOwnProperty('node_deleted')) {
            var node = json_data.node_deleted;
            $("#conn_nodes option[value='"+node+"']").remove();
            $("#nodes option[value='"+node+"']").remove();
            if( $('#nodes option').length == 0 ) {
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

    function reset_form(){
        $('#original_node').val('');
        $('#node').val('');
        $('#cookie').val('');
        $('#new_cluster').val('');
        $('#existing_cluster').val('');
        $('#node_form_heading').text('Add a node');
        $('#save_node').text('Save');
    }

    function save_node(){
        var cluster_name = $('#new_cluster').val() || $('#existing_cluster').val() || '';
        var original = $('#original_node').val();
        if (original) {
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'edit_node',
                                    'args':
                                        [original,
                                         $('#node').val(),
                                         $('#cookie').val(),
                                         cluster_name]
                                   })
            );
        } else {
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'add_node',
                                    'args':
                                        [$('#node').val(),
                                         $('#cookie').val(),
                                         cluster_name]
                                   })
            );
        }
        reset_form();
    }

    $('#del_node').click(function(){
        if ( confirm("Remove node "+$('#nodes').val()+" ?") ){
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'del_node',
                                    'args':
                                        // not a array, since its a multiple select
                                        $('#nodes').val()
                                   })
            );
        }
    });

    $('#edit_node').click(function(){
        var selected = $('#nodes').val();
        if (!selected || selected.length === 0) { alert('Select a node to edit.'); return; }
        var node = Array.isArray(selected) ? selected[0] : selected;
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'get_node',
                                'args': [node]
                               })
        );
    });

    $('#conn_del_node').click(function(){
        if ( confirm("Remove node "+$('#conn_nodes').val()+" ?") ){
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'conn_del_node',
                                    'args':
                                        // not a array, since its a multiple select
                                        $('#conn_nodes').val()
                                   })
            );
        }
    });

    $('#conn_edit_node').click(function(){
        var selected = $('#conn_nodes').val();
        if (!selected || selected.length === 0) { alert('Select a node to edit.'); return; }
        var node = Array.isArray(selected) ? selected[0] : selected;
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'get_node',
                                'args': [node]
                               })
        );
    });

    $('#disc_neigh_nodes').click(function(){
        if( $('#nodes').val() != null ){
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'discover_neighbour_nodes',
                                    'args':
                                        // not a array, since its a multiple select
                                        $('#nodes').val()
                                   })
            );
        } else {
            alert('Select a existing node.');
        }
    });

  });
})();

$(document).keypress(function(e) {
    if(e.which == 13) {
        //alert('You pressed enter!');
        angular.element(document.getElementsByTagName('body')).scope().save_node();
    }
});