(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){

    $scope.pids = {};
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
    ws.onmessage = function(message){
        handle_message(message);
    };
    ws.onclose = function(){
        console.log('Websocket closed !');
    };

    function handle_message(msg){
        var json_data = JSON.parse(msg.data);
        if(json_data.hasOwnProperty('processes')){
            $('#pid').empty();
            if(json_data.pids){
            }
        } else if(json_data.hasOwnProperty('nodes')) {
            if(json_data.nodes.length == 0){
                alert('First add a node.');
                window.location.href = 'index.html'
            }
            $('#nodes').empty();
            for(var n in json_data.nodes){
                var node = json_data.nodes[n];
                $('#nodes').append('<option value='+node+' >'+node+'</option>');
            }
            if(json_data.nodes.length>0){
                get_pids(json_data.nodes[0]);
            }
        } else if(json_data.hasOwnProperty('pids')){
            $('#pids_table').empty();
            for(var n in json_data.pids){
                var npinfo = json_data.pids[n];
                $('#pids_table').append('<tr><td>'+
                    npinfo.pid+'</td><td>'+
                    npinfo.name+'</td><td>'+
                    npinfo.mq+'</td><td>'+
                    '<button>TODO</button></td></tr>'
                );
            }
        }
    }

    function get_pids(node){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'non_sys_processes',
                                'args':
                                    [node]
                               })
        );
    }

    $('#nodes').change(function(){
        get_pids($('#nodes').val());
    });

  });

})();


    // function github_search_link(init_call){
    //     return '<a onclick=\"new_github_tab(\''+init_call+'\')\">'+init_call+'</a>';
    // }

    // function new_github_tab(init_call){
    //     var module = module_from_init_call(init_call);
    //     var API_url = github_api_erlang_search_query(module);
    //     var url = github_module_url(API_url, module);
    //     var win = window.open(url, '_blank');
    //     if (win) {
    //         //Browser has allowed it to be opened
    //         win.focus();
    //     } else {
    //         //Browser has blocked it
    //         alert('Please allow popups for this website');
    //     }
    // }

    // function module_from_init_call(init_call){
    //     if(init_call.includes(':') && init_call.includes('/')){
    //         var tokens = init_call.split(":");
    //         return tokens[0];
    //     } else {
    //         return init_call;
    //     }
    // }

    // function github_api_erlang_search_query(module){
    //     // https://api.github.com/search/code?q=-module%28application%29.filename:application.erl+in:file+language:Erlang+repo:erlang/otp
    //     var g_url = 'https://api.github.com/search/code?q=';
    //     g_url += '?q=-module%28'+module+'%28.'
    //     g_url += 'filename:'+module+'.erl+in:file+language:Erlang+repo:erlang/otp'
    //     return g_url;
    // }

    // function github_module_url(API_url, module){
    //     var resp = '#';
    //     var full_mod = module+".erl";
    //     $.getJSON( API_url, {
    //     format: "json"
    //     })
    //     .done(function( data ) {
    //       $.each( data.items, function( i, item ) {
    //         if( item.name == full_mod ){
    //             resp = item.html_url;
    //             return resp;
    //         }
    //       });
    //     });
    // }