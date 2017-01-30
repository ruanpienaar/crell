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
        alert('closed');
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
                    npinfo.mq+'</td></tr>');
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

    // var all_procs = $http.get("/crell_proc/pids/");
    // all_procs.success(function(data, status, headers, config) {
    //     $scope.pid_data = data;
    // });

    // all_procs.error(function(data, status, headers, config) {
    //     alert("pid_data rest call failed!");
    // });

    // $scope.trace_module = function(module){
    //     // var response = $http.get("/crell_mod/trace/" + module);
    //     // response.success(function(data, status, headers, config) {
    //     //     // here, do something with data..........
    //     //     // show the trace screen.......
    //     // });
    //     // response.error(function(data, status, headers, config) {
    //     //    alert("rest call failed!");
    //     // });
    // }

  });

})();


// {name: "init", children: [
        // {name: "erl_prim_loader", children: [
                //{name: "port 0", children: []}]}]}


// {"name":"user","children":[{"name":"user_drv","children":[
        /// {"name":"<0.32.0>","children":[]},
        /// {"name":"port 2944","children":[]}]}]}

// {"name":"<0.39.0>","children":[
        // {"name":"<0.33.0>","children":[
            // {"name":"<0.65.0>","children":[]}]}]}