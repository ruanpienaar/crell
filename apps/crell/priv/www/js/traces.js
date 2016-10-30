(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){
    // first run
    // $scope.traces = {};
    // var all_traces = $http.get("/crell_traces");
    // all_traces.success(function(data, status, headers, config) {

    //     $scope.traces = data;
    // });
    // all_traces.error(function(data, status, headers, config) {
    //     alert("traces rest call failed!");
    // });

    // active nodes
    $scope.active_trace_nodes = {};
    var active_trc_node = $http.get("/goanna_api/nodes");
    active_trc_node.success(function(data, status, headers, config){
        $scope.active_trace_nodes = data;
    });
    active_trc_node.error(function(data, status, headers, config){
        console.log("traces.js /goanna_api/nodes error "+status);
    });

    // active traces
    $scope.active_traces = {};
    var active_trc_http = $http.get("/goanna_api/list_active_traces");
    active_trc_http.success(function(data, status, headers, config){
        $scope.active_traces = data;
    });
    active_trc_http.error(function(data, status, headers, config){
        console.log("traces.js /goanna_api/list_active_traces error "+status);
    });


    // $scope.trace_module = function(module){
    //     var response = $http.get("/crell_mod/trace/" + module);
    //     response.success(function(data, status, headers, config) {
    //         // here, do something with data..........
    //         // show the trace screen.......
    //     });
    //     response.error(function(data, status, headers, config) {
    //        alert("rest call failed!");
    //     });
    // }
  });

})();