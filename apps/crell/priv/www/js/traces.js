(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){
    // first run
    $scope.traces = {};
    var all_traces = $http.get("/crell_traces");
    all_traces.success(function(data, status, headers, config) {

        $scope.traces = data;
    });
    all_traces.error(function(data, status, headers, config) {
        alert("traces rest call failed!");
    });

    $scope.trace_module = function(module){
        var response = $http.get("/crell_mod/trace/" + module);
        response.success(function(data, status, headers, config) {
            // here, do something with data..........
            // show the trace screen.......
        });
        response.error(function(data, status, headers, config) {
           alert("rest call failed!");
        });
    }
  });

})();