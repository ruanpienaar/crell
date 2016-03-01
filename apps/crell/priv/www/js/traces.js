
(function(){
  var app = angular.module('CrellApp', []);
  app.controller('CrellController', function($scope, $http){
    // first run
    $scope.mods = {};
    var all_mods = $http.get("/crell_traces/all");
    all_mods.success(function(data, status, headers, config) {
        $scope.mods = data;
    });
    all_mods.error(function(data, status, headers, config) {
        alert("rest call failed!");
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